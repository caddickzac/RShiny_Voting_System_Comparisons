# app.R — Simulated Voting System Outcome Comparisons
# Includes: RCV with Borda-like tiebreaker, stacked-round visual,
# approval “Didn’t vote” handling, 50% dashed line + label,
# voter/results tables, and top-aligned plots.

library(shiny)
library(tidyverse)
library(gridExtra)
library(ggforce)
library(cowplot)
library(DT)

# ---------------- helpers ----------------

rand_dimension_voters     <- function(n) runif(n, -100, 100)
rand_dimension_candidates <- function(n) runif(n,  -98,   98)

candidate_palette <- c(
  "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00",
           "#FFFF33", "#A65628", "#F781BF", "#999999", "#66C2A5",
           "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F",
           "#E5C494", "#B3B3B3", "#1B9E77", "#D95F02", "#7570B3"
)

# helpers for pretty labels
mk_voters_label     <- function(mx) tagList("Number of voters: ",     tags$span(sprintf("(max=%d)", mx), style="font-weight:normal"))
mk_candidates_label <- function(mx) tagList("Number of candidates: ", tags$span(sprintf("(max=%d)", mx), style="font-weight:normal"))


# ---- political compass pole labels (for maps) ----
add_pole_labels <- function(p,
                            x_min = -100, x_max = 100,
                            y_min = -100, y_max = 100,
                            off_bottom = 8,  # how far below x-axis (data units)
                            off_left   = 15  # how far left of y-axis (data units)
) {
  p +
    # x-axis poles (just below the x axis)
    annotate("text", x = x_min, y = y_min - off_bottom,
             label = "Left",  hjust = 0, vjust = 1, size = 3.6, fontface = 2, color = "grey25") +
    annotate("text", x = x_max, y = y_min - off_bottom,
             label = "Right", hjust = 1, vjust = 1, size = 3.6, fontface = 2, color = "grey25") +
    # y-axis poles (to the left of the y axis, vertical)
    annotate("text", x = x_min - off_left, y = y_max,
             label = "Authoritarian", angle = 90, hjust = 1, vjust = 1,
             size = 3.6, fontface = 2, color = "grey25") +
    annotate("text", x = x_min - off_left, y = y_min,
             label = "Libertarian",   angle = 90, hjust = 0, vjust = 0,
             size = 3.6, fontface = 2, color = "grey25")
}


max_dist <- sqrt(200^2 + 200^2)  # 282.8427
rank_by_distance <- function(D) t(apply(D, 1, order))

integer_breaks <- function(n = 6) {
  function(lims) {
    br <- pretty(c(floor(lims[1]), ceiling(lims[2])), n = n)
    unique(as.integer(round(br)))
  }
}

# centered winner/tie text
add_winner_text_center <- function(p, winners, x_levels, max_y) {
  if (length(winners) == 0) return(p)
  msg <- if (length(winners) == 1) paste0(winners, " Wins!")
  else paste(paste(winners, collapse = " and "), "Tie!")
  x_center <- (length(x_levels) + 1) / 2
  p + annotate("text", x = x_center, y = max_y * 0.98,
               label = msg, fontface = 2, color = "black")
}

# Discrete-x plots (plurality, approval)
add_half_line_discrete <- function(p, y50, ..., lbl = "50%",
                                   pad_frac = 0.01, pad_min = 0.3) {
  ymax <- y50 * 2
  pad  <- max(pad_frac * ymax, pad_min)   # how far above the line
  
  p +
    geom_hline(yintercept = y50, linetype = "dashed", color = "grey55", alpha = .7) +
    annotate(
      "text",
      x = Inf, y = y50 + pad,              # just ABOVE the line
      label = lbl, color = "grey30", alpha = .7,
      angle = 90,                          # vertical
      hjust = 0,                           # hug inside right edge
      vjust = -.5                            # bottom of text at y position
    )
}

# Continuous-x plots (RCV)
add_half_line_cont <- function(p, y50, ..., lbl = "50%",
                               pad_frac = 0.01, pad_min = 0.3) {
  ymax <- y50 * 2
  pad  <- max(pad_frac * ymax, pad_min)
  
  p +
    geom_hline(yintercept = y50, linetype = "dashed", color = "grey55", alpha = .7) +
    annotate(
      "text",
      x = Inf, y = y50 + pad,
      label = lbl, color = "grey30", alpha = .7,
      angle = 90,
      hjust = 0,
      vjust = -.5
    )
}

# Put every bar label at the same low baseline (uniform placement)
add_bar_value_labels <- function(p, data, x_col, y_col, max_y, digits = 0,
                                 baseline_frac = 0.02, baseline_min = 0.1) {
  df <- data
  baseline <- max(baseline_frac * max_y, baseline_min)
  df$label_y <- baseline
  vals <- df[[y_col]]
  df$lbl <- if (digits == 0) format(round(vals), big.mark = ",")
  else format(round(vals, digits), nsmall = digits)
  
  p + geom_text(
    data = df,
    aes_string(x = x_col, y = "label_y", label = "lbl"),
    color = "black", fontface = 2, size = 3.6,
    inherit.aes = FALSE
  )
}

top_choice_given_active <- function(rank_mat, active_mask) {
  apply(rank_mat, 1, function(row) {
    for (j in seq_along(row)) { id <- row[j]; if (active_mask[id]) return(id) }
    NA_integer_
  })
}

# -------------- RCV with Borda-like tiebreaker, then alphabetical --------------

rcv_irv <- function(rank_mat) {
  K <- ncol(rank_mat); N <- nrow(rank_mat)
  active <- rep(TRUE, K); majority <- floor(N/2) + 1
  rounds <- list(); eliminated <- integer()
  
  # Borda-like scores once (sum of K - position), lower = less overall support
  pos_matrix <- matrix(0L, nrow = N, ncol = K)
  for (i in seq_len(N)) pos_matrix[i, rank_mat[i, ]] <- seq_len(K)
  borda_scores <- colSums(K - pos_matrix)
  
  top_active <- function(act = active) {
    apply(rank_mat, 1, function(row) {
      for (j in seq_len(K)) { id <- row[j]; if (act[id]) return(id) }
      NA_integer_
    })
  }
  
  r <- 1
  repeat {
    top <- top_active(active)
    counts <- tabulate(top, nbins = K); counts[!active] <- 0
    
    # final two-candidate exact tie => declare tie
    if (sum(active) == 2L) {
      idx <- which(active)
      if (counts[idx[1]] == counts[idx[2]]) {
        rounds[[r]] <- list(counts = counts, active = active,
                            eliminated = if (r == 1) NA_integer_ else eliminated[r - 1])
        return(list(winner_index = NA_integer_, tie_indices = idx, rounds = rounds))
      }
    }
    
    rounds[[r]] <- list(counts = counts, active = active,
                        eliminated = if (r == 1) NA_integer_ else eliminated[r - 1])
    
    # winner by majority or only one left
    if (max(counts) >= majority || sum(active) == 1L) {
      return(list(winner_index = which.max(counts), tie_indices = integer(0), rounds = rounds))
    }
    
    # eliminate lowest tally; tie-break: lowest Borda-like, then alphabetical
    min_count <- min(counts[active])
    cand_min <- which(counts == min_count & active)
    
    elim <- if (length(cand_min) == 1) {
      cand_min
    } else {
      tied_borda <- borda_scores[cand_min]
      worst <- cand_min[tied_borda == min(tied_borda)]
      if (length(worst) > 1) min(worst) else worst
    }
    
    active[elim] <- FALSE
    eliminated[r] <- elim
    r <- r + 1
  }
}

# ---------------- UI ----------------

ui <- fluidPage(
  tags$head(tags$style(HTML("
  /* Reduce padding above the main panel */
  .main-panel { padding-top: 0 !important; }

  /* Pull the plot a bit upward to match the sidebar's top */
  .main-panel .shiny-plot-output { margin-top: -10px; }

  /* Trim ggplot's outer whitespace so the figure top hugs the container */
  .main-panel .plot-container { padding-top: 0 !important; }
  .dataTables_wrapper .dataTables_scrollHead table,
  .dataTables_wrapper .dataTables_scrollBody table {
    width: 100% !important;
  }
"))),
  titlePanel("Simulated Voting System Outcome Comparisons"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "example_type", "Example Type",
        choices = c("1-dimension", "2-dimension"),
        selected = "1-dimension"
      ),
      numericInput("total_voters", HTML("Number of voters: <span style='font-weight:normal'>(max=500)</span>"),
                   value = 30, min = 1, max = 500),
      numericInput("candidate_count", HTML("Number of candidates: <span style='font-weight:normal'>(max=20)</span>"),
                   value = 3, min = 2, max = 20),
      selectInput("voting_system", "See full results:",
                  c("Plurality"="plurality","Ranked-Choice"="ranked_choice",
                    "Approval"="approval","Cardinal (Score)"="score")),
      conditionalPanel("input.voting_system == 'approval'",
                       sliderInput("approval_thresh","Approval distance threshold", min=5,max=150,value=50,step=5)
      ),
      actionButton("randomize", "Randomize Data"),
      checkboxInput("show_voter_data", "Show Voter Data", value = FALSE),
      checkboxInput("show_results_table", "Show results table", value = FALSE),
      conditionalPanel("input.voting_system == 'ranked_choice'",
                       hr(),
                       tags$div("View RCV rounds", style="font-weight:bold;margin-bottom:.25rem;"),
                       tags$div(style="display:flex;gap:.5rem;align-items:center;",
                                actionButton("prev_round","◀"),
                                actionButton("next_round","▶"),
                                tags$div(textOutput("rcv_round_label"), style="margin-left:.5rem;font-weight:bold;")
                       )
      )
    ),
    mainPanel(class = "main-panel", style = "padding-top:0;margin-top:0;",
              plotOutput("plotgraph", width="100%", height="560px"),
              conditionalPanel("input.show_voter_data",
                               tags$hr(),
                               h4("Voter Data"),
                               DTOutput("voter_table")
              ),
              conditionalPanel("input.show_results_table",
                               tags$hr(),
                               h4("Results by Voting System"),
                               DTOutput("results_dt", width = "100%")
              )
    )
  )
)

# ---------------- Server ----------------

server <- function(input, output, session) {
  
  # ---- dynamic caps (depend on Example Type) ----
  voters_max     <- reactive(if (identical(input$example_type, "1-dimension")) 50 else 500)
  candidates_max <- reactive(if (identical(input$example_type, "1-dimension")) 8  else 20)
  
  # keep UI labels/max in sync and clamp current values
  observe({
    vm <- voters_max(); cm <- candidates_max()
    tv <- if (is.null(input$total_voters)) 30 else input$total_voters
    cc <- if (is.null(input$candidate_count)) 3 else input$candidate_count
    
    updateNumericInput(
      session, "total_voters",
      max   = vm,
      value = min(max(tv, 1), vm),
      label = mk_voters_label(vm)         # pretty label (no raw HTML shows)
    )
    updateNumericInput(
      session, "candidate_count",
      max   = cm,
      value = min(max(cc, 2), cm),
      label = mk_candidates_label(cm)
    )
  })
  
  
  
  # clamp inputs
  # ---- dynamic caps (depend on Example Type) ----
  voters_max     <- reactive({ if (isTruthy(input$example_type) && input$example_type == "1-dimension") 50 else 500 })
  candidates_max <- reactive({ if (isTruthy(input$example_type) && input$example_type == "1-dimension") 8  else 20  })
  
  # Keep UI labels/max in sync and clamp current values when type or values change
  observe({
    vm <- voters_max(); cm <- candidates_max()
    tv <- if (is.null(input$total_voters)) 30 else input$total_voters
    cc <- if (is.null(input$candidate_count)) 3 else input$candidate_count
    
    updateNumericInput(
      session, "total_voters",
      max   = vm,
      value = min(max(tv, 1), vm),
      label = HTML(sprintf("Number of voters: <span style='font-weight:normal'>(max=%d)</span>", vm))
    )
    updateNumericInput(
      session, "candidate_count",
      max   = cm,
      value = min(max(cc, 2), cm),
      label = HTML(sprintf("Number of candidates: <span style='font-weight:normal'>(max=%d)</span>", cm))
    )
  })
  
  # clamp inputs (also reacts to manual edits)
  observeEvent(input$total_voters, {
    vm <- voters_max()
    v  <- input$total_voters
    if (v > vm) updateNumericInput(session, "total_voters", value = vm)
    if (v < 1)  updateNumericInput(session, "total_voters", value = 1)
  }, ignoreInit = TRUE)
  
  observeEvent(input$candidate_count, {
    cm <- candidates_max()
    v  <- input$candidate_count
    if (v > cm) updateNumericInput(session, "candidate_count", value = cm)
    if (v < 2)  updateNumericInput(session, "candidate_count", value = 2)
  }, ignoreInit = TRUE)
  
  # ---- downstream reactives (unchanged) ----
  voterData <- eventReactive(list(input$randomize, input$total_voters, input$example_type), {
    if (identical(input$example_type, "1-dimension")) {
      tibble(x = rand_dimension_voters(input$total_voters),
             y = rep(0, input$total_voters))
    } else {
      tibble(x = rand_dimension_voters(input$total_voters),
             y = rand_dimension_voters(input$total_voters))
    }
  }, ignoreInit = FALSE)
  
  candidate_ids <- reactive(LETTERS[seq_len(input$candidate_count)])
  
  candidateData <- eventReactive(list(input$randomize, input$candidate_count, input$example_type), {
    if (identical(input$example_type, "1-dimension")) {
      tibble(x = rand_dimension_candidates(input$candidate_count),
             y = rep(0, input$candidate_count),
             id = candidate_ids())
    } else {
      tibble(x = rand_dimension_candidates(input$candidate_count),
             y = rand_dimension_candidates(input$candidate_count),
             id = candidate_ids())
    }
  }, ignoreInit = FALSE)
  
  
  dist_matrix <- reactive({
    V <- voterData(); C <- candidateData()
    sqrt(outer(V$x, C$x, `-`)^2 + outer(V$y, C$y, `-`)^2)
  })
  
  rank_matrix      <- reactive(rank_by_distance(dist_matrix()))
  first_choice_idx <- reactive(rank_matrix()[,1])
  pref1            <- reactive(candidateData()$id[first_choice_idx()])
  
  plurality_summary <- reactive({
    tibble(candidate = pref1()) |>
      count(candidate, name = "Votes") |>
      complete(candidate = candidate_ids(), fill = list(Votes = 0)) |>
      arrange(desc(Votes))
  })
  
  score_table <- reactive({
    C <- candidateData(); md <- colMeans(dist_matrix())
    tibble(candidate = C$id, mean_distance = md) |> arrange(mean_distance)
  })
  
  approval_summary <- reactive({
    C <- candidateData(); D <- dist_matrix(); thr <- input$approval_thresh
    approvals <- colSums(D <= thr); didnt <- sum(rowSums(D <= thr) == 0)
    tibble(candidate = c(C$id, "Didn't vote"),
           value = c(approvals, didnt)) |> arrange(desc(value))
  })
  
  rcv_out <- reactive({
    rm <- rank_matrix()
    rcv_irv(rm)
  })
  
  rcv_round <- reactiveVal(1)
  observeEvent(list(input$randomize, input$total_voters, input$candidate_count), { rcv_round(1) }, ignoreInit = FALSE)
  total_rounds <- reactive(length(rcv_out()$rounds))
  observeEvent(input$next_round, { rcv_round(min(rcv_round() + 1, total_rounds())) })
  observeEvent(input$prev_round, { rcv_round(max(rcv_round() - 1, 1)) })
  output$rcv_round_label <- renderText({
    paste0("Round ", rcv_round(), " of ", length(rcv_out()$rounds))
  })
  
  # ---------- maps ----------
  # ----- 1-D "Political leaning" strip -----
  map_1d <- reactive({
    V <- voterData(); C <- candidateData()
    pal <- setNames(candidate_palette[seq_len(nrow(C))], C$id)
    
    # voter x and their nearest candidate (used for colours)
    voters_col <- tibble(
      x       = V$x,
      nearest = factor(pref1(), levels = C$id)
    )
    
    # how far to extend the panel beyond [-100, 100]
    x_left  <- -130
    x_right <-  105
    y_top   <-  10
    y_bottom<- -10
    
    ggplot() +
      # (optional) top/bottom frame lines to emphasize the box
      annotate("segment", x = x_left, xend = x_right, y =  y_top, yend =  y_top, colour = "black", linewidth = 0.8) +
      annotate("segment", x = x_left, xend = x_right, y = y_bottom, yend = y_bottom, colour = "black", linewidth = 0.8) +
      
      # --- left-side key/labels -------------------------------------------------
    annotate("text", x = x_left + 3, y =  2.2, label = "atop(bold(Candidates))",
             hjust = 0, fontface = 2, colour = "#59A4F0") +
      annotate("text", x = x_left + 3, y = -1.8, label = "Voter",
               hjust = 0, fontface = 2, colour = "black") +
      annotate("text", x = x_left + 3, y = -3.6, label = "Preferences",
               hjust = 0, fontface = 2, colour = "black") +
      annotate("segment", x = x_left + 3, xend = -100, y = 0, yend = 0, colour = "grey55") +
      
      # --- main axis ------------------------------------------------------------
    annotate("segment", x = -100, xend = 100, y = 0, yend = 0, colour = "grey60", linewidth = 2) +
      annotate("segment", x = -100, xend = -100, y = -3, yend = 3, colour = "grey60", linewidth = 2) +
      annotate("segment", x =  100, xend =  100, y = -3, yend = 3, colour = "grey60", linewidth = 2) +
      
      # voters (ticks below the axis), coloured by nearest candidate
      geom_segment(
        data = voters_col, aes(x = x, xend = x, colour = nearest),
        y = -2, yend = -0.1, linewidth = 1.1, inherit.aes = FALSE
      ) +
      
      # candidates (ticks above the axis) in fixed blue, labels in palette colours
      geom_segment(
        data = C,
        aes(x = x, xend = x), y = 0.1, yend = 2,
        colour = "#59A4F0", linewidth = 1.1, inherit.aes = FALSE
      ) +
      geom_text(
        data = C, aes(x = x, y = 3, label = id, colour = id),
        fontface = 2, size = 5, inherit.aes = FALSE
      ) +
      
      # numbers & words under the axis
      annotate("text", x = -100, y = -4, label = "-100", fontface = 2, colour = "grey40", size = 3, hjust = 0) +
      annotate("text", x =    0, y = -4, label =   "0", fontface = 2, colour = "grey40", size = 3, hjust = .5) +
      annotate("text", x =  100, y = -4, label =  "100", fontface = 2, colour = "grey40", size = 3, hjust = 1) +
      annotate("text", x = -100, y = -5, label = "Liberal",      colour = "grey30", size = 3.2, fontface = 2, hjust = 0) +
      annotate("text", x =    0, y = -5, label = "Moderate",     colour = "grey30", size = 3.2, fontface = 2, hjust = .5) +
      annotate("text", x =  100, y = -5, label = "Conservative", colour = "grey30", size = 3.2, fontface = 2, hjust = 1) +
      annotate("text", x =    0, y = -7, label = "Political Leaning",
               colour = "black", size = 4.0, fontface = 2, hjust = .5) +
      
      # colouring for voters and candidate labels
      scale_colour_manual(values = pal, guide = "none") +
      
      # IMPORTANT: expand panel so left labels are inside the plotting area
      coord_cartesian(xlim = c(x_left, x_right), ylim = c(-7.5, 5.5), clip = "off") +
      
      theme_bw() +
      theme(
        legend.position = "none",
        axis.title = element_blank(),
        axis.text  = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.margin = margin(t = 5, r = 15, b = 28, l = 5)
      ) +
      labs(title = "Political Leaning (1-D)")
  })
  
  
  
  
  map_plot_generic <- function(current_choice_ids, active_mask = NULL, rcv_mode = FALSE) {
    V <- voterData(); C <- candidateData()
    pal <- setNames(candidate_palette[seq_len(nrow(C))], C$id)
    df_c <- C; df_c$alpha <- 1
    if (rcv_mode && !is.null(active_mask)) df_c$alpha <- ifelse(active_mask, 1, 0.25)
    
    x_breaks <- c(-100, -50, 0, 50, 100)
    x_labels <- c("-100", "-50", "0", "50", "")   # hide "100"; we’ll draw it ourselves
    y_min    <- -100
    off      <- 6
    
    g <- ggplot() +
      geom_point(data = V, aes(x=x, y=y, color=factor(current_choice_ids)), size=1.8, alpha=0.9) +
      geom_text(data = df_c, aes(x=x, y=y, label=id, color=id, alpha=alpha),
                fontface=2, size=6, show.legend=FALSE)
    
    if (rcv_mode && !is.null(active_mask)) {
      elim_df <- df_c[!active_mask, , drop=FALSE]
      if (nrow(elim_df)) {
        len <- 5
        g <- g +
          geom_segment(data=elim_df, aes(x=x-len, xend=x+len, y=y-len, yend=y+len),
                       color="red2", linewidth=1.0, inherit.aes=FALSE) +
          geom_segment(data=elim_df, aes(x=x-len, xend=x+len, y=y+len, yend=y-len),
                       color="red2", linewidth=1.0, inherit.aes=FALSE) +
          geom_text(data=elim_df, aes(x=x, y=y + len + 3, label="eliminated"),
                    color="black", size=3.5, fontface=2, inherit.aes=FALSE)
      }
    }
    
    p <- g +
      scale_color_manual(values = pal, guide="none") +
      scale_alpha_identity() +
      coord_fixed(xlim=c(-100,100), ylim=c(-100,100), expand=FALSE, clip = "off") +
      scale_x_continuous(breaks = x_breaks, labels = x_labels,
                         expand = expansion(mult=c(0.02,0.02))) +
      scale_y_continuous(expand = expansion(mult=c(0.02,0.02))) +
      theme_bw() +
      theme(panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            aspect.ratio=1,
            # give room for the outside pole labels
            plot.margin = margin(t = 5, r = 10, b = 28, l = 32)) +
      labs(title = "Voter & Candidate Positions",
           x="Economic Scale", y="Social Scale") +
      # custom right-aligned “100”
      annotate("text", x = 100, y = y_min - off, label = "100",
               hjust = 1, vjust = -.5, size = 3)
    
    # add “Left/Right” and vertical “Authoritarian/Libertarian”
    p <- add_pole_labels(p)
    p
  }
  map_default  <- reactive({ map_plot_generic(pref1()) })
  map_rcv      <- reactive({
    out <- rcv_out(); r <- rcv_round(); snap <- out$rounds[[r]]
    C <- candidateData(); top_ids <- C$id[ top_choice_given_active(rank_matrix(), snap$active) ]
    map_plot_generic(top_ids, active_mask = snap$active, rcv_mode = TRUE)
  })
  map_approval <- reactive({
    V <- voterData(); C <- candidateData(); D <- dist_matrix(); thr <- input$approval_thresh
    pal <- setNames(candidate_palette[seq_len(nrow(C))], C$id)
    inside_any <- apply(D <= thr, 1, any)
    V$color_id <- pref1(); V$alpha <- ifelse(inside_any, 0.9, 0.3)
    
    p <- ggplot() +
      geom_point(data=V, aes(x=x, y=y, color=factor(color_id), alpha=alpha), size=1.8) +
      geom_text(data=C, aes(x=x, y=y, label=id, color=id), fontface=2, size=6, show.legend=FALSE) +
      geom_circle(data=C, aes(x0=x, y0=y, r=thr, color=id), alpha=0.25, inherit.aes=FALSE) +
      scale_color_manual(values=pal, guide="none") +
      scale_alpha_identity() +
      coord_fixed(xlim=c(-100,100), ylim=c(-100,100), expand=FALSE, clip = "off") +
      scale_x_continuous(expand = expansion(mult=c(0.02,0.02))) +
      scale_y_continuous(expand = expansion(mult=c(0.02,0.02))) +
      theme_bw() +
      theme(panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            aspect.ratio=1,
            plot.margin = margin(t = 5, r = 10, b = 28, l = 32)) +
      labs(title = "Voter & Candidate Positions",
           x="Economic Scale", y="Social Scale")
    
    p <- add_pole_labels(p)
    p
  })
  
  # ---------- RCV composition helper ----------
  rcv_composition_round <- function(round_index) {
    out <- rcv_out(); rm <- rank_matrix(); C <- candidateData()
    src_idx <- first_choice_idx()
    snap <- out$rounds[[round_index]]; active <- snap$active
    dest_idx <- top_choice_given_active(rm, active)
    tibble(src = C$id[src_idx], dest = C$id[dest_idx]) |>
      filter(!is.na(dest)) |>
      count(dest, src, name = "count") |>
      complete(dest = C$id[active], src = C$id, fill = list(count = 0))
  }
  
  # ---------- results bars ----------
  bars_plurality <- reactive({
    pal <- setNames(candidate_palette[seq_len(input$candidate_count)], candidate_ids())
    df <- plurality_summary()
    maxv <- max(df$Votes); is_tie <- sum(df$Votes == maxv) > 1
    winners <- if (is_tie) df$candidate[df$Votes == maxv] else df$candidate[which.max(df$Votes)]
    
    p <- ggplot(df, aes(x=candidate, y=Votes, fill=candidate)) +
      geom_col() +
      scale_fill_manual(values=pal, guide="none") +
      scale_y_continuous(limits=c(0, input$total_voters), breaks=integer_breaks()) +
      theme_bw() +
      theme(panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            aspect.ratio=1) +
      labs(title = "Plurality Results", x="Candidate", y="Votes")
    
    p <- add_bar_value_labels(p, df, "candidate", "Votes", input$total_voters, 0)
    p <- add_half_line_discrete(p, input$total_voters/2, candidate_ids())
    p <- add_winner_text_center(p, winners, candidate_ids(), input$total_voters)
    p
  })
  
  bars_score <- reactive({
    pal <- setNames(candidate_palette[seq_len(input$candidate_count)], candidate_ids())
    df <- score_table()
    minv <- min(df$mean_distance); is_tie <- sum(abs(df$mean_distance - minv) < 1e-9) > 1
    winners <- if (is_tie) df$candidate[abs(df$mean_distance - minv) < 1e-9]
    else df$candidate[which.min(df$mean_distance)]
    
    p <- ggplot(df, aes(x=candidate, y=mean_distance, fill=candidate)) +
      geom_col() +
      scale_fill_manual(values=pal, guide="none") +
      scale_y_continuous(limits=c(0, max_dist)) +
      theme_bw() +
      theme(panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            aspect.ratio=1) +
      labs(title = "Cardinal (Score) Results", x="Candidate", y="Mean distance (lower is better)")
    
    p <- add_bar_value_labels(p, df, "candidate", "mean_distance", max_dist, 1)
    p <- add_winner_text_center(p, winners, candidate_ids(), max_dist)
    p
  })
  
  bars_approval <- reactive({
    pal <- setNames(candidate_palette[seq_len(input$candidate_count)], candidate_ids())
    df <- approval_summary()
    df$fill <- df$candidate
    pal_full <- c(pal, "Didn't vote" = "#777777")
    
    # Force order: all candidates first, then "Didn't vote" last
    x_levels <- c(candidate_ids(), "Didn't vote")
    df$candidate <- factor(df$candidate, levels = x_levels)
    
    # Axis labels (multiline for Didn't vote when >10 candidates)
    x_labels <- setNames(x_levels, x_levels)
    if (input$candidate_count > 10) {
      x_labels["Didn't vote"] <- "Didn't\nvote"
    }
    
    # Winners ignore "Didn't vote"
    cand_df <- df |> filter(as.character(candidate) != "Didn't vote")
    maxv <- max(cand_df$value)
    winners <- if (sum(cand_df$value == maxv) > 1) cand_df$candidate[cand_df$value == maxv]
    else cand_df$candidate[which.max(cand_df$value)]
    
    p <- ggplot(df, aes(x = candidate, y = value, fill = fill)) +
      geom_col() +
      scale_fill_manual(values = pal_full, guide = "none") +
      scale_x_discrete(limits = x_levels, labels = x_labels) +
      scale_y_continuous(limits = c(0, input$total_voters), breaks = integer_breaks()) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            aspect.ratio = 1,
            plot.margin = margin(t = 2, r = 5, b = 5, l = 5),
            axis.text.x = element_text(hjust = 0.5)) +
      labs(title = sprintf("Approval Results (threshold = %s)", input$approval_thresh),
           x = "Candidate", y = "Approvals")
    
    p <- add_bar_value_labels(p, df, "candidate", "value", input$total_voters, 0)
    p <- add_winner_text_center(p, winners, candidate_ids(), input$total_voters)
    p <- add_half_line_discrete(p, input$total_voters/2, x_levels)
    p
  })
  
  # ---------- RCV bars (explicit rectangles so sizes match counts exactly) ----------
  bars_rcv_round <- reactive({
    out <- rcv_out(); r <- rcv_round(); C <- candidateData()
    rounds <- out$rounds; snap <- rounds[[r]]
    active <- snap$active
    dest_levels <- C$id[active]
    final_tie <- is.na(out$winner_index) && length(out$tie_indices) == 2L
    
    title_main <- sprintf("Ranked-Choice Results • Round %d of %d", r, length(rounds))
    subtitle   <- if (!is.na(snap$eliminated)) paste0("Eliminated: ", C$id[snap$eliminated]) else NULL
    
    # Round 1: simple bars
    if (r == 1) {
      counts <- snap$counts
      df <- tibble(candidate = C$id, votes = counts, active = active) |>
        filter(active)
      p <- ggplot(df, aes(x = candidate, y = votes, fill = candidate)) +
        geom_col() +
        scale_fill_manual(values = setNames(candidate_palette[seq_len(nrow(C))], C$id), guide = "none") +
        scale_y_continuous(limits = c(0, input$total_voters), breaks = integer_breaks()) +
        theme_bw() +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              aspect.ratio = 1,
              plot.margin = margin(t = 2, r = 5, b = 5, l = 5)) +
        labs(title = title_main, subtitle = subtitle, x = "Candidate", y = "Votes")
      p <- add_bar_value_labels(p, df, "candidate", "votes", input$total_voters, 0)
      p <- add_half_line_discrete(p, input$total_voters/2, df$candidate)
      return(p)
    }
    
    # r >= 2
    comp_prev <- rcv_composition_round(r - 1)
    comp_cur  <- rcv_composition_round(r)
    
    joined <- full_join(comp_prev, comp_cur, by = c("dest","src"), suffix = c("_prev","_cur")) |>
      mutate(count_prev = replace_na(count_prev, 0L),
             count_cur  = replace_na(count_cur, 0L),
             base_count = pmin(count_prev, count_cur),
             new_count  = pmax(count_cur - base_count, 0L)) |>
      filter(dest %in% dest_levels)
    
    order_tbl <- joined |>
      transmute(dest, src, total_cur = count_cur) |>
      group_by(dest) |>
      arrange(desc(total_cur), .by_group = TRUE) |>
      mutate(src_order = row_number()) |>
      ungroup()
    
    parts <- bind_rows(
      joined |> filter(base_count > 0) |> transmute(dest, src, count = base_count, layer = "base"),
      joined |> filter(new_count  > 0) |> transmute(dest, src, count = new_count,  layer = "new")
    ) |>
      left_join(order_tbl, by = c("dest","src")) |>
      mutate(layer = factor(layer, levels = c("base","new"))) |>
      arrange(dest, src_order, layer)
    
    x_idx <- setNames(seq_along(dest_levels), dest_levels)
    rects <- parts |>
      group_by(dest) |>
      mutate(ymin = cumsum(lag(count, default = 0)),
             ymax = ymin + count,
             x    = x_idx[dest],
             xmin = x - 0.45, xmax = x + 0.45) |>
      ungroup()
    
    src_cols <- setNames(candidate_palette[seq_len(nrow(C))], C$id)
    
    totals <- comp_cur |>
      filter(dest %in% dest_levels) |>
      group_by(dest) |> summarise(votes = sum(count), .groups = "drop") |>
      mutate(x = x_idx[dest])
    
    seg_labels <- rects |>
      mutate(label_y = pmax(ymin + 0.3, 0.6),
             x_left  = x - 0.28,
             letter  = src)
    
    p <- ggplot() +
      geom_rect(data = rects,
                aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = src),
                color = NA) +
      scale_fill_manual(values = src_cols, guide = "none") +
      scale_y_continuous(limits = c(0, input$total_voters), breaks = integer_breaks()) +
      scale_x_continuous(limits = c(0.5, length(dest_levels) + 0.5),
                         breaks = seq_along(dest_levels),
                         labels = dest_levels) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            aspect.ratio = 1,
            plot.margin = margin(t = 2, r = 5, b = 5, l = 5)) +
      labs(title = title_main, subtitle = subtitle, x = "Candidate", y = "Votes") +
      geom_text(data = seg_labels,
                aes(x = x, y = label_y, label = count),
                color = "black", fontface = 2, size = 3.2) +
      geom_text(data = seg_labels,
                aes(x = x_left, y = label_y, label = letter),
                color = "black", fontface = 2, size = 3.0, hjust = 1) +
      geom_text(data = totals,
                aes(x = x, y = votes + pmax(0.02 * input$total_voters, 0.8), label = votes),
                color = "black", fontface = 2, size = 4)
    
    p <- add_half_line_cont(p, input$total_voters/2, length(dest_levels) + 0.48)
    
    if (r == length(rounds)) {
      winners <- if (is.na(out$winner_index)) candidateData()$id[out$tie_indices] else candidateData()$id[out$winner_index]
      p <- add_winner_text_center(p, winners, dest_levels, input$total_voters)
    }
    p <- add_pole_labels(p)
    p
  })
  
  # ---------- Compose (top aligned) ----------
  output$plotgraph <- renderPlot({
    if (identical(input$example_type, "1-dimension")) {
      left_plot <- map_1d()
    } else {
      left_plot <- switch(
        input$voting_system,
        "plurality"     = map_default(),
        "ranked_choice" = map_rcv(),
        "approval"      = map_approval(),
        "score"         = map_default()
      )
    }
    right_plot <- switch(
      input$voting_system,
      "plurality"     = bars_plurality(),
      "ranked_choice" = bars_rcv_round(),
      "approval"      = bars_approval(),
      "score"         = bars_score()
    )
    cowplot::plot_grid(left_plot, right_plot, ncol = 2, align = "h")
  })
  
  
  
  # ---------- Voter data table ----------
  voter_table <- reactive({
    V <- voterData()
    C <- candidateData()
    D <- dist_matrix()                   # N x K
    rm <- rank_matrix()                  # N x K (indices)
    N <- nrow(D); K <- ncol(D)
    
    dist_df <- as_tibble(D)
    names(dist_df) <- paste0(C$id, "_Distance")
    
    pref_letters <- matrix(C$id[rm], nrow = N, ncol = K)
    pref_df <- as_tibble(pref_letters)
    names(pref_df) <- paste0("Preference_", seq_len(K))
    
    tibble(Voter = seq_len(N),
           x = round(V$x, 1),
           y = round(V$y, 1)) |>
      bind_cols(dist_df |> mutate(across(everything(), ~round(., 1)))) |>
      bind_cols(pref_df)
  })
  output$voter_table <- renderDT({
    df <- voter_table()
    pref_cols <- grep("^Preference_", names(df), value = TRUE)
    
    datatable(
      df,
      options  = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE),
      rownames = FALSE
    ) %>%
      formatStyle(pref_cols, `text-align` = "center")
  })
  
  # ---------- Results summary table (Plurality, Ranked-Choice, Approval, Cardinal (Score)) ----------
  results_table <- reactive({
    C <- candidateData(); ids <- C$id
    
    # Plurality counts for all candidates (0 if none)
    plur <- tibble(candidate = ids) |>
      left_join(plurality_summary(), by = "candidate") |>
      mutate(Votes = replace_na(Votes, 0L)) |>
      transmute(candidate, Plurality = Votes)
    
    # Ranked-Choice: final round counts for finalists; "X" otherwise
    out <- rcv_out()
    last <- out$rounds[[length(out$rounds)]]
    rc_counts <- last$counts
    rc_active <- last$active
    rc_col <- ifelse(rc_active, as.character(rc_counts), "X")
    rcv_df <- tibble(candidate = ids, `Ranked-Choice` = rc_col)
    
    # Approval counts (exclude "Didn't vote")
    appr <- approval_summary() |>
      filter(candidate %in% ids) |>
      select(candidate, value) |>
      right_join(tibble(candidate = ids), by = "candidate") |>
      mutate(value = replace_na(value, 0L)) |>
      transmute(candidate, Approval = value)
    
    # Cardinal (Score): mean distances (lower = better)
    score <- score_table() |>
      right_join(tibble(candidate = ids), by = "candidate") |>
      transmute(candidate, `Cardinal (Score)` = round(mean_distance, 1))
    
    plur |>
      left_join(rcv_df, by = "candidate") |>
      left_join(appr,   by = "candidate") |>
      left_join(score,  by = "candidate") |>
      rename(Candidate = candidate)
  })
  
  output$results_dt <- renderDT({
    df <- results_table()
    
    # winners per column
    plur_winners  <- df$Candidate[df$Plurality == max(df$Plurality, na.rm = TRUE)]
    
    rc_nums       <- suppressWarnings(as.numeric(df$`Ranked-Choice`))
    rc_max        <- max(rc_nums, na.rm = TRUE)
    rcv_winners   <- df$Candidate[!is.na(rc_nums) & rc_nums == rc_max]
    
    appr_winners  <- df$Candidate[df$Approval == max(df$Approval, na.rm = TRUE)]
    
    score_min     <- min(df$`Cardinal (Score)`, na.rm = TRUE)
    score_winners <- df$Candidate[df$`Cardinal (Score)` == score_min]
    
    datatable(
      df,
      class   = "display nowrap compact",
      options = list(
        pageLength = 10,
        scrollX    = TRUE,
        autoWidth  = TRUE,
        columnDefs = list(
          list(width = "90px",  targets = 0),                      # Candidate
          list(width = "90px",  className = "dt-center", targets = 1),  # Plurality
          list(width = "130px", className = "dt-center", targets = 2),  # Ranked-Choice
          list(width = "90px",  className = "dt-center", targets = 3),  # Approval
          list(width = "150px", className = "dt-center", targets = 4)   # Cardinal (Score)
        )
      ),
      rownames = FALSE
    ) %>%
      formatStyle(
        "Plurality",
        valueColumns = "Candidate",
        fontWeight   = styleEqual(plur_winners, rep("bold", length(plur_winners)))
      ) %>%
      formatStyle(
        "Ranked-Choice",
        valueColumns = "Candidate",
        fontWeight   = styleEqual(rcv_winners, rep("bold", length(rcv_winners)))
      ) %>%
      formatStyle(
        "Approval",
        valueColumns = "Candidate",
        fontWeight   = styleEqual(appr_winners, rep("bold", length(appr_winners)))
      ) %>%
      formatStyle(
        "Cardinal (Score)",
        valueColumns = "Candidate",
        fontWeight   = styleEqual(score_winners, rep("bold", length(score_winners)))
      )
  }, server = TRUE)
}

shinyApp(ui, server)
