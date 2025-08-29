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

# 50% line + right-edge label (discrete x)
add_half_line_discrete <- function(p, y50, x_levels, lbl = "50%") {
  p +
    geom_hline(yintercept = y50, linetype = "dashed", color = "grey55", alpha = .7) +
    annotate("text",
             x = tail(x_levels, 1), y = y50,
             label = lbl, color = "grey30", alpha = .7, # zc
             hjust = -2.5, vjust = -0.3)
}

# 50% line + right-edge label (continuous x)
add_half_line_cont <- function(p, y50, xmax, lbl = "50%") {
  p +
    geom_hline(yintercept = y50, linetype = "dashed", color = "grey55", alpha = .7) +
    annotate("text",
             x = xmax, y = y50,
             label = lbl, color = "grey30", alpha = .7,
             hjust = 1.02, vjust = -0.3)
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
"))),
  titlePanel("Simulated Voting System Outcome Comparisons"),
  sidebarLayout(
    sidebarPanel(
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
      actionButton("randomize", "Randomize"),
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
              plotOutput("plotgraph", width="100%", height="700px"), # zc
              conditionalPanel("input.show_voter_data",
                               tags$hr(),
                               h4("Voter Data"),
                               DTOutput("voter_table")
              ),
              conditionalPanel("input.show_results_table",
                               tags$hr(),
                               h4("Results by Voting System"),
                               DTOutput("results_dt")
              )
    )
  )
)

# ---------------- Server ----------------

server <- function(input, output, session) {
  
  # clamp inputs
  observeEvent(input$total_voters, {
    v <- input$total_voters; if (v > 500) updateNumericInput(session,"total_voters",value=500)
    if (v < 1) updateNumericInput(session,"total_voters",value=1)
  }, ignoreInit = TRUE)
  observeEvent(input$candidate_count, {
    v <- input$candidate_count; if (v > 20) updateNumericInput(session,"candidate_count",value=20)
    if (v < 2)  updateNumericInput(session,"candidate_count",value=2)
  }, ignoreInit = TRUE)
  
  voterData <- eventReactive(list(input$randomize, input$total_voters), {
    tibble(x = rand_dimension_voters(input$total_voters),
           y = rand_dimension_voters(input$total_voters))
  }, ignoreInit = FALSE)
  
  candidate_ids <- reactive(LETTERS[seq_len(input$candidate_count)])
  candidateData <- eventReactive(list(input$randomize, input$candidate_count), {
    tibble(x = rand_dimension_candidates(input$candidate_count),
           y = rand_dimension_candidates(input$candidate_count),
           id = candidate_ids())
  }, ignoreInit = FALSE)
  
  dist_matrix <- reactive({
    V <- voterData(); C <- candidateData()
    sqrt(outer(V$x, C$x, `-`)^2 + outer(V$y, C$y, `-`)^2)
  })
  rank_matrix <- reactive(rank_by_distance(dist_matrix()))
  first_choice_idx <- reactive(rank_matrix()[,1])
  pref1 <- reactive(candidateData()$id[first_choice_idx()])
  
  plurality_summary <- reactive({
    tibble(candidate = pref1()) |>
      count(candidate, name="Votes") |>
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
    tibble(candidate = c(C$id,"Didn't vote"),
           value = c(approvals, didnt)) |> arrange(desc(value))
  })
  
  rcv_out <- reactive({
    rm <- rank_matrix()
    rcv_irv(rm)
  })
  rcv_round <- reactiveVal(1)
  observeEvent(list(input$randomize, input$total_voters, input$candidate_count), { rcv_round(1) }, ignoreInit = FALSE)
  total_rounds <- reactive(length(rcv_out()$rounds))
  observeEvent(input$next_round, { rcv_round(min(rcv_round()+1, total_rounds())) })
  observeEvent(input$prev_round, { rcv_round(max(rcv_round()-1, 1)) })
  output$rcv_round_label <- renderText({
    paste0("Round ", rcv_round(), " of ", length(rcv_out()$rounds))
  })
  
  # ---------- maps ----------
  map_plot_generic <- function(current_choice_ids, active_mask = NULL, rcv_mode = FALSE) {
    V <- voterData(); C <- candidateData()
    pal <- setNames(candidate_palette[seq_len(nrow(C))], C$id)
    df_c <- C; df_c$alpha <- 1
    if (rcv_mode && !is.null(active_mask)) df_c$alpha <- ifelse(active_mask, 1, 0.25)
    
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
    
    g +
      scale_color_manual(values = pal, guide="none") +
      scale_alpha_identity() +
      coord_fixed(xlim=c(-100,100), ylim=c(-100,100), expand=FALSE) +
      scale_x_continuous(expand = expansion(mult=c(0.02,0.02))) +
      scale_y_continuous(expand = expansion(mult=c(0.02,0.02))) +
      theme_bw() +
      theme(panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            aspect.ratio=1) +
      labs(title = "Voter & Candidate Positions",
           x="Economic Scale", y="Social Scale")
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
    ggplot() +
      geom_point(data=V, aes(x=x, y=y, color=factor(color_id), alpha=alpha), size=1.8) +
      geom_text(data=C, aes(x=x, y=y, label=id, color=id), fontface=2, size=6, show.legend=FALSE) +
      geom_circle(data=C, aes(x0=x, y0=y, r=thr, color=id), alpha=0.25, inherit.aes=FALSE) +
      scale_color_manual(values=pal, guide="none") +
      scale_alpha_identity() +
      coord_fixed(xlim=c(-100,100), ylim=c(-100,100), expand=FALSE) +
      scale_x_continuous(expand = expansion(mult=c(0.02,0.02))) +
      scale_y_continuous(expand = expansion(mult=c(0.02,0.02))) +
      theme_bw() +
      theme(panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            aspect.ratio=1) +
      labs(title = "Voter & Candidate Positions",
           x="Economic Scale", y="Social Scale")
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
    p
  })
  
  # ---------- Compose (top aligned) ----------
  output$plotgraph <- renderPlot({
    plots <- switch(
      input$voting_system,
      "plurality"     = list(map_default(),   bars_plurality()),
      "ranked_choice" = list(map_rcv(),      bars_rcv_round()),
      "approval"      = list(map_approval(), bars_approval()),
      "score"         = list(map_default(),  bars_score())
    )
    plot_grid(plotlist = plots, ncol = 2, align = "h")
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
      options = list(
        pageLength = 10,
        scrollX    = TRUE,
        autoWidth  = TRUE,
        columnDefs = list(
          list(width = "9ch",  targets = 0),                        # Candidate
          list(width = "9ch",  className = "dt-center", targets = 1),  # Plurality
          list(width = "13ch", className = "dt-center", targets = 2),  # Ranked-Choice
          list(width = "9ch",  className = "dt-center", targets = 3),  # Approval
          list(width = "16ch", className = "dt-center", targets = 4)   # Cardinal (Score)
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
