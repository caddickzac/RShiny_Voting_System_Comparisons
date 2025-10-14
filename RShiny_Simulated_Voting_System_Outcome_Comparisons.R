# app.R — Simulated Voting System Outcome Comparisons
# Includes: RCV with Borda-like tiebreaker, stacked-round visual,
# approval “Didn’t vote” handling, 50% dashed line + label,
# voter/results tables, and top-aligned plots.

library(shiny)
library(tidyverse)
library(shinyjs)
library(ggforce)
library(cowplot)
library(DT)
library(later)

# ---------------- helpers ----------------

rand_dimension_voters     <- function(n) runif(n, -100, 100)
rand_dimension_candidates <- function(n) runif(n,  -98,   98)

candidate_palette <- c(
  "#E41A1C", "#0136F5", "#4DAF4A", "#984EA3", "#2D5780",
           "#FFD92F", "#A65628", "#F781BF", "#FF7F00", "#66C2A5",
           "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#F78977",
           "#E5C494", "#B3B3B3", "#1B9E77", "#D95F02", "#7570B3"
)

# ---- 1D strip (plurality-style aesthetics) with support for RCV rounds ----
make_1d_strip <- function(V, C, top_choice_ids, active_mask = rep(TRUE, nrow(C))) {
  pal  <- setNames(candidate_palette[seq_len(nrow(C))], C$id)
  df_c <- C %>% mutate(alpha = ifelse(active_mask, 1, 0.25))  # fade eliminated
  
  # voters coloured by current top choice (factor to lock palette order)
  voters_col <- tibble(x = V$x, nearest = factor(top_choice_ids, levels = C$id))
  
  # panel extents
  x_left  <- -140
  x_right <-  105
  
  p <- ggplot() +
    # thin frame lines
    annotate("segment", x = x_left-45, xend = x_right+35, y =  1.5, yend =  1.5, colour = "black", linewidth = 0.8) +
    annotate("segment", x = x_left-45, xend = x_right+35, y = -1.7, yend = -1.7, colour = "black", linewidth = 0.8) +
    annotate("segment", x = x_left-45, xend = x_left-45, y = -1.7, yend =  1.5, colour = "black", linewidth = 0.8) +
    annotate("segment", x = x_right+35, xend = x_right+35, y = -1.7, yend =  1.5, colour = "black", linewidth = 0.8) +
    annotate("segment", x = -100, xend = -100, y = -1.7, yend = -1.9, colour = "black", linewidth = 0.8) +
    annotate("segment", x =    0, xend =    0, y = -1.7, yend = -1.9, colour = "black", linewidth = 0.8) +
    annotate("segment", x =  100, xend =  100, y = -1.7, yend = -1.9, colour = "black", linewidth = 0.8) +
    
    # left-side labels/key
    annotate("text", x = x_left - 35, y =  .1, label = 'atop(bold("Candidates"))',
             hjust = 0, fontface = 2, colour = "#59A4F0", size = 4.7, parse = TRUE) +
    annotate("text", x = x_left - 35, y = -.4, label = "Voter",
             hjust = 0, fontface = 2, colour = "black", size = 4.7) +
    annotate("text", x = x_left - 35, y = -.8, label = "Preferences",
             hjust = 0, fontface = 2, colour = "black", size = 4.7) +
    annotate("segment", x = x_left - 35, xend = -110, y = 0, yend = 0, colour = "#BEBEBE", linewidth = 1) +
    
    # main axis + brackets
    annotate("segment", x = -101, xend = 101, y = 0, yend = 0, colour = "#BEBEBE", linewidth = 1) +
    annotate("segment", x = -101, xend = -101, y = -.7, yend =  .7, colour = "#BEBEBE", linewidth = 1) +
    annotate("segment", x =  101, xend =  101, y = -.7, yend =  .7, colour = "#BEBEBE", linewidth = 1) +
    
    # voters as ticks below axis (coloured by nearest)
    geom_segment(data = voters_col, aes(x = x, xend = x, colour = nearest),
                 y = -.5, yend = -0.03, linewidth = 1, inherit.aes = FALSE) +
    
    # candidate ticks above axis in fixed blue (faded if eliminated)
    geom_segment(data = df_c, aes(x = x, xend = x, alpha = alpha),
                 y = 0.03, yend = .5, colour = "#59A4F0", linewidth = 1, inherit.aes = FALSE) +
    
    # candidate letters (palette colours; faded if eliminated)
    geom_text(data = df_c, aes(x = x, y = .8, label = id, colour = id, alpha = alpha),
              fontface = 2, size = 8, inherit.aes = FALSE) +
    
    # numbers/words
    annotate("text", x = -100, y = -1.4, label = "-100", fontface = 2, colour = "grey40", size = 5, hjust = .5) +
    annotate("text", x =    0, y = -1.4, label =   "0",  fontface = 2, colour = "grey40", size = 5, hjust = .5) +
    annotate("text", x =  100, y = -1.4, label = "100",  fontface = 2, colour = "grey40", size = 5, hjust = .5) +
    annotate("text", x = -100, y = -2.2, label = "Liberal",      colour = "grey30", size = 5, fontface = 2, hjust = .5) +
    annotate("text", x =    0, y = -2.2, label = "Moderate",     colour = "grey30", size = 5, fontface = 2, hjust = .5) +
    annotate("text", x =  100, y = -2.2, label = "Conservative", colour = "grey30", size = 5, fontface = 2, hjust = .5) +
    annotate("text", x =    0, y = -3,   label = "Political Leaning", colour = "black", size = 9, hjust = .5) +
    
    scale_colour_manual(values = pal, guide = "none") +
    scale_alpha_identity() +
    coord_cartesian(xlim = c(x_left - 45, x_right + 35), ylim = c(-7.5, 5.5), clip = "off") +
    theme_bw() +
    theme(
      legend.position = "none",
      axis.title = element_blank(),
      axis.text  = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.margin = margin(t = 5, r = 15, b = 28, l = 5)
    ) +
    labs(title = "Voter & Candidate Positions")
  
  # red X overlay for eliminated candidates (cover the letter only)
  elim_df <- df_c %>% filter(alpha < 1)
  if (nrow(elim_df)) {
    lab_y <- 0.80   # where the candidate letter sits
    y_pad <- 0.22   # half-height of the X
    x_pad <- 3.5    # half-width of the X
    
    p <- p +
      geom_segment(
        data = elim_df, aes(x = x - x_pad, xend = x + x_pad),
        y = lab_y - y_pad, yend = lab_y + y_pad,
        colour = "red2", linewidth = 1.2, inherit.aes = FALSE
      ) +
      geom_segment(
        data = elim_df, aes(x = x - x_pad, xend = x + x_pad),
        y = lab_y + y_pad, yend = lab_y - y_pad,
        colour = "red2", linewidth = 1.2, inherit.aes = FALSE
      )
  }
  
  p
}

# ---- 1D Approval strip: shaded boxes instead of circles ----
make_1d_strip_approval <- function(V, C, thr, top_choice_ids, inside_any,
                                   box_alpha = 0.18, bracket_lwd = 0.25) {
  pal <- setNames(candidate_palette[seq_len(nrow(C))], C$id)
  
  # voters: color by nearest; fade if they approve nobody
  voters_col <- tibble(
    x       = V$x,
    nearest = factor(top_choice_ids, levels = C$id),
    alpha   = ifelse(inside_any, 1, 0.35)
  )
  
  # approval rectangles (clamped to axis span)
  rect_df <- C |>
    transmute(
      id,
      xmin = pmax(x - thr, -101),
      xmax = pmin(x + thr,  101),
      ymin = -0.5,  # bottom of voter line
      ymax =  0.5   # top of candidate line
    )
  
  # U-bracket segments (top + left + right)
  seg_top   <- rect_df |> transmute(id, x = xmin, xend = xmax, y = ymax,    yend = ymax)
  seg_left  <- rect_df |> transmute(id, x = xmin, xend = xmin, y = ymin+.7, yend = ymax)
  seg_right <- rect_df |> transmute(id, x = xmax, xend = xmax, y = ymin+.7, yend = ymax)
  
  # panel extents to match your 1D layout
  x_left  <- -140
  x_right <-  105
  
  ggplot() +
    # shaded approval box
    geom_rect(
      data = rect_df,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = id),
      alpha = box_alpha, color = NA
    ) +
    # U-bracket overlay (thin, constant linewidth)
    geom_segment(
      data = seg_top,
      aes(x = x, xend = xend, y = y, yend = yend, colour = id),
      linewidth = bracket_lwd, lineend = "butt"
    ) +
    geom_segment(
      data = seg_left,
      aes(x = x, xend = xend, y = y, yend = yend, colour = id),
      linewidth = bracket_lwd, lineend = "butt"
    ) +
    geom_segment(
      data = seg_right,
      aes(x = x, xend = xend, y = y, yend = yend, colour = id),
      linewidth = bracket_lwd, lineend = "butt"
    ) +
    # frame + axis furniture
    annotate("segment", x = x_left-45, xend = x_right+35, y =  1.5, yend =  1.5, colour = "black", linewidth = 0.8) +
    annotate("segment", x = x_left-45, xend = x_right+35, y = -1.7, yend = -1.7, colour = "black", linewidth = 0.8) +
    annotate("segment", x = x_left-45, xend = x_left-45, y = -1.7, yend =  1.5, colour = "black", linewidth = 0.8) +
    annotate("segment", x = x_right+35, xend = x_right+35, y = -1.7, yend =  1.5, colour = "black", linewidth = 0.8) +
    annotate("segment", x = -100, xend = -100, y = -1.7, yend = -1.9, colour = "black", linewidth = 0.8) +
    annotate("segment", x =    0, xend =    0, y = -1.7, yend = -1.9, colour = "black", linewidth = 0.8) +
    annotate("segment", x =  100, xend =  100, y = -1.7, yend = -1.9, colour = "black", linewidth = 0.8) +
    annotate("text", x = x_left - 35, y =  .1, label = 'atop(bold("Candidates"))',
             hjust = 0, fontface = 2, colour = "#59A4F0", size = 4.7, parse = TRUE) +
    annotate("text", x = x_left - 35, y = -.4, label = "Voter",
             hjust = 0, fontface = 2, colour = "black", size = 4.7) +
    annotate("text", x = x_left - 35, y = -.8, label = "Preferences",
             hjust = 0, fontface = 2, colour = "black", size = 4.7) +
    annotate("segment", x = x_left - 35, xend = -110, y = 0, yend = 0, colour = "#BEBEBE", linewidth = 1) +
    annotate("segment", x = -101, xend = 101, y = 0, yend = 0, colour = "#BEBEBE", linewidth = 1) +
    annotate("segment", x = -101, xend = -101, y = -.7, yend =  .7, colour = "#BEBEBE", linewidth = 1) +
    annotate("segment", x =  101, xend =  101, y = -.7, yend =  .7, colour = "#BEBEBE", linewidth = 1) +
    # voters + candidate marks
    geom_segment(
      data = voters_col,
      aes(x = x, xend = x, colour = nearest, alpha = alpha),
      y = -.5, yend = -0.03, linewidth = 1.1, inherit.aes = FALSE
    ) +
    geom_segment(
      data = C, aes(x = x, xend = x),
      y = 0.03, yend = .5, colour = "#59A4F0", linewidth = 1.1, inherit.aes = FALSE
    ) +
    geom_text(
      data = C, aes(x = x, y = .8, label = id, colour = id),
      fontface = 2, size = 8, inherit.aes = FALSE
    ) +
    # labels under axis
    annotate("text", x = -100, y = -1.4, label = "-100", fontface = 2, colour = "grey40", size = 5, hjust = .5) +
    annotate("text", x =    0, y = -1.4, label =   "0",  fontface = 2, colour = "grey40", size = 5, hjust = .5) +
    annotate("text", x =  100, y = -1.4, label = "100",  fontface = 2, colour = "grey40", size = 5, hjust = .5) +
    annotate("text", x = -100, y = -2.2, label = "Liberal",      colour = "grey30", size = 5, fontface = 2, hjust = .5) +
    annotate("text", x =    0, y = -2.2, label = "Moderate",     colour = "grey30", size = 5, fontface = 2, hjust = .5) +
    annotate("text", x =  100, y = -2.2, label = "Conservative", colour = "grey30", size = 5, fontface = 2, hjust = .5) +
    annotate("text", x =    0, y = -3,   label = "Political Leaning", colour = "black", size = 9, hjust = .5) +
    scale_colour_manual(values = pal, guide = "none") +
    scale_fill_manual(values = pal,  guide = "none") +
    scale_alpha_identity() +
    coord_cartesian(xlim = c(x_left - 45, x_right + 35), ylim = c(-7.5, 5.5), clip = "off") +
    theme_bw() +
    theme(
      legend.position = "none",
      axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
      panel.grid = element_blank(), plot.margin = margin(t = 5, r = 15, b = 28, l = 5)
    ) +
    labs(title = "Voter & Candidate Positions")
}

and_join <- function(x) {
  x <- as.character(x); n <- length(x)
  if (n == 0) return("")
  if (n == 1) return(x)
  if (n == 2) return(paste(x, collapse = " and "))
  paste(paste(x[1:(n-1)], collapse = ", "), x[n], sep = ", and ")
}

# sample integer positions with a minimum gap in "notches"
# ensures |xi - xj| >= min_gap for all selected points
sample_min_gap_int <- function(n, lo, hi, min_gap = 2) {
  if (n < 1) return(integer(0))
  pool <- lo:hi
  if (n > length(pool)) stop(" too small for requested n")
  sel <- integer(0)
  while (length(sel) < n) {
    if (!length(pool)) stop("Not enough positions for the requested min_gap.")
    pick <- sample(pool, 1)
    sel  <- c(sel, pick)
    pool <- pool[abs(pool - pick) >= min_gap]  # prune neighbors that are too close
  }
  as.numeric(sort(sel))
}


#  rcv safeguard
safe_round_index <- function(out, r) {
  if (length(out$rounds) == 0) return(1L)
  max(1L, min(r, length(out$rounds)))
}

# approval pole labeller 
# approval pole labels — draw OUTSIDE the panel, matching other plots’ alignment
add_pole_labels_outside <- function(p,
                                    col = "grey25",
                                    size_pt = 11,
                                    fw = 2,
                                    inset = 0.02) {  # inset as fraction of the canvas
  cowplot::ggdraw(p) +
    # bottom-left “Left” (left-justified, sits just above the bottom edge)
    cowplot::draw_label("Left",
                        x = inset, y = inset,
                        hjust = -2.8, vjust = -4.2,
                        colour = col, fontface = fw, size = size_pt) +
    # bottom-right “Right” (right-justified)
    cowplot::draw_label("Right",
                        x = 1 - inset, y = inset,
                        hjust = 1, vjust = -4.2,
                        colour = col, fontface = fw, size = size_pt) +
    # top-left vertical “Authoritarian” (top-aligned)
    cowplot::draw_label("Authoritarian",
                        x = inset, y = 1 - inset, angle = 90,
                        hjust = 1.4, vjust = 4.5,
                        colour = col, fontface = fw, size = size_pt) +
    # bottom-left vertical “Libertarian” (bottom-aligned)
    cowplot::draw_label("Libertarian",
                        x = inset, y = inset, angle = 90,
                        hjust = -.95, vjust = 4.5,
                        colour = col, fontface = fw, size = size_pt)
}

# ---- political compass pole labels (for 2D maps) ----
add_pole_labels <- function(p,
                            x_min = -100, x_max = 100,
                            y_min = -100, y_max = 100,
                            off_bottom = 8,
                            off_left   = 15) {
  p +
    annotate("text", x = x_min, y = y_min - off_bottom,
             label = "Left",  hjust = 0, vjust = 1, size = 3.6, fontface = 2, color = "grey25") +
    annotate("text", x = x_max, y = y_min - off_bottom,
             label = "Right", hjust = 1, vjust = 1, size = 3.6, fontface = 2, color = "grey25") +
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
  pad  <- max(pad_frac * ymax, pad_min)
  p +
    geom_hline(yintercept = y50, linetype = "dashed", color = "grey55", alpha = .7) +
    annotate("text", x = Inf, y = y50 + pad, label = lbl,
             color = "grey30", alpha = .7, angle = 90, hjust = 0, vjust = -.5)
}

# Continuous-x plots (RCV)
add_half_line_cont <- function(p, y50, ..., lbl = "50%",
                               pad_frac = 0.01, pad_min = 0.3) {
  ymax <- y50 * 2
  pad  <- max(pad_frac * ymax, pad_min)
  p +
    geom_hline(yintercept = y50, linetype = "dashed", color = "grey55", alpha = .7) +
    annotate("text", x = Inf, y = y50 + pad, label = lbl,
             color = "grey30", alpha = .7, angle = 90, hjust = 0, vjust = -.5)
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
    aes(x = .data[[x_col]], y = label_y, label = lbl),
    color = "black", fontface = 2, size = 3.6,
    inherit.aes = FALSE
  )
}

top_choice_given_active <- function(rank_mat, active_mask) {
  active_mask <- as.logical(active_mask)
  active_mask[is.na(active_mask)] <- FALSE
  
  apply(rank_mat, 1, function(row) {
    for (j in seq_along(row)) {
      id <- row[j]
      if (!is.finite(id)) next
      if (id >= 1L && id <= length(active_mask) && isTRUE(active_mask[id])) {
        return(id)
      }
    }
    NA_integer_
  })
}

# -------------- RCV with Borda-like tiebreaker, then alphabetical --------------
rcv_irv <- function(rank_mat) {
  K <- ncol(rank_mat); N <- nrow(rank_mat)
  active <- rep(TRUE, K); majority <- floor(N/2) + 1
  rounds <- list(); eliminated <- integer()
  
  # Borda-like scores once (sum of K - position), lower = worse
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
  tags$head(
    tags$style(HTML("
      .main-panel { padding-top:0 !important; padding-bottom:40px !important; }
      .main-panel .shiny-plot-output { margin-top:-10px; }
      .main-panel .plot-container { padding-top:0 !important; }
      .dataTables_wrapper .dataTables_scrollHead table,
      .dataTables_wrapper .dataTables_scrollBody table { width:100% !important; }
      .container-fluid { padding-bottom:40px; }
      .dataTables_wrapper { margin-bottom:16px; }

      /* toolbar row */
      .toolbar-inline{
        display:flex; gap:.5rem; align-items:flex-start; flex-wrap:wrap;
      }
      .toolbar-inline .btn{
        height:34px; padding:6px 12px; line-height:1.42857143; vertical-align:middle;
      }

      /* compact fileInput: hide the filename box, keep button */
      #import_csv.shiny-input-container{ width:auto; margin:0; }
      #import_csv .input-group{ display:inline-flex; width:auto; align-items:center; }
      #import_csv .form-control{ display:none !important; }   /* <- hides big filename field */
      #import_csv .btn{ height:34px; padding:6px 12px; }

      /* progress/status bar: always on its own row under the buttons */
      #import_csv .shiny-file-input-progress{
        order:99; flex-basis:100%; width:100%; margin:.35rem 0 0 0;
      }
      #import_csv .shiny-file-input-progress .progress{ margin-bottom:0; }
      #import_csv .shiny-file-input-progress .progress-bar{ text-align:center; }
    ")),
    # JS helper to clear filename + hide progress on demand
    tags$script(HTML("
      Shiny.addCustomMessageHandler('toolbarReset', function(id){
        var c = document.getElementById(id);
        if(!c) return;
        var fi = c.querySelector('input[type=file]');
        if(fi) fi.value = '';
        var txt = c.querySelector('.form-control');
        if(txt){ txt.value=''; txt.placeholder='Select CSV'; }
        var progs = c.querySelectorAll('.shiny-file-input-progress');
        progs.forEach(function(p){
          p.style.display = 'none';
          p.innerHTML = '';
        });
      });
    "))
  ),
  
  titlePanel("Simulated Voting System Outcome Comparisons"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("example_type","Example Type",
                  choices=c("1-dimension","2-dimension"), selected="1-dimension"),
      numericInput("total_voters","Number of voters: (max=50)", value=11, min=1, max=50),
      numericInput("candidate_count","Number of candidates: (max=8)", value=3, min=2, max=8),
      selectInput("voting_system","See full results:",
                  c("Approval"="approval","Borda Count"="borda","Cardinal (Score)"="score",
                    "Plurality"="plurality","Ranked-Choice"="ranked_choice"),
                  selected="plurality"),
      conditionalPanel("input.voting_system == 'approval'",
                       sliderInput("approval_thresh","Approval distance threshold", min=5,max=150,value=50,step=5)
      ),
      
      selectInput(
        "scenario",
        "Scenario:",
        choices = c(
          "Random",
          "Center Squeeze (1D)",
          "Spoiler Effect (1D)",
          "Mutual Majority Criterion (1D)",
          "Condorcet Cycle (2D)",
          "Clone Penalty (1D)"
        ),
        selected = "Random"
      ),
      # show/hide candidates
      checkboxInput("enable_filter", "Remove candidates", FALSE),
      conditionalPanel(
        "input.enable_filter",
        uiOutput("candidate_filter_ui")
      ),
      tags$div(class="toolbar-inline",
               actionButton("randomize","Randomize Data"),
               downloadButton("export_csv", "Export CSV"),
               uiOutput("import_ui")),   # <- fileInput will be rendered here
      checkboxInput("show_explanation","Show results explanation", FALSE),
      checkboxInput("show_voter_data","Show voter data", FALSE),
      checkboxInput("show_results_table","Show results table", FALSE),
      checkboxInput("show_criteria","Show voting criterion assessments", FALSE),
      
      conditionalPanel("input.voting_system == 'ranked_choice'",
                       hr(),
                       tags$div("View RCV rounds", style="font-weight:bold;margin-bottom:.25rem;"),
                       tags$div(style="display:flex;gap:.5rem;align-items:center;",
                                actionButton("prev_round","◀"),
                                actionButton("next_round","▶"),
                                tags$div(textOutput("rcv_round_label"),
                                         style="margin-left:.5rem;font-weight:bold;")
                       ))
    ),
    
    mainPanel(class="main-panel", style="padding-top:0;margin-top:0;",
              plotOutput("plotgraph", width="100%", height="560px"),
              uiOutput("scenario_desc"),
              conditionalPanel("input.show_explanation",
                               tags$hr(), h4("Explanation of Voting Results"), uiOutput("explanation")),
              conditionalPanel("input.show_voter_data",
                               tags$hr(), h4("Voter Data"), DTOutput("voter_table")),
              conditionalPanel("input.show_results_table",
                               tags$hr(), h4("Results by Voting System"), DTOutput("results_dt", width="100%")),
              conditionalPanel("input.show_criteria",
                               tags$hr(), h4("Voting criteria assessment"), DTOutput("criteria_dt"),
                               tags$br(), uiOutput("criteria_help"))
    )
  )
)

# ---------------- Server ----------------
server <- function(input, output, session) {
  # caps that depend on Example Type
  voters_max     <- reactive(if (identical(input$example_type, "1-dimension")) 50 else 500)
  candidates_max <- reactive(if (identical(input$example_type, "1-dimension")) 8  else 20)
  
  updating <- reactiveVal(FALSE)
  
  # robust guard releaser: wait one tick, then release only when inputs match
  release_guard_when <- function(session, cond) {
    armed <- reactiveVal(FALSE)
    later::later(function() armed(TRUE), 0)  # arm on next event loop tick
    
    obs <- NULL
    obs <- observe({
      if (isTRUE(armed()) && isTRUE(cond())) {
        suppress_randomize(FALSE)   # allow normal observers again
        obs$destroy()
      }
    }, priority = 100)
  }
  
  approval_thr <- reactive({
    if (is.null(input$approval_thresh)) 50 else input$approval_thresh
  })
  
  
  # ---- import/export plumbing ----
  V_override <- reactiveVal(NULL)   # when set, voterData() will use this
  C_override <- reactiveVal(NULL)   # when set, candidateData() will use this
  suppress_randomize <- reactiveVal(FALSE) # block "auto-randomize" while we are programmatically applying a scenario/import
  rand_bump <- reactiveVal(0L)
  bump_random <- function() rand_bump(rand_bump() + 1L)
  import_bump <- reactiveVal(0)     # trigger to recompute eventReactives after import
  
  output$export_csv <- downloadHandler(
    filename = function() sprintf("voting-scenario-%s.csv", Sys.Date()),
    contentType = "text/csv",
    content = function(file) {
      V <- voterData()
      C <- candidateData()
      df <- dplyr::bind_rows(
        dplyr::transmute(V, x = x, y = y, label = "voter"),
        dplyr::transmute(C, x = x, y = y, label = paste0("candidate_", tolower(id)))
      )
      utils::write.csv(df, file, row.names = FALSE)  # <-- header is just x,y,label
    }
  )
  
  # Find the real header row 'x,y,label' (in case an old file still has "Example Type" lines)
  read_scenario_csv <- function(path) {
    lines <- readLines(path, warn = FALSE)
    lines <- sub("^\ufeff", "", lines)  # strip BOM if present
    hdr_rx  <- "^\\s*x\\s*,\\s*y\\s*,\\s*label\\s*$"
    hdr_row <- which(grepl(hdr_rx, tolower(lines)))[1]
    if (is.na(hdr_row)) hdr_row <- 1L  # assume simple file with header on first row
    utils::read.csv(text = paste(lines[hdr_row:length(lines)], collapse = "\n"),
                    stringsAsFactors = FALSE, check.names = FALSE)
  }
  
  # Rebuild handle for the Import CSV control
  import_reset <- reactiveVal(0)
  
  # Render the fileInput (depends on import_reset())
  output$import_ui <- renderUI({
    import_reset()  # dependency
    fileInput("import_csv", label = NULL, accept = ".csv",
              buttonLabel = "Import CSV", placeholder = "Select CSV",
              width = "220px")
  })
  
  output$scenario_desc <- renderUI({
    nm <- input$scenario
    if (is.null(nm) || identical(nm, "Random")) return(NULL)
    
    desc <- scenario_descriptions[[nm]]
    if (is.null(desc)) {
      desc <- HTML(sprintf("<p><b>%s</b> — no description provided yet.</p>", nm))
    }
    
    tags$div(
      tags$hr(),
      tags$h4("Scenario description"),
      desc,
      style = "margin-top:0.5rem;margin-bottom:0.75rem;"
    )
  })
  
  
  observeEvent(input$import_csv, {
    req(input$import_csv$datapath)
    df <- try(read_scenario_csv(input$import_csv$datapath), silent = TRUE)
    if (inherits(df, "try-error")) {
      showNotification("Import failed: could not read CSV.", type = "error", duration = 6); return()
    }
    
    # normalize + validate columns
    names(df) <- tolower(trimws(names(df)))
    if (!all(c("x","y","label") %in% names(df))) {
      showNotification("Import failed: CSV must have columns x, y, label.", type = "error", duration = 6); return()
    }
    df$x <- suppressWarnings(as.numeric(df$x))
    df$y <- suppressWarnings(as.numeric(df$y))
    if (any(!is.finite(df$x)) || any(!is.finite(df$y))) {
      showNotification("Import failed: x and y must be numeric.", type = "error", duration = 6); return()
    }
    if (any(df$x < -100 | df$x > 100 | df$y < -100 | df$y > 100)) {
      showNotification("Import failed: all x and y must be in [-100, 100].", type = "error", duration = 6); return()
    }
    
    # split rows
    df$label <- tolower(trimws(df$label))
    voters <- dplyr::filter(df, label == "voter")
    cands  <- dplyr::filter(df, grepl("^candidate[_-]?([a-z])$", label))
    if (!nrow(voters)) { showNotification("Import failed: no voters found.", type="error", duration = 6); return() }
    if (!nrow(cands))  { showNotification("Import failed: no candidates found.", type="error", duration = 6); return() }
    
    # candidate IDs (A..)
    cands$letter <- toupper(sub("^candidate[_-]?([a-z])$", "\\1", cands$label))
    dup_ids <- cands$letter[duplicated(cands$letter)]
    if (length(dup_ids)) {
      showNotification(sprintf("Import failed: duplicate rows for candidate(s): %s.",
                               paste(sort(unique(dup_ids)), collapse=", ")),
                       type="error", duration = 6); return()
    }
    # enforce contiguous A..K for palette/tables
    K <- nrow(cands)
    expected <- LETTERS[seq_len(K)]
    if (!setequal(cands$letter, expected)) {
      showNotification(sprintf(
        "Import failed: candidate labels must be candidate_%s..candidate_%s with no gaps.",
        tolower(expected[1]), tolower(expected[K])
      ), type="error", duration = 6); return()
    }
    cands <- dplyr::arrange(cands, letter)
    
    # --- Auto pick 1D vs 2D based on y-values
    is_1d <- all(abs(df$y) < 1e-9)
    suppress_randomize(TRUE)
    freezeReactiveValue(input, "example_type")
    freezeReactiveValue(input, "total_voters")
    freezeReactiveValue(input, "candidate_count")
    
    target_type <- if (is_1d) "1-dimension" else "2-dimension"
    
    # define tv/cc from the parsed data BEFORE we reference them elsewhere
    tv <- nrow(voters)
    cc <- nrow(cands)
    
    updateSelectInput(session, "example_type", selected = target_type)
    updateNumericInput(session, "total_voters",   value = tv)
    updateNumericInput(session, "candidate_count", value = cc)
    
    release_guard_when(session, function(){
      isTRUE(all.equal(input$example_type, target_type)) &&
        isTRUE(all.equal(input$total_voters,   tv)) &&
        isTRUE(all.equal(input$candidate_count, cc))
    })
    
    # Per-type geometry checks
    MIN_GAP_1D <- 2
    if (is_1d) {
      # enforce spacing on x for voters & candidates
      vxs <- sort(voters$x); cxs <- sort(cands$x)
      if (length(vxs) > 1 && min(diff(vxs)) < MIN_GAP_1D) {
        showNotification(sprintf("Import failed: in 1-Dimension, voters must be ≥ %d units apart on x.", MIN_GAP_1D),
                         type="error", duration = 6); return()
      }
      if (length(cxs) > 1 && min(diff(cxs)) < MIN_GAP_1D) {
        showNotification(sprintf("Import failed: in 1-Dimension, candidates must be ≥ %d units apart on x.", MIN_GAP_1D),
                         type="error", duration = 6); return()
      }
      voters$y <- 0; cands$y <- 0
    } else {
      # 2-D: no duplicate voter coordinates
      pair_key <- paste(round(voters$x, 8), round(voters$y, 8))
      if (any(duplicated(pair_key))) {
        showNotification("Import failed: voters cannot occupy identical (x,y) points in 2-Dimension.",
                         type="error", duration = 6); return()
      }
    }
    
    # Build overrides and sync UI
    V_new <- tibble::tibble(x = as.numeric(voters$x), y = as.numeric(voters$y))
    C_new <- tibble::tibble(x = as.numeric(cands$x),  y = as.numeric(cands$y), id = cands$letter)
    updateNumericInput(session, "total_voters",   value = nrow(V_new))
    updateNumericInput(session, "candidate_count", value = nrow(C_new))
    V_override(V_new); C_override(C_new)
    import_bump(import_bump() + 1L)
    import_reset(import_reset() + 1L)
    session$sendCustomMessage("toolbarReset", "import_csv")
    
    # tidy the file input UI (clear filename + hide progress), if you kept the JS hook
    session$sendCustomMessage("toolbarReset", "import_csv")
    showNotification(sprintf("Imported %d voters and %d candidates.", nrow(V_new), nrow(C_new)), type="message")
    session$onFlushed(function() updating(FALSE), once = TRUE)
  })
  
  # If user flips 1D/2D, leave any preset, switch to Random, and redraw
  observeEvent(input$example_type, {
    if (isTRUE(updating()) || isTRUE(suppress_randomize())) return()
    if (!is.null(V_override()) || !is.null(C_override())) return()
    
    if (!identical(input$scenario, "Random")) {
      updateSelectInput(session, "scenario", selected = "Random")
    }
    bump_random()
  }, ignoreInit = TRUE, priority = 10)
  
  # If user changes voter or candidate counts, same behavior
  observeEvent(list(input$total_voters, input$candidate_count), {
    if (isTRUE(updating()) || isTRUE(suppress_randomize())) return()
    # NEW: if a preset or import override is active, do nothing
    if (!is.null(V_override()) || !is.null(C_override())) return()
    if (!identical(input$scenario, "Random")) {
      updateSelectInput(session, "scenario", selected = "Random")
    }
    bump_random()
  }, ignoreInit = TRUE, priority = 10)
  
  # clear overrides + clear the file input UI on Randomize
  observeEvent(input$randomize, {
    V_override(NULL); C_override(NULL)
    updateSelectInput(session, "scenario", selected = "Random")
    import_reset(import_reset() + 1L)
    session$sendCustomMessage("toolbarReset", "import_csv")
    session$onFlushed(function(){
      later::later(function() updating(FALSE), 0)
    }, once = TRUE)
  })
  
  
  s <- function(n) ifelse(n == 1, "", "s") # text explanation pluralization variable
  
  # update maxes + labels when Example Type changes (no feedback loop)
  observeEvent(input$example_type, {
    if (suppress_randomize()) return()
    vm <- voters_max(); cm <- candidates_max()
    tv <- if (is.null(input$total_voters)) 30 else input$total_voters
    cc <- if (is.null(input$candidate_count)) 3 else input$candidate_count
    
    updateNumericInput(session, "total_voters",
                       max   = vm,
                       value = min(max(tv, 1), vm),
                       label = sprintf("Number of voters: (max=%d)", vm)
    )
    updateNumericInput(session, "candidate_count",
                       max   = cm,
                       value = min(max(cc, 2), cm),
                       label = sprintf("Number of candidates: (max=%d)", cm)
    )
  }, ignoreInit = FALSE)
  
  observeEvent(input$example_type, {
    rcv_round(1)
  }, ignoreInit = TRUE)
  
  # for results explanation
  fmt_pct <- function(n, d) if (d > 0) sprintf("%d%%", round(100 * n / d)) else "—"
  safe_get <- function(x, i, default = 0L) if (length(x) >= i) x[i] else default
  
  plurality_outcome <- reactive({
    df <- plurality_summary()
    df <- arrange(df, desc(Votes))
    if (!nrow(df)) return(NULL)
    top_votes <- safe_get(df$Votes, 1)
    runner    <- safe_get(df$Votes, 2)
    list(
      winner   = df$candidate[1],
      top      = top_votes,
      runner   = runner,
      margin   = top_votes - runner,
      total    = input$total_voters,
      tie      = sum(df$Votes == top_votes) > 1,
      ties     = df$candidate[df$Votes == top_votes]
    )
  })
  
  approval_outcome <- reactive({
    df <- approval_summary()
    # turnout excludes "Didn't vote"
    didnt   <- df$value[df$candidate == "Didn't vote"]; if (!length(didnt)) didnt <- 0L
    turnout <- max(0L, input$total_voters - didnt)
    cand    <- df |> filter(candidate != "Didn't vote") |> arrange(desc(value))
    if (!nrow(cand)) return(NULL)
    top <- safe_get(cand$value, 1); runner <- safe_get(cand$value, 2)
    list(
      winner = cand$candidate[1],
      top    = top,
      runner = runner,
      margin = top - runner,
      turnout = turnout,
      tie    = sum(cand$value == top) > 1,
      ties   = cand$candidate[cand$value == top]
    )
  })
  
  score_outcome <- reactive({
    df <- score_table() |> arrange(mean_distance)
    if (!nrow(df)) return(NULL)
    best  <- safe_get(df$mean_distance, 1)
    nextb <- safe_get(df$mean_distance, 2, default = NA_real_)
    list(
      winner    = df$candidate[1],
      best      = best,
      next_best = nextb,
      gap       = if (is.na(nextb)) NA_real_ else (nextb - best),
      tie       = sum(abs(df$mean_distance - best) < 1e-9) > 1,
      ties      = df$candidate[abs(df$mean_distance - best) < 1e-9]
    )
    
  })
  
  borda_outcome <- reactive({
    df <- borda_summary() |> arrange(desc(Points))
    if (!nrow(df)) return(NULL)
    top <- safe_get(df$Points, 1); runner <- safe_get(df$Points, 2)
    list(
      winner = df$candidate[1],
      top    = top,
      runner = runner,
      margin = top - runner,
      tie    = sum(df$Points == top) > 1,
      ties   = df$candidate[df$Points == top]
    )
  })
  
  rcv_outcome <- reactive({
    out <- rcv_out()
    rounds <- out$rounds
    if (!length(rounds)) return(NULL)
    last   <- rounds[[length(rounds)]]
    counts <- last$counts
    active <- last$active
    finalists <- which(active)
    winner_id <- if (is.na(out$winner_index)) NA_integer_ else out$winner_index
    list(
      rounds      = length(rounds),
      finalists   = finalists,
      final_counts= counts[finalists],
      winner_idx  = winner_id,
      tie         = is.na(out$winner_index),
      tie_ids     = if (is.na(out$winner_index)) out$tie_indices else integer(0)
    )
  })
  
  # clamp inputs (manual edits)
  observeEvent(input$total_voters, {
    if (suppress_randomize()) return() 
    vm <- voters_max(); v <- input$total_voters
    if (v > vm)      updateNumericInput(session, "total_voters", value = vm)
    else if (v < 1)  updateNumericInput(session, "total_voters", value = 1)
  }, ignoreInit = TRUE)
  
  observeEvent(input$candidate_count, {
    if (suppress_randomize()) return() 
    cm <- candidates_max(); v <- input$candidate_count
    if (v > cm)      updateNumericInput(session, "candidate_count", value = cm)
    else if (v < 2)  updateNumericInput(session, "candidate_count", value = 2)
  }, ignoreInit = TRUE)
  
  observeEvent(input$example_type, {
    if (suppress_randomize()) return()
    rcv_round(1)
  }, ignoreInit = TRUE)
  
  # ---- downstream reactives ----
  explanation_html <- reactive({
    vs <- input$voting_system
    C  <- candidateData()
    
    if (vs == "plurality") {
      o <- plurality_outcome(); if (is.null(o)) return("")
      if (o$tie) {
        sprintf(
          "<p><b>Who wins according to plurality?</b><br>
       <b>%s tie according to plurality</b>, because while these candidates had the
       largest number of votes, they also had an equal number of votes (%d/%d votes or %s).<br><br>
       <b>Plurality Voting Rules</b>:<br>
       Plurality voting prioritizes selecting a winner who has the single most support. A single candidate is chosen by each voter. 
       A winning candidate in a plurality system needs to have more votes than any other candidate but does not necessarily 
       need to have over 50%% of the votes to win.<br><br></p>",
          and_join(o$ties), o$top, o$total, fmt_pct(o$top, o$total)
        )
      } else {
        sprintf(
          "<p><b>Who wins according to plurality?</b><br>
       <b>%s wins according to plurality</b>, because this candidate had the largest number of votes 
       (%d/%d voters or %s).<br><br>
       <b>Plurality Voting Rules</b>:<br>
       Plurality voting prioritizes selecting a winner who has the single most support. A single candidate is chosen by each voter. 
       A winning candidate in a plurality system needs to have more votes than any other candidate but does not necessarily 
       need to have over 50%% of the votes to win.
          <br><br></p>",
          o$winner, o$top, o$total, fmt_pct(o$top, o$total)
        )
      }
      
    } else if (vs == "approval") {
      o <- approval_outcome(); if (is.null(o)) return("")
      if (o$turnout == 0) {
        return(sprintf(
          "<p><b>Who wins according to approval voting?</b><br>
       <b>No approvals were cast.</b> At the current threshold (%s), no voter approved any candidate,
       so every candidate has 0 approvals and the method cannot select a winner.<br><br>
       <b>What to try:</b> Increase the approval distance threshold or compare with other methods that
       don’t rely on approval cutoffs.<br><br></p>",
          approval_thr()
        ))
      }
      
      if (o$tie) {
        sprintf(
          "<p><b>Who wins according to approval voting?</b><br>
       <b>%s</b> tie according to approval voting, because while these candidates had the largest number of votes of approval,
       they also had an equal number of votes (%d/%d votes or %s).<br><br>
       <b>Approval Voting Rules</b>:<br>
       Approval voting prioritizes selecting a candidate based on who voters most approve of. In contrast to plurality voting, 
       voters can choose many candidates, essentially rating each candidate with a 1 (approve) or 0 (do not approve). 
       A winning candidate is the one with the most votes of approval and does not 
       need to have over 50%% of the votes to win.
       <br><br></p>",
          paste(o$ties, collapse = " and "), o$top, o$turnout, fmt_pct(o$top, o$turnout)
        )
      } else {
        sprintf(
          "<p><b>Who wins according to approval voting?</b><br>
       <b>%s wins according to approval voting</b>, because this candidate had the largest number of votes 
       (%d/%d votes or %s).<br><br>
       <b>Approval Voting Rules</b>:<br>
       Approval voting prioritizes selecting a candidate based on who voters most approve of. In contrast to plurality voting, 
       voters can choose many candidates, essentially rating each candidate with a 1 (approve) or 0 (do not approve). 
       A winning candidate is the one with the most votes of approval and does not 
       need to have over 50%% of the votes to win.
       <br><br></p>",
          o$winner, o$top, o$turnout, fmt_pct(o$top, o$turnout)
        )
      }
      
    } else if (vs == "score") { # zc
      o <- score_outcome(); if (is.null(o)) return("")
      if (o$tie) {
        sprintf(
          "<p><b>Who wins according to Cardinal (Score)?</b><br>
      <b>%s tie according to Cardinal (Score) voting, because these candidates are both equally close to the 
      voters with an average distance %.1f.<br><br>
      <b>Cardinal (Score) Voting Rules:</b><br>
      In Cardinal (Score) voting the candidate that is closest on average to the voters wins. 
      Each candidate receives a score from every voter, which is averaged into a single score for each candidate. 
      In this setup, distance is used as the score each candidate receives from every voter. <br><br></p>",
          paste(o$ties, collapse = " and "), o$best
        )
      } else {
        sprintf(
          "<p><b>Who wins according to Cardinal (Score)?</b><br>
      <b>%s wins according to Cardinal (Score) voting</b>, because this candidate is closest to the voters 
      with an average distance of %.1f.<br><br>
      <b>Cardinal (Score) Voting Rules:</b><br>
      In Cardinal (Score) voting the candidate that is closest on average to the voters wins. 
      Each candidate receives a score from every voter, which is averaged into a single score for each candidate. 
      In this setup, distance is used as the score each candidate receives from every voter. <br><br></p>",
          o$winner, o$best
        )
      }
    } else if (vs == "borda") {
      o <- borda_outcome(); if (is.null(o)) return("")
      if (o$tie) {
        sprintf("<p><b>Who wins according to Borda Count voting?</b><br>
                <b>%s</b> tie according to Borda count voting, because while these candidates had the 
                largest number of points, they also had an equal number of points (<b>%d</b> points).<br><br>
                <b>Borda Count Voting Rules:</b><br>
                Borda Count turns each voter’s rankings of candidates into points. The first choice receives the most points, 
                the second choice one fewer, and so on, down to zero for the last choice. After all ballots are tallied, 
                the candidate with the highest total points wins. This methods rewards broadly liked candidates, 
                even if they do not receive the most first-choice votes.    
                <br><br></p>",
                paste(o$ties, collapse = " and "), o$top)
      } else {
        sprintf("<p><b>Who wins according to Borda Count voting?</b><br> 
                <b>%s</b> wins according to Borda count voting, because this candidate had the largest number of 
                points (<b>%d</b> points).<br><br>
                <b>Borda Count Voting Rules:</b><br>
                Borda Count turns each voter’s rankings of candidates into points. The first choice receives the most points, 
                the second choice one fewer, and so on, down to zero for the last choice. After all ballots are tallied, 
                the candidate with the highest total points wins. This methods rewards broadly liked candidates, 
                even if they do not receive the most first-choice votes. 
                <br><br></p>",
                o$winner, o$top)
      }
    } else if (vs == "ranked_choice") { # zc
      o <- rcv_outcome(); if (is.null(o)) return("")
      if (o$tie) {
        ids <- C$id[o$tie_ids]
        # tied candidates' final-round tallies (equal in a final-round tie)
        final_ids <- C$id[o$finalists]
        tie_votes <- o$final_counts[match(ids[1], final_ids)]
        if (is.na(tie_votes)) tie_votes <- o$final_counts[1L]  # safety
        
        sprintf("<p><b>Who wins according to ranked-choice voting?</b><br>
            <b>%s tie according to ranked-choice voting</b>, 
            after %d round%s with %d/%d voters or %s each.<br><br>
            <b>Ranked-Choice Voting Rules</b>:<br>
            Ranked-Choice voting prioritizes majority approval and a winning candidate must have 
            over 50%% of the total votes. This works by first counting all voters' first preference 
            and seeing if any candidate has over 50%% support. If so, this candidate wins. If not, 
            then the candidate with the least amount of support is “eliminated” and their votes are 
            re-assigned to their next favorite candidate. The votes are then counted again to see if 
            a candidate has over 50%% support. If so, this candidate wins. If not, the process is repeated.<br><br>
            Note: If, during an elimination round, there are two candidates who equally have the least amount of support (measured by vote counts), 
            the candidate with the lowest Borda count points is eliminated (reflecting the candidate that is ranked lowest by the voters).
            <br><br></p>",
                paste(ids, collapse = " and "), o$rounds, s(o$rounds), tie_votes, input$total_voters, fmt_pct(tie_votes, input$total_voters))
      }else {
        winner <- C$id[o$winner_idx]
        # winner's final-round tally
        final_ids <- C$id[o$finalists]
        w_final   <- o$final_counts[match(winner, final_ids)]
        
        sprintf("<p><b>Who wins according to ranked-choice voting?</b><br>
            <b>%s wins according to ranked-choice voting</b>, 
            after %d round%s with %d/%d voters or %s.<br><br>
            <b>Ranked-Choice Voting Rules</b>:<br>
            Ranked-Choice voting prioritizes majority approval and a winning candidate must have 
            over 50%% of the total votes. This works by first counting all voters' first preference 
            and seeing if any candidate has over 50%% support. If so, this candidate wins. If not, 
            then the candidate with the least amount of support is “eliminated” and their votes are 
            re-assigned to their next favorite candidate. The votes are then counted again to see if 
            a candidate has over 50%% support. If so, this candidate wins. If not, the process is repeated.<br><br>
            Note: If, during an elimination round, there are two candidates who equally have the least amount of support (measured by vote counts), 
            the candidate with the lowest Borda count points is eliminated (reflecting the candidate that is ranked lowest by the voters).
            <br><br></p>",
                winner, o$rounds, s(o$rounds), w_final, input$total_voters, fmt_pct(w_final, input$total_voters))
      }
    }
    else {
      ""
    }
  })
  
  output$explanation <- renderUI({
    HTML(explanation_html())
  })
  
  output$criteria_help <- renderUI({
    HTML(paste0(
      "<h4>What these criteria mean</h4>",
      
      "<p><b>Majority Winner</b> — If only one candidate is ranked 1st by more than 50% of voters, ",
      "that candidate must win. ",
      "<i>Assessment:</i> Pass if the method elects that candidate; Fail otherwise; N/A if no candidate has over 50% of the votes",
      " (Note: A tie for most first-place votes does not count as a majority winner.)</p>",
      
      "<p><b>Majority Loser</b> — If only one candidate is ranked last by more than 50% of voters, ",
      "that candidate must not win. ",
      "<i>Assessment:</i> Pass if the method does not elect that candidate; Fail if it does; N/A if no such candidate exists.</p>",
      
      "<p><b>Plurality Leader</b> — Every winning candidate must be among the candidate(s) with the most first-choice votes. ",
      "<i>Assessment:</i> Pass if all winners are in the plurality-leader set; Fail if any winner is outside it.</p>",
      
      "<p><b>Mutual Majority (MMC)</b> — If a majority of voters ranks every member of some group of candidates above every non-member, ",
      "the winner must come from that group. ",
      "<i>Assessment:</i> Pass if all winners are inside at least one such majority-preferred group; Fail if any winner is outside; N/A if no such group exists in this profile.</p>",
      
      "<p><b>Condorcet Winner</b> — If only one candidate beats every other candidate in head-to-head majority matchups, ",
      "that candidate must win. ",
      "<i>Assessment:</i> Pass if the method elects that candidate; Fail otherwise; N/A if no unique Condorcet winner exists (including any pairwise ties).</p>",
      
      "<p><b>Independence of Irrelevant Alternatives (IIA)</b> — Removing any non-winning candidate should not change who wins ",
      "among the remaining candidates. ",
      "<i>Assessment:</i> Pass if the winner set over the remaining options never changes; Fail if it changes even once.</p>"
    ))
  })
  
  # A few example presets. Add as many as you want.
  scenario_bank <- list(
    "Random" = NULL,  # special key
    "Center Squeeze (1D)" = list(
      type = "1d",
      V = tibble(x = c(
        # example 2
        -95, -85, -75, -69, -58, -47, -39, -30, # liberal > centrist > conservative
        -7, # centrist > liberal > conservative
        -1, 4, 8, 16, 20, # centrist > conservative > liberal
        40, 55, 65, 75, 80, 90, 95), # conservative > centrist > liberal
        
        # example 1
        # -95, -90, -85, -82, -75, -72, -66, -60, # liberal > centrist > conservative
        # -7, # centrist > liberal > conservative
        # -1, 4, 8, 16, 20, # centrist > conservative > liberal
        # 55, 60, 65, 75, 80, 90, 95), # conservative > centrist > liberal
        y = 0),
      C = tibble(id = c("A","B","C"), x = c(-70, 11, 60), y = 0)
    ),
    "Spoiler Effect (1D)" = list(
      type = "1d",
      V = tibble(x = c(
        -88, -82, -76, -68, -60, -55, # liberal > centrist > conservative
        -13, # centrist > liberal > conservative
        -3, 5, 18, 30, # centrist > conservative > liberal
        45, 50), # conservative > centrist > liberal
        y = 0),
      C = tibble(id = c("A","B","C"), x = c(-80, -20, 64), y = 0)
    ),
    "Mutual Majority Criterion (1D)" = list(
      type = "1d",
      V = tibble(x = c(
        93, 87, 81, 75, 70, 66, 63, 57,  # conservative > centrist > liberal
        15, # centrist > conservative > liberal
        -96, -91,  -83,  -77,  -72, -67, -56),  # liberal > centrist > conservative
        y = 0),
      C = tibble(id = c("A","B","C"), x = c(-75, 20, 75), y = 0)
    ),
    # Simple 2D cycle-ish demo (tweak as desired)
    "Condorcet Cycle (2D)" = list(
      type = "2d",
      V = tibble(
        x = c(
          -28,-30,-35,-29,-23,-26,-27,-18, # A > B > C 
          65,70,66,55,85,58, # B > C > A
          22,25,16,23,20), # C > A > B
        y = c(
          14,16,12,20,8,13,10,17, # A > B > C 
          5,10,-2,8,15,-5, # B > C > A
          -55,-45,-48,-58,-49 # C > A > B
        )
      ),
      C = tibble(id = c("A","B","C"),
                 x = c(-20, 60, 38),
                 y = c(0, 10, -52))
    ),
    "Clone Penalty (1D)" = list(
      type = "1d",
      V = tibble(
        x = c(
          # # Left bloc (majority): A≈D > B > C  — 20 voters
          # -97,-94,-92,-90,-88,-86,-84,-82,-80,-78,-76,-74,-72,-70,-68,-66,-64,-62,-60,-58, -56,
          # # Center: B > A≈D > C  — 13 voters (bumped up)
          # -12,-8,-6,-2, 2, 6, 10, 14, 18, 20, 22, 24, 26,
          # # Right: C > B > A≈D  — 7 voters
          # 58, 66, 72, 78, 82, 86, 90
          # Left bloc (majority): prefers A≈D > B > C — same voters
          -99, -96, -93, -90, -87, -84, -80, -76, -72, -68, -64, -60, -56, -52, -48, -44,
          # Center voters: B > A≈D > C — same voters
          -9, -5, -2, 2, 5, 8, 12, 16, 20, 25, 29, 33,
          # Right voters: C > B > A≈D — same voters
          65, 72, 79
          
          # # Left bloc (majority): prefers A≈D > B > C — same voters
          # -94, -92, -88, -84, -80, -76, -72, -68, -64, -60, -56, -52, -48, -44, -40, -35,
          # # Center voters: B > A≈D > C — same voters
          # -9, -5, -2, 2, 5, 8, 12, 16, 20, 25, 29, 
          # # Right voters: C > B > A≈D — same voters
          # 60, 70, 80, 88
        ),
        y = 0
      ),
      C = tibble(
        id = c("A","B","C","D"),
        x  = c(-71, -67, 4, 70),# -70, 0, 70, -60),  # A and D are "clones" on the left
        y  = 0)
    )
  )
  
  scenario_descriptions <- list(
    "Center Squeeze (1D)" = HTML("
    <p><b>Center Squeeze:</b> Moderate or middle-of-the-road candidates can lose to extreme candidates within plurality and
    ranked-choice  voting because they are “squeezed” by candidates on either side of them taking away voters.
    <br><br>
    In this scenario, there is a fairly good middle-of-the-road candidate, 
    but ranked-choice and plurality voting instead choose extreme candidates. Think about it this way - 
    imagine that the voters were the same but instead of having three candidates there were only two. 
    If this was just an election with A vs B, B would win. 
    If this was just an election of B vs C, B would win. If there was an election of A vs C, C would win. 
    So in the head-to-head comparisons B wins two of the three elections.
    </p>
  "),
    "Spoiler Effect (1D)" = HTML("
    <p><b>Spoiler Effect:</b> In plurality voting, elections can be dramatically affected by <i>spoiler candidates</i> 
    who win only a small number of votes.
    <br><br>
    In this scenario, candidate C has minimal support, but their presence in the election makes candidate A the winner. 
    If they were not in the election, candidate B would win. Note that in the results table the other voting systems select B
    as the winner and candidate A only wins under plurality voting.</p>
  "),
    "Mutual Majority Criterion (1D)" = HTML("
    <p><b>Mutual Majority:</b> In certain situations cardinal voting can lead to winners that very few voters would choose
    as their first pick.
    <br><br>
    In this scenario, B won according to cardinal, but only one voter was close to B. Additionally, candidate C lost according 
    to cardinal voting but many more voters are close to C than B
    </p>
  "),
    "Condorcet Cycle (2D)" = HTML("
    <p><b>Condorcet Cycle:</b> One way to assess who is the best candidate is to examine head-to-head comparisons for each 
    candidate pairing. However, in some situations a rock-paper-scissors-like loop can occur, where 
    no single candidate beats all others head-to-head.
    <br><br>
    In this scenario, A wins in a A vs B comparison (13 to 6), B wins in a B vs C comparison (14 to 6), and 
    C wins in a C vs A comparison (11 to 8). 
    </p>
  "),
    "Clone Penalty (1D)" = HTML("
    <p><b>Clone Penalty:</b> In situations where two candidates are too similar to one another, they can split  
    voters support and can hand victory to a less-popular alternative.
    <br><br>
    In this scenario, candidates A and B are splitting a group of voters' support, which leads to candidate C's victory in 
    plurality, cardinal, and Borda Count voting, but not ranked-choice voting. Note that if either A or B were not present, 
    the other candidate would have 16 votes and would win in a plurality voting system.
    </p>
  ")
  )
  
  
  observeEvent(input$scenario, {
    nm <- input$scenario
    sc <- scenario_bank[[nm]]
    
    if (is.null(sc) || identical(nm, "Random")) {
      # back to random mode (no overrides)
      V_override(NULL); C_override(NULL)
      suppress_randomize(FALSE)        # <-- release the guard here
      updating(FALSE)                  # (optional) make sure we're not in “updating” mode
      rcv_round(1)                     # (optional) reset RCV navigator
      bump_random()
      session$sendCustomMessage("toolbarReset", "import_csv")
      return()
    }
    
    updating(TRUE)
    suppress_randomize(TRUE)
    
    freezeReactiveValue(input, "example_type")
    freezeReactiveValue(input, "total_voters")
    freezeReactiveValue(input, "candidate_count")
    
    # Apply the scenario overrides
    V_override(sc$V)
    C_override(sc$C)
    
    # Use the scenario’s declared type (1d/2d) to choose the UI “Example Type”
    target_type <- if (!is.null(sc$type) && sc$type == "1d") "1-dimension" else "2-dimension"
    tv <- nrow(sc$V)
    cc <- nrow(sc$C)
    
    updateSelectInput(session, "example_type", selected = target_type)
    updateNumericInput(session, "total_voters",   value = tv)
    updateNumericInput(session, "candidate_count", value = cc)
    
    release_guard_when(session, function(){
      isTRUE(all.equal(input$example_type,  target_type)) &&
        isTRUE(all.equal(input$total_voters,  tv)) &&
        isTRUE(all.equal(input$candidate_count, cc))
    })
    
    # Clear any import status
    session$sendCustomMessage("toolbarReset", "import_csv")
    session$onFlushed(function(){
      later::later(function() updating(FALSE), 0)
    }, once = TRUE)
  })
  
  # ---- voting-criteria helpers ----

  # plurality leaders (indices of candidates with the most 1st-choice votes)
  plurality_leader_idx <- function(rm) {
    K <- ncol(rm)
    counts <- tabulate(rm[,1], nbins = K)
    which(counts == max(counts))
  }
  
  # Pass if every method winner is among the plurality leaders.
  # If no winners (shouldn't happen) -> "N/A".
  assess_plurality_respect <- function(method_winners, pl_idx, Cids) {
    if (!length(method_winners)) return("N/A")
    pl_ids <- Cids[pl_idx]
    if (length(pl_ids) == 0L) return("N/A")             # degenerate
    if (all(method_winners %in% pl_ids)) "Pass" else "Fail"
  }
  
  # Mutual Majority Criterion: if some set S is ranked above all others
  # by >50% of voters, the winner must be in S. If such S exists and the
  # winner is outside S -> Fail; if no such S -> N/A; otherwise Pass.
  assess_mutual_majority <- function(method_winners, rm, Cids) {
    N <- nrow(rm); K <- ncol(rm)
    if (!length(method_winners)) return("N/A")
    
    # position matrix: pos[i,c] = rank position (1 = top)
    pos <- matrix(0L, N, K)
    for (i in seq_len(N)) pos[i, rm[i,]] <- seq_len(K)
    
    # search all non-empty proper subsets S
    has_mm <- FALSE
    for (m in 1:(K - 1L)) {
      combs <- combn(K, m, simplify = FALSE)
      for (S in combs) {
        Scomp <- setdiff(seq_len(K), S)
        # voter i counts if max rank of S < min rank of outside S
        voters <- vapply(seq_len(N), function(i) {
          max(pos[i, S]) < min(pos[i, Scomp])
        }, logical(1))
        if (sum(voters) > N/2) {
          has_mm <- TRUE
          S_ids <- Cids[S]
          if (!all(method_winners %in% S_ids)) return("Fail")
        }
      }
    }
    if (has_mm) "Pass" else "N/A"
  }
  
  # Majority Winner: a candidate ranked 1st by > 50% of voters
  majority_winner_idx <- function(rm) {
    N <- nrow(rm); K <- ncol(rm)
    counts <- tabulate(rm[,1], nbins = K)
    idx <- which(counts > N/2)
    if (length(idx) == 1L) idx else integer(0)
  }
  
  # Majority Loser: a candidate ranked last by > 50% of voters
  majority_loser_idx <- function(rm) {
    N <- nrow(rm); K <- ncol(rm)
    last <- rm[, K]
    counts <- tabulate(last, nbins = K)
    idx <- which(counts > N/2)
    if (length(idx) == 1L) idx else integer(0)
  }
  
  # Pairwise matrix (# voters pref i over j) and Condorcet winner index (if any)
  pairwise_wins <- function(rm) {
    N <- nrow(rm); K <- ncol(rm)
    pos <- matrix(0L, N, K)
    for (i in seq_len(N)) pos[i, rm[i,]] <- seq_len(K)
    W <- matrix(0L, K, K)
    for (i in seq_len(K)) for (j in seq_len(K)) if (i != j) W[i,j] <- sum(pos[,i] < pos[,j])
    W
  }
  condorcet_winner_idx <- function(rm) {
    N <- nrow(rm); K <- ncol(rm)
    W <- pairwise_wins(rm)
    idx <- which(rowSums(W > N/2) == (K - 1L))
    if (length(idx) == 1L) idx else integer(0)
  }
  
  # Compute winners (as IDs) for each method on a given profile
  winners_for <- function(method, D, rm, ids, thr){
    K <- ncol(rm)
    if (method == "plurality") {
      counts <- tabulate(rm[,1], nbins = K)
      ids[counts == max(counts)]
    } else if (method == "ranked_choice") {
      out <- rcv_irv(rm)
      if (is.na(out$winner_index)) ids[out$tie_indices] else ids[out$winner_index]
    } else if (method == "approval") {
      approvals <- colSums(D <= thr)
      ids[approvals == max(approvals)]
    } else if (method == "score") {
      md <- colMeans(D)
      ids[abs(md - min(md)) < 1e-12]
    } else if (method == "borda") {
      # reuse Borda logic
      pos <- matrix(0L, nrow = nrow(rm), ncol = K)
      for (i in seq_len(nrow(rm))) pos[i, rm[i,]] <- seq_len(K)
      pts <- colSums(K - pos)
      ids[pts == max(pts)]
    } else {
      character(0)
    }
  }
  
  # Remove a candidate (by index) from D and rm
  remove_candidate <- function(D, rm, rem_idx){
    D2 <- D[, -rem_idx, drop = FALSE]
    K  <- ncol(rm)
    rm2 <- t(vapply(seq_len(nrow(rm)), function(i){
      row <- rm[i, ]
      row <- row[row != rem_idx]
      # reindex: shift down anything above removed index
      row[row > rem_idx] <- row[row > rem_idx] - 1L
      as.integer(row)
    }, integer(K - 1L)))
    list(D = D2, rm = rm2)
  }
  
  # Empirical IIA probe: does removing any non-winner change the winner set?
  check_iia <- function(method, D, rm, ids, thr){
    orig <- winners_for(method, D, rm, ids, thr)
    if (!length(orig)) return("N/A")
    orig_idx <- match(orig, ids)
    losers   <- setdiff(seq_along(ids), orig_idx)
    if (!length(losers)) return("Pass")  # nothing irrelevant to remove
    
    for (rem in losers){
      pr <- remove_candidate(D, rm, rem)
      ids2 <- ids[-rem]
      new  <- winners_for(method, pr$D, pr$rm, ids2, thr)
      expected <- setdiff(orig, ids[rem])  # original winners among remaining
      # If the winner set over the remaining options changes, flag a violation
      if (!setequal(new, expected)) return("Fail")
    }
    "Pass"
  }
  
  
  # Build the criteria matrix for the current scenario
  criteria_df <- reactive({
    ids <- candidateData()$id
    D   <- dist_matrix()
    rm  <- rank_matrix()
    thr <- as.numeric(approval_thr())
    N   <- nrow(rm)
    
    # criterion “targets” from the current profile
    mw  <- majority_winner_idx(rm)   # may be NA
    ml  <- majority_loser_idx(rm)    # may be NA
    cw  <- condorcet_winner_idx(rm)  # may be NA
    pl  <- plurality_leader_idx(rm) 
    
    systems <- c("Plurality" = "plurality",
                 "Ranked-Choice" = "ranked_choice",
                 "Approval" = "approval",
                 "Cardinal (Score)" = "score",
                 "Borda Count" = "borda")
    
    rows <- lapply(names(systems), function(label){
      code <- systems[[label]]
      w    <- winners_for(code, D, rm, ids, thr)
      
      # Majority Winner (must elect the unique majority favorite)
      mw_cell <- if (length(mw) == 0L) "N/A" else if (length(w) == 1L && w == ids[mw]) "Pass" else "Fail"
      
      # Majority Loser (must not elect the majority last-place)
      ml_cell <- if (length(ml) == 0L) "N/A" else if (ids[ml] %in% w) "Fail" else "Pass"
      
      # Condorcet Winner (must elect the unique CW; ties count as Fail)
      cw_cell <- if (length(cw) == 0L) "N/A" else if (length(w) == 1L && w == ids[cw]) "Pass" else "Fail"
      
      # plurality leader
      pl_cell <- assess_plurality_respect(w, pl, ids)
      
      # mutual marjoity
      mm_cell <- assess_mutual_majority(w, rm, ids)
      
      # IIA (empirical removal test)
      iia_cell <- check_iia(code, D, rm, ids, thr)
      
      tibble(`Voting system` = label,
             `Majority Winner` = mw_cell,
             `Majority Loser`  = ml_cell,
             `Plurality Leader`  = pl_cell,
             `Mutual Majority`  = mm_cell,
             `Condorcet Winner`= cw_cell,
             `IIA`             = iia_cell)
    })
    bind_rows(rows)
  })
  
  output$criteria_dt <- renderDT({
    df  <- criteria_df()
    pal <- c("Pass" = "#1a7f37", "Fail" = "#d1242f", "N/A" = "#6e7781")
    
    # all columns except the first (“Voting system”)
    crit_cols <- setdiff(names(df), "Voting system")
    crit_idx0 <- match(crit_cols, names(df)) - 1L   # DT uses 0-based indices
    
    dt <- datatable(
      df,
      class   = "display nowrap compact",
      options = list(
        dom = "t", paging = FALSE, autoWidth = TRUE,
        columnDefs = list(list(className = "dt-center", targets = crit_idx0))
      ),
      rownames = FALSE
    )
    
    # color each criteria column
    for (col in crit_cols) {
      dt <- dt %>% formatStyle(
        col,
        backgroundColor = styleEqual(names(pal), unname(pal)),
        color = "white"
      )
    }
    dt
  })
  
  voterData <- eventReactive(
    list(input$randomize, rand_bump(), input$total_voters, input$example_type, V_override()),
    {
      vo <- V_override()
      if (!is.null(vo)) return(vo)
      
      if (identical(input$example_type, "1-dimension")) {
        tibble(
          x = sample_min_gap_int(input$total_voters, lo = -100, hi = 100, min_gap = 2),
          y = rep(0, input$total_voters)
        )
      } else {
        tibble(
          x = rand_dimension_voters(input$total_voters),
          y = rand_dimension_voters(input$total_voters)
        )
      }
    },
    ignoreInit = FALSE
  )
  
  # dynamic checkbox UI – defaults to "all candidates selected"
  output$candidate_filter_ui <- renderUI({
    ids <- candidate_ids()
    checkboxGroupInput(
      "candidate_filter",
      label = "Candidates included:",
      choices = ids,
      selected = ids,           # start with all ON
      inline   = TRUE
    )
  })
  
  # whenever the candidate set changes (randomize, scenario/import),
  # reset the checkboxes to "all selected"
  observeEvent(candidate_ids(), {
    ids <- candidate_ids()
    if (!is.null(ids) && length(ids)) {
      updateCheckboxGroupInput(session, "candidate_filter",
                               choices  = ids, selected = ids)
    }
  }, ignoreInit = FALSE)
  
  candidate_ids <- reactive(LETTERS[seq_len(input$candidate_count)])
  
  candidateData <- eventReactive(
    list(input$randomize, rand_bump(), input$candidate_count, input$example_type, C_override()),
    {
      co <- C_override()
      if (!is.null(co)) return(co)
      
      if (identical(input$example_type, "1-dimension")) {
        tibble(
          x  = sample_min_gap_int(input$candidate_count, lo = -98, hi = 98, min_gap = 2),
          y  = rep(0, input$candidate_count),
          id = candidate_ids()
        )
      } else {
        tibble(
          x  = rand_dimension_candidates(input$candidate_count),
          y  = rand_dimension_candidates(input$candidate_count),
          id = candidate_ids()
        )
      }
    },
    ignoreInit = FALSE
  )
  
  # keep_ids are candidate *letters* (e.g., c("A","C"))
  subset_profile <- function(D, rm, all_ids, keep_ids) {
    keep_ids <- intersect(keep_ids, all_ids)
    keep_idx <- match(keep_ids, all_ids)
    
    # distances: keep selected columns (already stable when K′ = 1)
    D2 <- D[, keep_idx, drop = FALSE]
    
    # map old candidate indices -> new 1..K′ indices
    idx_map <- setNames(seq_along(keep_idx), keep_idx)  # "old col idx" -> "new col idx"
    
    # build an N × K′ matrix robustly, even when K′ = 1
    rm2_rows <- lapply(seq_len(nrow(rm)), function(i) {
      row <- rm[i, ]
      row <- row[row %in% keep_idx]                 # drop unchecked
      as.integer(idx_map[as.character(row)])        # remap to 1..K′
    })
    rm2 <- do.call(rbind, rm2_rows)                 # N × K′, never 1 × N
    storage.mode(rm2) <- "integer"
    
    list(D = D2, rm = rm2, ids = keep_ids)
  }
  
  # which candidates are currently "in"?
  included_ids <- reactive({
    ids <- candidate_ids()
    if (!isTRUE(input$enable_filter)) return(ids)
    sel <- input$candidate_filter
    if (is.null(sel) || !length(sel)) return(ids)  # if user empties it, fall back to all
    intersect(sel, ids)
  })
  
  dist_matrix <- reactive({
    V <- voterData(); C <- candidateData()
    sqrt(outer(V$x, C$x, `-`)^2 + outer(V$y, C$y, `-`)^2)
  })
  
  # filtered candidate tibble
  candidateData_current <- reactive({
    C <- candidateData()
    keep <- included_ids()
    C %>% dplyr::filter(id %in% keep) %>% dplyr::arrange(match(id, keep))
  })
  
  # filtered distances/ranks
  dist_rank_current <- reactive({
    D  <- dist_matrix()
    rm <- rank_matrix()
    ids <- candidateData()$id
    subset_profile(D, rm, ids, included_ids())
  })
  
  # convenience helpers (use these below)
  rank_matrix_current <- reactive(dist_rank_current()$rm)
  dist_matrix_current <- reactive(dist_rank_current()$D)
  candidate_ids_current <- reactive(dist_rank_current()$ids)
  first_choice_idx_current <- reactive(rank_matrix_current()[,1])
  pref1_current <- reactive(candidate_ids_current()[first_choice_idx_current()])
  
  rank_matrix      <- reactive(rank_by_distance(dist_matrix()))
  first_choice_idx <- reactive(rank_matrix()[,1])
  pref1            <- reactive(candidateData()$id[first_choice_idx()])
  
  plurality_summary <- reactive({
    tibble(candidate = pref1_current()) |>
      count(candidate, name = "Votes") |>
      complete(candidate = candidate_ids_current(), fill = list(Votes = 0)) |>
      arrange(desc(Votes))
  })
  
  score_table <- reactive({
    C <- candidateData_current(); md <- colMeans(dist_matrix_current())
    tibble(candidate = C$id, mean_distance = md) |> arrange(mean_distance)
  })
  
  approval_summary <- reactive({
    C <- candidateData_current(); D <- dist_matrix_current(); thr <- as.numeric(approval_thr())
    approvals <- colSums(D <= thr); didnt <- sum(rowSums(D <= thr) == 0)
    tibble(candidate = c(C$id, "Didn't vote"),
           value = c(approvals, didnt)) |> arrange(desc(value))
  })
  
  borda_summary <- reactive({
    rm <- rank_matrix_current()
    K  <- ncol(rm); N <- nrow(rm)
    
    pos_matrix <- matrix(0L, nrow = N, ncol = K)
    for (i in seq_len(N)) pos_matrix[i, rm[i, ]] <- seq_len(K)
    
    points <- colSums(K - pos_matrix)
    
    tibble(candidate = candidate_ids_current(),  # <- was candidate_ids_current()$id
           Points    = as.integer(points)) |>
      arrange(desc(Points))
  })
  
  rcv_out <- reactive({
    rm <- rank_matrix_current()
    rcv_irv(rm)
  })
  
  rcv_round <- reactiveVal(1)
  observeEvent(list(input$randomize, input$total_voters, input$candidate_count), { rcv_round(1) }, ignoreInit = FALSE)
  total_rounds <- reactive(length(rcv_out()$rounds))
  observeEvent(input$next_round, { rcv_round(min(rcv_round() + 1, total_rounds())) })
  observeEvent(input$prev_round, { rcv_round(max(rcv_round() - 1, 1)) })
  output$rcv_round_label <- renderText({
    out <- rcv_out()
    r   <- safe_round_index(out, rcv_round())
    paste0("Round ", r, " of ", length(out$rounds))
  })
  
  
  # ---------- maps ----------
  map_1d <- reactive({
    V <- voterData(); C <- candidateData_current()
    make_1d_strip(V, C, top_choice_ids = pref1_current(), active_mask = rep(TRUE, nrow(C)))
  })
  
  map_1d_approval <- reactive({
    V <- voterData()
    C <- candidateData_current()
    thr <- as.numeric(approval_thr())
    
    # which voters approve at least one candidate (for fading)
    D <- abs(outer(V$x, C$x, `-`))          # 1-D distance
    inside_any <- apply(D <= thr, 1, any)
    
    make_1d_strip_approval(
      V = V,
      C = C,
      thr = thr,
      top_choice_ids = pref1_current(),
      inside_any = inside_any
    )
  })
  
  
  map_1d_rcv <- reactive({
    out  <- rcv_out()
    r    <- rcv_round()
    snap <- out$rounds[[r]]
    V  <- voterData(); C <- candidateData_current(); rm <- rank_matrix_current()
    dest_idx <- top_choice_given_active(rm, snap$active)
    top_ids  <- C$id[dest_idx]
    make_1d_strip(V, C, top_choice_ids = top_ids, active_mask = snap$active)
  })
  
  map_plot_generic <- function(current_choice_ids, C = candidateData(), active_mask = NULL, rcv_mode = FALSE) {
    V <- voterData()
    pal <- setNames(candidate_palette[seq_len(length(candidate_ids_current()))],
                    candidate_ids_current())
    df_c <- C; df_c$alpha <- 1
    if (rcv_mode && !is.null(active_mask)) df_c$alpha <- ifelse(active_mask, 1, 0.25)
    
    x_breaks <- c(-100, -50, 0, 50, 100)
    x_labels <- c("-100", "-50", "0", "50", "")
    y_min    <- -100; off <- 6
    
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
            plot.margin = margin(t = 5, r = 10, b = 28, l = 32)) +
      labs(title = "Voter & Candidate Positions",
           x="Economic Scale", y="Social Scale") +
      annotate("text", x = 100, y = y_min - off, label = "100",
               hjust = 1, vjust = -.5, size = 3)
    
    add_pole_labels(p)
  }
  
  map_default  <- reactive({ map_plot_generic(pref1()) })
  map_rcv <- reactive({
    out <- rcv_out()
    r   <- safe_round_index(out, rcv_round())
    snap <- out$rounds[[r]]
    
    V  <- voterData()
    C  <- candidateData_current()
    rm <- rank_matrix_current()
    
    dest_idx <- top_choice_given_active(rm, snap$active)
    top_ids  <- C$id[dest_idx]
    
    map_plot_generic(top_ids, C = C, active_mask = snap$active, rcv_mode = TRUE)
  })
  
  map_approval <- reactive({
    V   <- voterData()
    C   <- candidateData_current()        # <- filtered candidates
    D   <- dist_matrix_current()          # <- filtered distances
    thr <- as.numeric(approval_thr())
    
    cur_ids <- candidate_ids_current()
    pal <- setNames(candidate_palette[seq_len(length(cur_ids))], cur_ids)
    
    # which voters approve at least one *current* candidate
    inside_any <- if (ncol(D)) apply(D <= thr, 1, any) else rep(FALSE, nrow(V))
    
    # color by nearest among *current* candidates
    V$color_id <- pref1_current()
    V$alpha    <- ifelse(inside_any, 0.9, 0.3)
    
    p <- ggplot() +
      geom_point(data = V, aes(x = x, y = y, color = factor(color_id), alpha = alpha), size = 1.8) +
      geom_text (data = C, aes(x = x, y = y, label = id, color = id), fontface = 2, size = 6, show.legend = FALSE) +
      ggforce::geom_circle(data = C, aes(x0 = x, y0 = y, r = thr, color = id), alpha = 0.25, inherit.aes = FALSE) +
      scale_color_manual(values = pal, guide = "none") +
      scale_alpha_identity() +
      coord_fixed(xlim = c(-100, 100), ylim = c(-100, 100), expand = FALSE, clip = "on") +
      scale_x_continuous(expand = expansion(mult = c(0.02, 0.02))) +
      scale_y_continuous(expand = expansion(mult = c(0.02, 0.02))) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            aspect.ratio = 1,
            plot.margin = margin(t = 5, r = 10, b = 28, l = 32)) +
      labs(title = "Voter & Candidate Positions",
           x = "Economic Scale", y = "Social Scale")
    
    add_pole_labels_outside(p)
  })
  
  # ---------- RCV composition helper ----------
  rcv_composition_round <- function(round_index) {
    out <- rcv_out()
    rm  <- rank_matrix_current()       # <- filtered
    C   <- candidateData_current()     # <- filtered
    
    src_idx <- rm[, 1]
    snap    <- out$rounds[[round_index]]
    active  <- snap$active             # length == nrow(C)
    
    dest_idx <- top_choice_given_active(rm, active)
    
    tibble(src = C$id[src_idx], dest = C$id[dest_idx]) |>
      filter(!is.na(dest)) |>
      count(dest, src, name = "count") |>
      complete(dest = C$id[active], src = C$id, fill = list(count = 0))
  }
  
  # ---------- results bars ----------
  bars_plurality <- reactive({
    pal <- setNames(candidate_palette[seq_len(length(candidate_ids_current()))],
                    candidate_ids_current())
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
    add_winner_text_center(p, winners, candidate_ids(), input$total_voters)
  })
  
  bars_score <- reactive({
    pal <- setNames(candidate_palette[seq_len(length(candidate_ids_current()))],
                    candidate_ids_current())
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
    add_winner_text_center(p, winners, candidate_ids(), max_dist)
  })
  
  bars_approval <- reactive({
    cur_ids <- candidate_ids_current()
    pal <- setNames(candidate_palette[seq_len(length(candidate_ids_current()))],
                    candidate_ids_current())
    df <- approval_summary()
    df$fill <- df$candidate
    pal_full <- c(pal, "Didn't vote" = "#777777")
    
    x_levels <- c(cur_ids, "Didn't vote")
    df$candidate <- factor(df$candidate, levels = x_levels)
    
    x_labels <- setNames(x_levels, x_levels)
    if (length(cur_ids) > 10) x_labels["Didn't vote"] <- "Didn't\nvote"
    
    # winners (ignore "Didn't vote")
    cand_df <- df |> filter(as.character(candidate) != "Didn't vote")
    maxv <- max(cand_df$value)
    winners <- if (sum(cand_df$value == maxv) > 1) cand_df$candidate[cand_df$value == maxv]
    else cand_df$candidate[which.max(cand_df$value)]
    
    # --- turnout denominator for the 50% guide ---
    didnt   <- df |> filter(as.character(candidate) == "Didn't vote") |> pull(value)
    didnt   <- if (length(didnt)) didnt else 0L
    turnout <- max(0L, input$total_voters - didnt)
    y50     <- turnout / 2
    
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
      labs(title = sprintf("Approval Results (threshold = %s)", approval_thr()),
           x = "Candidate", y = "Approvals")
    
    p <- add_bar_value_labels(p, df, "candidate", "value", input$total_voters, 0)
    p <- add_winner_text_center(p, winners, cur_ids, input$total_voters)
    add_half_line_discrete(p, y50, x_levels)
  })
  
  bars_borda <- reactive({
    pal <- setNames(candidate_palette[seq_len(length(candidate_ids_current()))],
                    candidate_ids_current())
    df  <- borda_summary() |> rename(points = Points)
    
    K <- ncol(rank_matrix_current())
    N <- nrow(rank_matrix_current())
    max_points <- (K - 1L) * N
    winners <- df$candidate[df$points == max(df$points)]
    
    p <- ggplot(df, aes(x = candidate, y = points, fill = candidate)) +
      geom_col() +
      scale_fill_manual(values = pal, guide = "none") +
      scale_y_continuous(limits = c(0, max_points), breaks = integer_breaks()) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            aspect.ratio = 1) +
      labs(title = "Borda Count Results", x = "Candidate", y = "Points")
    
    p <- add_bar_value_labels(p, df, "candidate", "points", max_points, 0)
    add_winner_text_center(p, winners, candidate_ids(), max_points)
  })
  
  
  
  # ---------- RCV bars ----------
  bars_rcv_round <- reactive({
    out <- rcv_out(); r <- rcv_round(); C <- candidateData_current()
    rounds <- out$rounds; snap <- rounds[[r]]
    active <- snap$active
    dest_levels <- C$id[active]
    title_main <- sprintf("Ranked-Choice Results • Round %d of %d", r, length(rounds))
    subtitle   <- if (!is.na(snap$eliminated)) paste0("Eliminated: ", C$id[snap$eliminated]) else NULL
    
    # Round 1
    if (r == 1) {
      counts <- snap$counts
      df <- tibble(candidate = C$id, votes = counts, active = active) |> filter(active)
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
      return(add_half_line_discrete(p, input$total_voters/2, df$candidate))
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
    if (identical(input$example_type, "1-dimension")) {
      left_plot <- switch(
        input$voting_system,
        "plurality"     = map_1d(),
        "ranked_choice" = map_1d_rcv(),
        "approval"      = map_1d_approval(),
        "score"         = map_1d(),
        "borda"         = map_1d()
      )
    } else {
      left_plot <- switch(
        input$voting_system,
        "plurality"     = map_default(),
        "ranked_choice" = map_rcv(),
        "approval"      = map_approval(),
        "score"         = map_default(),
        "borda"         = map_default()
      )
    }
    
    right_plot <- switch(
      input$voting_system,
      "plurality"     = bars_plurality(),
      "ranked_choice" = bars_rcv_round(),
      "approval"      = bars_approval(),
      "score"         = bars_score(),
      "borda"         = bars_borda()
    )
    cowplot::plot_grid(left_plot, right_plot, ncol = 2, align = "h")
  })
  
  # ---------- Voter data table ----------
  voter_table <- reactive({
    V  <- voterData()
    C  <- candidateData()
    D  <- dist_matrix()          # N x K distances
    rm <- rank_matrix()          # N x K candidate indices, nearest -> farthest
    N  <- nrow(D); K <- ncol(D)
    thr <- as.numeric(approval_thr())
    
    # Distances as columns A_Distance, B_Distance, ...
    dist_df <- tibble::as_tibble(D, .name_repair = "minimal")
    names(dist_df) <- paste0(C$id, "_Distance")
    
    # Preferences as letters in distance order
    pref_letters <- matrix(C$id[rm], nrow = N, ncol = K)
    pref_df <- tibble::as_tibble(pref_letters, .name_repair = "minimal")
    names(pref_df) <- paste0("Preference_", seq_len(K))
    
    # New "Approves" (comma-separated IDs, by proximity; blank if none)
    approves_vec <- vapply(seq_len(N), function(i) {
      ord  <- rm[i, ]                 # voter i candidate order (nearest -> farthest)
      keep <- D[i, ord] <= thr        # which are within threshold
      ids  <- C$id[ord[keep]]
      if (length(ids) == 0) "" else paste(ids, collapse = ", ")
    }, character(1))
    
    tibble(
      Voter = seq_len(N),
      x     = round(V$x, 1),
      y     = round(V$y, 1)
    ) |>
      bind_cols(dist_df |> mutate(across(everything(), ~ round(., 1)))) |>
      bind_cols(pref_df) |>
      bind_cols(tibble(Approves = approves_vec))   # <-- appended at the end
  })
  
  
  output$voter_table <- renderDT({
    df <- voter_table()
    pref_cols <- grep("^Preference_", names(df), value = TRUE)
    datatable(
      df,
      options  = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE),
      rownames = FALSE
    ) %>% formatStyle(pref_cols, `text-align` = "center")
  })
  
  # ---------- Results summary table ----------
  results_table <- reactive({
    C   <- candidateData_current()
    ids <- C$id
    N   <- nrow(voterData())
    
    # Plurality (existing)
    plur_counts <- tibble(candidate = ids) |>
      left_join(plurality_summary(), by = "candidate") |>
      mutate(Votes = replace_na(Votes, 0L),
             Plurality = sprintf("%d (%d%%)", Votes, round(100 * Votes / N))) |>
      select(candidate, Plurality)
    
    # Ranked-Choice (existing)
    out       <- rcv_out()
    last_snap <- out$rounds[[length(out$rounds)]]
    rc_counts <- last_snap$counts
    rc_active <- last_snap$active
    rc_text   <- ifelse(rc_active,
                        sprintf("%d (%d%%)", rc_counts, round(100 * rc_counts / N)),
                        "X")
    rcv_df <- tibble(candidate = ids, `Ranked-Choice` = rc_text)
    
    # Borda (NEW) — raw point totals
    borda_df <- borda_summary() |>
      right_join(tibble(candidate = ids), by = "candidate") |>
      transmute(candidate, `Borda Count` = Points)
    
    # Approval (existing)
    appr_raw <- approval_summary() |> filter(candidate %in% ids)
    appr_counts <- tibble(candidate = ids) |>
      left_join(appr_raw, by = "candidate") |>
      mutate(value = replace_na(value, 0L),
             pct   = if (N > 0) round(100 * value / N) else 0,
             Approval = sprintf("%d (%d%%)", value, pct)) |>
      select(candidate, Approval)
    
    # Cardinal (existing)
    score <- score_table() |>
      right_join(tibble(candidate = ids), by = "candidate") |>
      transmute(candidate, `Cardinal (Score)` = round(mean_distance, 1))
    
    plur_counts |>
      left_join(rcv_df,   by = "candidate") |>
      left_join(appr_counts, by = "candidate") |>
      left_join(score,    by = "candidate") |>
      left_join(borda_df, by = "candidate") |>
      rename(Candidate = candidate)
  })
  
  output$results_dt <- renderDT({
    df  <- results_table()
    ids <- candidateData()$id
    
    # winners (existing)
    plur_s <- plurality_summary()
    plur_winners <- plur_s$candidate[plur_s$Votes == max(plur_s$Votes, na.rm = TRUE)]
    
    appr_s <- approval_summary() |> filter(candidate %in% ids)
    appr_winners <- appr_s$candidate[appr_s$value == max(appr_s$value, na.rm = TRUE)]
    
    score_s <- score_table()
    score_winners <- score_s$candidate[score_s$mean_distance == min(score_s$mean_distance, na.rm = TRUE)]
    
    out <- rcv_out()
    rcv_winners <- if (is.na(out$winner_index)) candidateData()$id[out$tie_indices]
    else candidateData()$id[out$winner_index]
    
    # NEW: Borda winners
    borda_s <- borda_summary()
    borda_winners <- borda_s$candidate[borda_s$Points == max(borda_s$Points, na.rm = TRUE)]
    
    datatable(
      df,
      class   = "display nowrap compact",
      options = list(
        pageLength = 10, scrollX = TRUE, autoWidth = TRUE,
        columnDefs = list(
          list(width = "90px",  targets = 0),   # Candidate
          list(width = "110px", className = "dt-center", targets = 1), # Plurality
          list(width = "130px", className = "dt-center", targets = 2), # Ranked-Choice
          list(width = "110px", className = "dt-center", targets = 4), # Approval
          list(width = "150px", className = "dt-center", targets = 5),  # Cardinal (Score)
          list(width = "90px",  className = "dt-center", targets = 3) # Borda 
        )
      ),
      rownames = FALSE
    ) %>%
      formatStyle("Plurality",        valueColumns = "Candidate",
                  fontWeight = styleEqual(plur_winners,  rep("bold", length(plur_winners)))) %>%
      formatStyle("Ranked-Choice",    valueColumns = "Candidate",
                  fontWeight = styleEqual(rcv_winners,   rep("bold", length(rcv_winners)))) %>%
      formatStyle("Borda Count",            valueColumns = "Candidate",
                  fontWeight = styleEqual(borda_winners, rep("bold", length(borda_winners)))) %>%
      formatStyle("Approval",         valueColumns = "Candidate",
                  fontWeight = styleEqual(appr_winners,  rep("bold", length(appr_winners)))) %>%
      formatStyle("Cardinal (Score)", valueColumns = "Candidate",
                  fontWeight = styleEqual(score_winners, rep("bold", length(score_winners))))
  }, server = TRUE)
  
}

shinyApp(ui, server)
