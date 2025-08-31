# Voting System Comparison Simulation (Shiny for R)

> **Status:** Under active development.

An interactive Shiny app for exploring how different voting methods select winners on synthetic electorates. Compare outcomes across 1-D and 2-D political spaces, tweak parameters, and inspect round-by-round dynamics.

## Features

- **Voting systems**  
  - **Plurality** (most first-place votes)  
  - **Ranked-Choice (IRV)** with a Borda-like tiebreaker, alphabetical as last resort; stacked round visualization  
  - **Approval** with adjustable **distance threshold**; turnout percentages exclude “Didn’t vote” (non-approvers)  
  - **Cardinal (Score)** using mean distance (lower is better)  
  - **Borda** (basic scheme): with *K* candidates, ranks score `K-1, K-2, …, 0`
- **Spaces:** 1-D and 2-D electorates with labeled axes
- **Scales:** up to **20 candidates** and **500 voters**
- **Visuals:**
  - Top-aligned layout: map on the left, results on the right
  - 50% guide line/label on bar charts
  - 2-D approval regions drawn as circles (clipped to frame)
  - **1-D approval** regions drawn as **boxes** (candidate-colored with opacity) plus “┐” style brackets for overlap clarity
  - RCV: eliminated candidates shown with a red “X” on maps; stacked flow bars across rounds
- **Tables:**
  - **Voter Data**: coordinates, distances to each candidate, full preference order, and an **“Approves”** column (comma-separated approved candidates; blank if none)
  - **Results**: per-method outcomes; **counts + percentages** formatted `N (P%)`  
    - Cardinal shows numeric score only  
    - RCV shows `X` for eliminated candidates in the final round snapshot
- **Sampling nicety (1-D):** voter and candidate x-positions are sampled with a **minimum 2-notch separation** to avoid overplotting of tick marks.

## Quick start

### Requirements
- R (≥ 4.1 recommended)
- Packages: `shiny`, `tidyverse`, `ggforce`, `cowplot`, `gridExtra`, `DT`

```r
install.packages(c("shiny","tidyverse","ggforce","cowplot","gridExtra","DT"))
