## ---------------------------------------------------------------------------
## MA115 Project 1 -- "How to Lie with Statistics (and How to Catch It)"
## 03_part1_graphs.R
##
## PART 1 (deliberately misleading) graphs G1, G2, G3, drawn with base R
## graphics only (no ggplot2).
##
##   G1 -- bar chart,    technique: truncated axis
##   G2 -- scatterplot,  technique: aggregation hiding individual variation
##   G3 -- histograms,   technique: misleading histogram bin selection
##
## Three different graph types are used, which satisfies the requirement of at
## least two. No data value is altered by any of these figures.
##
## Run from the project directory:  Rscript scripts/03_part1_graphs.R
## ---------------------------------------------------------------------------

## ---------------------------------------------------------------------------
## Locate the project directory
##
## The project directory is the folder that holds arabica_data_cleaned.csv. It
## is found by searching the folder of this script, the folder it is being
## sourced from, and the current working directory, walking up a few levels
## from each. The script then switches to it, so every relative path below
## resolves no matter where the script was launched from: an IDE console, the
## scripts folder, or Rscript started from somewhere else entirely.
## ---------------------------------------------------------------------------
find_project_root <- function(marker = "arabica_data_cleaned.csv", max_up = 4L) {
  starts <- getwd()
  cmd <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(cmd)) starts <- c(dirname(sub("^--file=", "", cmd[1])), starts)
  ## When the file is run with source(), base R keeps its path in `ofile`.
  for (i in seq_len(sys.nframe())) {
    of <- tryCatch(get("ofile", envir = sys.frame(i), inherits = FALSE),
                   error = function(e) NULL)
    if (is.character(of) && length(of) == 1L) starts <- c(dirname(of), starts)
  }
  for (s in starts) {
    d <- normalizePath(s, winslash = "/", mustWork = FALSE)
    for (k in seq_len(max_up + 1L)) {
      if (file.exists(file.path(d, marker))) return(d)
      if (identical(dirname(d), d)) break
      d <- dirname(d)
    }
  }
  stop("Cannot locate the project directory containing '", marker, "'.\n",
       "  Searched upward from: ", paste(unique(starts), collapse = " | "), "\n",
       "  Fix: setwd(\"<project directory>\") and run again.", call. = FALSE)
}
setwd(find_project_root())
cat("[path] project directory:", getwd(), "\n")
clean_path <- file.path("data", "coffee_analysis_cleaned.csv")
if (!file.exists(clean_path)) {
  stop("Missing ", clean_path, ". Run scripts/01_data_preparation.R first.")
}
for (d in c("outputs/tables", "outputs/graphs", "outputs/values")) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

save_table <- function(x, name) {
  write.csv(x, file.path("outputs", "tables", name), row.names = FALSE)
}
## High-resolution PNG device: 200 dpi, cairo when the build supports it.
open_png <- function(name, width = 2000, height = 1400, res = 200) {
  args <- list(filename = file.path("outputs", "graphs", name),
               width = width, height = height, res = res)
  if (isTRUE(capabilities("cairo"))) args$type <- "cairo"
  do.call(png, args)
}
close_png <- function() invisible(dev.off())

dat <- read.csv(clean_path, stringsAsFactors = FALSE)
dat$altitude_group <- factor(dat$altitude_group, levels = c("Low", "High"))

cat("\n==========================================================\n")
cat("PART 1: MISLEADING GRAPHS (G1-G3)\n")
cat("==========================================================\n")

low  <- dat$Total.Cup.Points[dat$altitude_group == "Low"]
high <- dat$Total.Cup.Points[dat$altitude_group == "High"]
mean_low  <- mean(low)
mean_high <- mean(high)

## ---------------------------------------------------------------------------
## G1 -- TRUNCATED BAR CHART
## Technique: truncated axis.
##
## The bar heights are the true group means. Only the y-axis window is
## manipulated: it is computed from the data so that the visible range is a
## narrow collar around the two means, which converts a 1.4 point gap on a
## 0-100 point scale into a visually enormous difference in bar height.
## ---------------------------------------------------------------------------
g1_means <- c(Low = mean_low, High = mean_high)
g1_pad   <- 0.15 * (max(g1_means) - min(g1_means))
g1_ylim  <- c(min(g1_means) - g1_pad, max(g1_means) + g1_pad)

## Quantifies the exaggeration: the ratio of the bar heights as actually drawn
## above the truncated baseline, against the ratio of the real means.
g1_drawn_ratio <- (max(g1_means) - g1_ylim[1]) / (min(g1_means) - g1_ylim[1])
g1_true_ratio  <- max(g1_means) / min(g1_means)

open_png("G1_misleading_truncated_barchart.png")
op <- par(mar = c(5, 5, 5, 2))
bp <- barplot(g1_means,
              ylim = g1_ylim,
              xpd = FALSE,                     # clip the bars at the truncated base
              col = c("grey70", "forestgreen"),
              border = "grey20",
              names.arg = c("Low altitude\n(< 1500 m)", "High altitude\n(>= 1500 m)"),
              ylab = "Mean total cup points",
              xlab = "Altitude group",
              main = "G1  High-altitude coffee scores dramatically higher",
              cex.names = 1.0, cex.lab = 1.1)
text(bp, g1_means - 0.07 * diff(g1_ylim), labels = sprintf("%.2f", g1_means),
     col = "white", font = 2, cex = 1.2)
mtext(sprintf("Mean quality by altitude group (y-axis shown from %.2f to %.2f)",
              g1_ylim[1], g1_ylim[2]), side = 3, line = 0.3, cex = 0.85)
par(op)
close_png()

g1_tbl <- data.frame(
  label = "G1",
  item = c("mean points, Low", "mean points, High", "difference",
           "y-axis lower limit used", "y-axis upper limit used",
           "visual height ratio as drawn", "true ratio of the means"),
  value = c(mean_low, mean_high, mean_high - mean_low,
            g1_ylim[1], g1_ylim[2], g1_drawn_ratio, g1_true_ratio),
  stringsAsFactors = FALSE
)
save_table(g1_tbl, "G1_truncated_axis_details.csv")

cat("\nG1  Truncated bar chart\n")
cat("  Technique ........ truncated axis\n")
cat(sprintf("  y-axis drawn from %.2f to %.2f instead of 0 to 100\n",
            g1_ylim[1], g1_ylim[2]))
cat(sprintf("  The High bar is drawn %.1f times taller than the Low bar,\n",
            g1_drawn_ratio))
cat(sprintf("  although the means themselves differ by a factor of only %.3f.\n",
            g1_true_ratio))
cat("  Saved: outputs/graphs/G1_misleading_truncated_barchart.png\n")

## ---------------------------------------------------------------------------
## G2 -- AGGREGATED ALTITUDE SCATTERPLOT
## Technique: aggregation hiding individual variation.
##
## Altitude is cut into consistent 200 m bands and each band contributes a
## single point, its mean score. Two things happen:
##   (a) 1055 noisy coffees become a handful of tidy dots, so the scatter that
##       dominates the real data disappears;
##   (b) a band holding 10 coffees is drawn exactly as prominently as a band
##       holding 225, so the trend line is driven by thin bands.
##
## A minimum band size is applied before plotting. Dropping bands with very
## few observations is a defensible stability rule, and it is stated openly
## here -- but it also removes the sparse high-altitude bands that spoil the
## story. Every band, including the dropped ones, is kept in the saved table
## and all of them are shown in Part 2.
## ---------------------------------------------------------------------------
g2_band_width <- 200
g2_min_band_n <- 10
g2_breaks <- seq(100, 3500, by = g2_band_width)
dat$alt_band <- cut(dat$altitude_mean_meters, breaks = g2_breaks,
                    include.lowest = TRUE, right = FALSE)

g2_band_tbl <- data.frame(
  label = "G2",
  band = levels(dat$alt_band),
  band_lower_m = head(g2_breaks, -1),
  band_upper_m = tail(g2_breaks, -1),
  band_midpoint_m = (head(g2_breaks, -1) + tail(g2_breaks, -1)) / 2,
  n_observations = as.integer(table(dat$alt_band)),
  mean_points = as.numeric(tapply(dat$Total.Cup.Points, dat$alt_band, mean)),
  sd_points = as.numeric(tapply(dat$Total.Cup.Points, dat$alt_band, sd)),
  stringsAsFactors = FALSE
)
g2_band_tbl$band_midpoint_km <- g2_band_tbl$band_midpoint_m / 1000
g2_band_tbl$shown_in_G1_part1 <- g2_band_tbl$n_observations >= g2_min_band_n
save_table(g2_band_tbl, "G2_altitude_band_means.csv")

g2_nonempty_tbl <- g2_band_tbl[g2_band_tbl$n_observations > 0, ]
g2_plot_tbl     <- g2_band_tbl[g2_band_tbl$n_observations >= g2_min_band_n, ]

## Trend line fitted to the AGGREGATED band means rather than to individual
## coffees. Averaging strips out the within-band noise before the fit, which
## is what inflates R-squared. Part 2 refits the same relationship honestly.
g2_fit <- lm(mean_points ~ band_midpoint_km, data = g2_plot_tbl)
g2_r2_aggregated <- summary(g2_fit)$r.squared

## Two reference fits, for the Part 2 comparison.
g2_fit_all_bands <- lm(mean_points ~ band_midpoint_km, data = g2_nonempty_tbl)
g2_r2_all_bands  <- summary(g2_fit_all_bands)$r.squared
g2_fit_individual <- lm(Total.Cup.Points ~ altitude_km, data = dat)
g2_r2_individual  <- summary(g2_fit_individual)$r.squared

open_png("G2_misleading_aggregated_scatter.png")
op <- par(mar = c(5, 5, 5, 2))
plot(g2_plot_tbl$band_midpoint_m, g2_plot_tbl$mean_points,
     pch = 19, cex = 1.7, col = "darkorange3",
     xlab = "Altitude (m), 200 m band midpoint",
     ylab = "Mean total cup points for the band",
     main = "G2  Quality rises almost linearly with altitude",
     cex.lab = 1.1)
grid(col = "grey88")
points(g2_plot_tbl$band_midpoint_m, g2_plot_tbl$mean_points,
       pch = 19, cex = 1.7, col = "darkorange3")
abline(a = coef(g2_fit)[1], b = coef(g2_fit)[2] / 1000, col = "firebrick", lwd = 3)
mtext(sprintf("Band means only, %d bands plotted, aggregated R-squared = %.3f",
              nrow(g2_plot_tbl), g2_r2_aggregated),
      side = 3, line = 0.3, cex = 0.85)
par(op)
close_png()

g2_tbl <- data.frame(
  label = "G2",
  item = c("band width (m)", "minimum band size required to plot",
           "bands defined", "bands containing data", "bands plotted",
           "observations behind the plotted bands",
           "observations in the analytical sample",
           "R-squared from the plotted band means",
           "R-squared from all non-empty band means",
           "R-squared from the individual observations",
           "R-squared inflation factor (plotted bands vs individuals)",
           "smallest plotted band sample size",
           "largest plotted band sample size"),
  value = c(g2_band_width, g2_min_band_n,
            length(g2_breaks) - 1, nrow(g2_nonempty_tbl), nrow(g2_plot_tbl),
            sum(g2_plot_tbl$n_observations), nrow(dat),
            g2_r2_aggregated, g2_r2_all_bands, g2_r2_individual,
            g2_r2_aggregated / g2_r2_individual,
            min(g2_plot_tbl$n_observations), max(g2_plot_tbl$n_observations)),
  stringsAsFactors = FALSE
)
save_table(g2_tbl, "G2_aggregation_details.csv")

cat("\nG2  Aggregated altitude scatterplot\n")
cat("  Technique ........ aggregation hiding individual variation\n")
cat(sprintf("  %d consistent %d m bands; %d contain data; %d meet the n >= %d rule\n",
            length(g2_breaks) - 1, g2_band_width, nrow(g2_nonempty_tbl),
            nrow(g2_plot_tbl), g2_min_band_n))
cat(sprintf("  %d individual coffees collapse into %d plotted points\n",
            nrow(dat), nrow(g2_plot_tbl)))
cat(sprintf("  Plotted band sizes run from %d to %d, yet every point looks equal\n",
            min(g2_plot_tbl$n_observations), max(g2_plot_tbl$n_observations)))
cat(sprintf("  R-squared: plotted bands %.4f | all bands %.4f | individuals %.4f\n",
            g2_r2_aggregated, g2_r2_all_bands, g2_r2_individual))
cat(sprintf("  Aggregation inflates R-squared by a factor of %.1f\n",
            g2_r2_aggregated / g2_r2_individual))
cat("  Every band, including those dropped from the figure:\n")
print(g2_nonempty_tbl[, c("band", "n_observations", "mean_points",
                          "shown_in_G1_part1")],
      row.names = FALSE, digits = 4)
cat("  Saved: outputs/graphs/G2_misleading_aggregated_scatter.png\n")

## ---------------------------------------------------------------------------
## G3 -- SELECTIVELY BINNED HISTOGRAM
## Technique: misleading histogram bin selection.
##
## Step 1 investigates a range of defensible bin widths on a common scale.
## Step 2 selects the width whose histograms put the two groups' visual peaks
## furthest apart, then draws each group with its OWN breaks over its OWN
## x-range on a raw count scale. Every score is untouched; only the binning
## and the axes are chosen for effect.
## ---------------------------------------------------------------------------
g3_candidates <- c(0.25, 0.5, 1, 1.5, 2, 2.5, 3, 4, 5)

## For each candidate width, bin BOTH groups on identical breaks and record
## how far apart the two modal (tallest) bins sit. A wider gap between the
## modal bins is what makes the pair of histograms look more separated.
scan_width <- function(w) {
  rng  <- range(dat$Total.Cup.Points)
  brks <- seq(floor(rng[1] / w) * w, ceiling(rng[2] / w) * w, by = w)
  mids <- head(brks, -1) + w / 2
  cl <- table(cut(low,  breaks = brks, include.lowest = TRUE))
  ch <- table(cut(high, breaks = brks, include.lowest = TRUE))
  c(n_bins = length(brks) - 1,
    modal_bin_low = mids[which.max(cl)],
    modal_bin_high = mids[which.max(ch)],
    modal_gap_points = mids[which.max(ch)] - mids[which.max(cl)],
    max_bin_share_gap = max(abs(prop.table(ch) - prop.table(cl))))
}
g3_scan <- as.data.frame(t(vapply(g3_candidates, scan_width, numeric(5))))
g3_bin_tbl <- data.frame(label = "G3", bin_width_points = g3_candidates,
                         g3_scan, stringsAsFactors = FALSE)
save_table(g3_bin_tbl, "G3_bin_width_investigation.csv")

## What the standard automatic rules would have chosen, for reference.
sturges_bins <- nclass.Sturges(dat$Total.Cup.Points)
fd_bins      <- nclass.FD(dat$Total.Cup.Points)
scott_bins   <- nclass.scott(dat$Total.Cup.Points)
g3_rules_tbl <- data.frame(
  label = "G3",
  rule = c("Sturges", "Freedman-Diaconis", "Scott"),
  suggested_n_bins = c(sturges_bins, fd_bins, scott_bins),
  implied_bin_width_points =
    diff(range(dat$Total.Cup.Points)) / c(sturges_bins, fd_bins, scott_bins),
  stringsAsFactors = FALSE
)
save_table(g3_rules_tbl, "G3_automatic_bin_rules.csv")

## Selection rule: the largest modal-bin gap; ties broken by the larger
## single-bin share gap.
g3_order <- order(-g3_bin_tbl$modal_gap_points, -g3_bin_tbl$max_bin_share_gap)
g3_chosen_width <- g3_bin_tbl$bin_width_points[g3_order[1]]

## Exact bin definitions, retained in code. Each group is binned over its own
## range, so the two panels are deliberately not on a comparable footing.
g3_breaks_low  <- seq(floor(min(low)  / g3_chosen_width) * g3_chosen_width,
                      ceiling(max(low)  / g3_chosen_width) * g3_chosen_width,
                      by = g3_chosen_width)
g3_breaks_high <- seq(floor(min(high) / g3_chosen_width) * g3_chosen_width,
                      ceiling(max(high) / g3_chosen_width) * g3_chosen_width,
                      by = g3_chosen_width)

open_png("G3_misleading_histograms.png", width = 2200, height = 1100)
op <- par(mfrow = c(1, 2), mar = c(5, 5, 4, 1), oma = c(0, 0, 3, 0))
hist(low, breaks = g3_breaks_low, col = "grey70", border = "white",
     xlim = range(g3_breaks_low), freq = TRUE,
     xlab = "Total cup points", ylab = "Number of coffees",
     main = "Low altitude (< 1500 m)", cex.lab = 1.1)
hist(high, breaks = g3_breaks_high, col = "forestgreen", border = "white",
     xlim = range(g3_breaks_high), freq = TRUE,
     xlab = "Total cup points", ylab = "Number of coffees",
     main = "High altitude (>= 1500 m)", cex.lab = 1.1)
mtext("G3  High-altitude coffee occupies a visibly higher quality range",
      outer = TRUE, cex = 1.2, font = 2, line = 0.5)
par(op)
close_png()

g3_tbl <- data.frame(
  label = "G3",
  item = c("chosen bin width (points)",
           "bins drawn, low-altitude panel", "bins drawn, high-altitude panel",
           "x-axis lower limit, low panel", "x-axis upper limit, low panel",
           "x-axis lower limit, high panel", "x-axis upper limit, high panel",
           "n low", "n high"),
  value = c(g3_chosen_width,
            length(g3_breaks_low) - 1, length(g3_breaks_high) - 1,
            min(g3_breaks_low), max(g3_breaks_low),
            min(g3_breaks_high), max(g3_breaks_high),
            length(low), length(high)),
  note = c("selected to maximise the modal-bin gap",
           "", "", "", "", "", "",
           "y-axis is raw counts despite unequal group sizes", ""),
  stringsAsFactors = FALSE
)
save_table(g3_tbl, "G3_histogram_details.csv")

cat("\nG3  Selectively binned histograms\n")
cat("  Technique ........ misleading histogram bin selection\n")
cat("  Bin widths investigated (identical breaks for both groups):\n")
print(g3_bin_tbl[, c("bin_width_points", "n_bins", "modal_bin_low",
                     "modal_bin_high", "modal_gap_points", "max_bin_share_gap")],
      row.names = FALSE, digits = 4)
cat("  Automatic rules for reference:\n")
print(g3_rules_tbl[, c("rule", "suggested_n_bins", "implied_bin_width_points")],
      row.names = FALSE, digits = 4)
cat(sprintf("  Selected bin width ........ %.2f points\n", g3_chosen_width))
cat(sprintf("  Low panel  x-axis %.2f to %.2f with %d bins\n",
            min(g3_breaks_low), max(g3_breaks_low), length(g3_breaks_low) - 1))
cat(sprintf("  High panel x-axis %.2f to %.2f with %d bins\n",
            min(g3_breaks_high), max(g3_breaks_high), length(g3_breaks_high) - 1))
cat("  The panels use different breaks and different x-ranges, on a raw count\n")
cat("  scale with unequal group sizes.\n")
cat("  Saved: outputs/graphs/G3_misleading_histograms.png\n")

## ---------------------------------------------------------------------------
## Graph type inventory, for the Step 10 verification
## ---------------------------------------------------------------------------
graph_types <- data.frame(
  label = c("G1", "G2", "G3"),
  graph_type = c("bar chart", "scatterplot", "histogram"),
  base_r_function = c("barplot()", "plot() + abline()", "hist()"),
  technique = c("truncated axis", "aggregation hiding individual variation",
                "misleading histogram bin selection"),
  file = c("G1_misleading_truncated_barchart.png",
           "G2_misleading_aggregated_scatter.png",
           "G3_misleading_histograms.png"),
  stringsAsFactors = FALSE
)
save_table(graph_types, "G_graph_type_inventory.csv")
cat(sprintf("\n  Distinct graph types used across G1-G3: %d\n",
            length(unique(graph_types$graph_type))))

saveRDS(list(
  g1 = list(mean_low = mean_low, mean_high = mean_high,
            ylim = g1_ylim, drawn_ratio = g1_drawn_ratio,
            true_ratio = g1_true_ratio),
  g2 = list(band_width = g2_band_width, min_band_n = g2_min_band_n,
            n_bands_defined = length(g2_breaks) - 1,
            n_bands_with_data = nrow(g2_nonempty_tbl),
            n_bands_plotted = nrow(g2_plot_tbl),
            n_obs_plotted = sum(g2_plot_tbl$n_observations),
            r2_aggregated = g2_r2_aggregated,
            r2_all_bands = g2_r2_all_bands,
            r2_individual = g2_r2_individual,
            inflation = g2_r2_aggregated / g2_r2_individual,
            min_band_n_plotted = min(g2_plot_tbl$n_observations),
            max_band_n_plotted = max(g2_plot_tbl$n_observations),
            band_tbl = g2_band_tbl),
  g3 = list(chosen_width = g3_chosen_width, candidates = g3_candidates,
            scan = g3_bin_tbl, rules = g3_rules_tbl,
            breaks_low = g3_breaks_low, breaks_high = g3_breaks_high,
            n_low = length(low), n_high = length(high)),
  graph_types = graph_types
), file.path("outputs", "values", "03_part1_graphs.rds"))

cat("\n[OK] 03_part1_graphs.R complete\n")
