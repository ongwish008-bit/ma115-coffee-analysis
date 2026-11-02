## ---------------------------------------------------------------------------
## MA115 Project 1 -- "How to Lie with Statistics (and How to Catch It)"
## 06_part2_graphs.R
##
## PART 2 (honest) graphs. Labels match Part 1 exactly. Base R graphics only.
##
##   G1 DEBUNK -- same bars on a full 0-100 axis, with sample sizes
##   G2 DEBUNK -- every individual observation, with the individual-level fit
##   G3 DEBUNK -- identical bins, identical x-range, comparable density scale
##
## Each debunk is also saved beside its misleading twin in a single
## side-by-side figure.
##
## Run from the project directory:  Rscript scripts/06_part2_graphs.R
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
cat("PART 2: HONEST GRAPHS (G1-G3 DEBUNKS)\n")
cat("==========================================================\n")

low  <- dat$Total.Cup.Points[dat$altitude_group == "Low"]
high <- dat$Total.Cup.Points[dat$altitude_group == "High"]
mean_low  <- mean(low)
mean_high <- mean(high)
n_low  <- length(low)
n_high <- length(high)

## ---------------------------------------------------------------------------
## G1 DEBUNK -- full 0 to 100 axis with sample sizes
## ---------------------------------------------------------------------------
g1_means <- c(Low = mean_low, High = mean_high)
## 95% confidence intervals for the two means, so the reader can judge the gap.
g1_se <- c(sd(low) / sqrt(n_low), sd(high) / sqrt(n_high))
g1_ci_lo <- g1_means - qt(0.975, c(n_low, n_high) - 1) * g1_se
g1_ci_hi <- g1_means + qt(0.975, c(n_low, n_high) - 1) * g1_se

draw_g1_honest <- function(main_txt = "G1 DEBUNK  The same means on a full 0-100 scale") {
  bp <- barplot(g1_means, ylim = c(0, 100),
                col = c("grey70", "forestgreen"), border = "grey20",
                names.arg = c(sprintf("Low altitude\n(< 1500 m)\nn = %d", n_low),
                              sprintf("High altitude\n(>= 1500 m)\nn = %d", n_high)),
                ylab = "Mean total cup points (scale 0-100)",
                xlab = "Altitude group", main = main_txt, cex.lab = 1.05)
  arrows(bp, g1_ci_lo, bp, g1_ci_hi, angle = 90, code = 3, length = 0.06, lwd = 2)
  text(bp, g1_means + 5, labels = sprintf("%.2f", g1_means), font = 2, cex = 1.05)
  invisible(bp)
}

open_png("G1_debunk_full_axis_barchart.png")
op <- par(mar = c(6, 5, 5, 2))
draw_g1_honest()
mtext(sprintf("Difference = %.2f points, which is %.1f%% of the 0-100 scale",
              mean_high - mean_low, 100 * (mean_high - mean_low) / 100),
      side = 3, line = 0.3, cex = 0.85)
par(op)
close_png()

## Side by side with the Part 1 version.
g1_pad  <- 0.15 * (max(g1_means) - min(g1_means))
g1_ylim <- c(min(g1_means) - g1_pad, max(g1_means) + g1_pad)

open_png("G1_comparison_misleading_vs_honest.png", width = 2400, height = 1200)
op <- par(mfrow = c(1, 2), mar = c(6, 5, 4, 1), oma = c(0, 0, 3, 0))
bp <- barplot(g1_means, ylim = g1_ylim, xpd = FALSE,
              col = c("grey70", "forestgreen"), border = "grey20",
              names.arg = c("Low", "High"),
              ylab = "Mean total cup points",
              main = sprintf("MISLEADING (G1): y-axis %.2f to %.2f",
                             g1_ylim[1], g1_ylim[2]))
text(bp, g1_means - 0.07 * diff(g1_ylim), labels = sprintf("%.2f", g1_means),
     col = "white", font = 2)
draw_g1_honest(main_txt = "HONEST (G1 DEBUNK): y-axis 0 to 100")
mtext("G1  The identical two numbers, drawn two ways",
      outer = TRUE, cex = 1.2, font = 2, line = 0.5)
par(op)
close_png()

g1_tbl <- data.frame(
  label = "G1_debunk",
  altitude_group = c("Low", "High"),
  n = c(n_low, n_high),
  mean_points = g1_means,
  ci_lower_95 = g1_ci_lo,
  ci_upper_95 = g1_ci_hi,
  stringsAsFactors = FALSE
)
save_table(g1_tbl, "G1_debunk_group_means_with_n.csv")

cat("\nG1 DEBUNK  Full-scale bar chart\n")
print(g1_tbl[, c("altitude_group", "n", "mean_points", "ci_lower_95", "ci_upper_95")],
      row.names = FALSE, digits = 5)
cat(sprintf("  On a 0-100 axis the %.2f point gap is %.1f%% of the scale and the\n",
            mean_high - mean_low, mean_high - mean_low))
cat("  two bars are nearly indistinguishable.\n")
cat("  Saved: outputs/graphs/G1_debunk_full_axis_barchart.png\n")
cat("         outputs/graphs/G1_comparison_misleading_vs_honest.png\n")

## ---------------------------------------------------------------------------
## G2 DEBUNK -- all 1055 individual observations with the individual-level fit
## ---------------------------------------------------------------------------
g2_band_width <- 200
g2_min_band_n <- 10
g2_breaks <- seq(100, 3500, by = g2_band_width)
dat$alt_band <- cut(dat$altitude_mean_meters, breaks = g2_breaks,
                    include.lowest = TRUE, right = FALSE)
band_tbl <- data.frame(
  band_midpoint_m = (head(g2_breaks, -1) + tail(g2_breaks, -1)) / 2,
  n_observations = as.integer(table(dat$alt_band)),
  mean_points = as.numeric(tapply(dat$Total.Cup.Points, dat$alt_band, mean)),
  stringsAsFactors = FALSE
)
band_tbl$band_midpoint_km <- band_tbl$band_midpoint_m / 1000
band_nonempty <- band_tbl[band_tbl$n_observations > 0, ]
band_plotted  <- band_tbl[band_tbl$n_observations >= g2_min_band_n, ]

fit_individual <- lm(Total.Cup.Points ~ altitude_km, data = dat)
r2_individual  <- summary(fit_individual)$r.squared
fit_aggregated <- lm(mean_points ~ band_midpoint_km, data = band_plotted)
r2_aggregated  <- summary(fit_aggregated)$r.squared

draw_g2_honest <- function(main_txt = "G2 DEBUNK  Every individual coffee, with the real fit") {
  plot(dat$altitude_mean_meters, dat$Total.Cup.Points,
       pch = 16, cex = 0.55, col = rgb(0.15, 0.15, 0.15, 0.30),
       xlab = "Altitude (m)", ylab = "Total cup points",
       main = main_txt, cex.lab = 1.05)
  ## Individual-level regression line, drawn on the metre scale.
  abline(a = coef(fit_individual)[1], b = coef(fit_individual)[2] / 1000,
         col = "firebrick", lwd = 3)
  ## Band means overlaid, sized by how many coffees each one represents.
  points(band_nonempty$band_midpoint_m, band_nonempty$mean_points,
         pch = 21, bg = "darkorange", col = "grey20",
         cex = 0.8 + 2.2 * sqrt(band_nonempty$n_observations /
                                  max(band_nonempty$n_observations)))
  legend("bottomright",
         legend = c(sprintf("individual observations (n = %d)", nrow(dat)),
                    sprintf("individual-level fit, R-squared = %.4f", r2_individual),
                    "band means, sized by band n"),
         pch = c(16, NA, 21), lty = c(NA, 1, NA), lwd = c(NA, 3, NA),
         col = c("grey40", "firebrick", "grey20"),
         pt.bg = c(NA, NA, "darkorange"), bty = "n", cex = 0.75)
}

open_png("G2_debunk_individual_scatter.png")
op <- par(mar = c(5, 5, 5, 2))
draw_g2_honest()
mtext(sprintf("Altitude explains %.2f%% of the variation in individual scores",
              100 * r2_individual), side = 3, line = 0.3, cex = 0.85)
par(op)
close_png()

## Side by side with the Part 1 aggregated version.
open_png("G2_comparison_misleading_vs_honest.png", width = 2400, height = 1200)
op <- par(mfrow = c(1, 2), mar = c(5, 5, 4, 1), oma = c(0, 0, 3, 0))
plot(band_plotted$band_midpoint_m, band_plotted$mean_points,
     pch = 19, cex = 1.7, col = "darkorange3",
     xlab = "Altitude (m), band midpoint", ylab = "Mean total cup points",
     main = sprintf("MISLEADING (G2): %d band means, R-sq = %.3f",
                    nrow(band_plotted), r2_aggregated))
abline(a = coef(fit_aggregated)[1], b = coef(fit_aggregated)[2] / 1000,
       col = "firebrick", lwd = 3)
draw_g2_honest(main_txt = sprintf("HONEST (G2 DEBUNK): %d coffees, R-sq = %.4f",
                                  nrow(dat), r2_individual))
mtext("G2  Aggregation turns a noise cloud into a tidy trend",
      outer = TRUE, cex = 1.2, font = 2, line = 0.5)
par(op)
close_png()

g2_tbl <- data.frame(
  label = "G2_debunk",
  quantity = c("observations plotted, honest version",
               "points plotted, misleading version",
               "R-squared, individual observations",
               "R-squared, aggregated band means",
               "R-squared inflation from aggregation",
               "residual SD of individual scores (points)",
               "total spread of individual scores (points)"),
  value = c(nrow(dat), nrow(band_plotted), r2_individual, r2_aggregated,
            r2_aggregated / r2_individual,
            summary(fit_individual)$sigma,
            diff(range(dat$Total.Cup.Points))),
  stringsAsFactors = FALSE
)
save_table(g2_tbl, "G2_debunk_individual_vs_aggregated.csv")

cat("\nG2 DEBUNK  Individual-level scatterplot\n")
print(g2_tbl[, c("quantity", "value")], row.names = FALSE, digits = 4)
cat("  Saved: outputs/graphs/G2_debunk_individual_scatter.png\n")
cat("         outputs/graphs/G2_comparison_misleading_vs_honest.png\n")

## ---------------------------------------------------------------------------
## G3 DEBUNK -- identical bins, identical x-range, comparable density scale
## ---------------------------------------------------------------------------
## One set of breaks covering both groups, chosen by a standard rule rather
## than for visual effect.
g3_width <- 1
g3_rng   <- range(dat$Total.Cup.Points)
g3_breaks <- seq(floor(g3_rng[1] / g3_width) * g3_width,
                 ceiling(g3_rng[2] / g3_width) * g3_width, by = g3_width)
g3_xlim <- range(g3_breaks)

h_low  <- hist(low,  breaks = g3_breaks, plot = FALSE)
h_high <- hist(high, breaks = g3_breaks, plot = FALSE)
g3_ylim <- c(0, max(c(h_low$density, h_high$density)) * 1.08)

draw_g3_honest <- function(titles = c("Low altitude (< 1500 m)",
                                      "High altitude (>= 1500 m)")) {
  hist(low, breaks = g3_breaks, freq = FALSE, xlim = g3_xlim, ylim = g3_ylim,
       col = "grey70", border = "white",
       xlab = "Total cup points", ylab = "Density (area sums to 1)",
       main = sprintf("%s\nn = %d", titles[1], n_low), cex.lab = 1.05)
  abline(v = mean_low, col = "firebrick", lwd = 2, lty = 2)
  hist(high, breaks = g3_breaks, freq = FALSE, xlim = g3_xlim, ylim = g3_ylim,
       col = "forestgreen", border = "white",
       xlab = "Total cup points", ylab = "Density (area sums to 1)",
       main = sprintf("%s\nn = %d", titles[2], n_high), cex.lab = 1.05)
  abline(v = mean_high, col = "firebrick", lwd = 2, lty = 2)
}

open_png("G3_debunk_matched_histograms.png", width = 2200, height = 1100)
op <- par(mfrow = c(1, 2), mar = c(5, 5, 5, 1), oma = c(0, 0, 3, 0))
draw_g3_honest()
mtext(sprintf(paste0("G3 DEBUNK  Identical %.1f point bins, identical x-axis ",
                     "%.0f-%.0f, density scale"),
              g3_width, g3_xlim[1], g3_xlim[2]),
      outer = TRUE, cex = 1.1, font = 2, line = 0.5)
par(op)
close_png()

## A single overlay makes the overlap unmistakable.
open_png("G3_debunk_overlay_density.png")
op <- par(mar = c(5, 5, 5, 2))
hist(low, breaks = g3_breaks, freq = FALSE, xlim = g3_xlim, ylim = g3_ylim,
     col = rgb(0.5, 0.5, 0.5, 0.55), border = "white",
     xlab = "Total cup points", ylab = "Density (area sums to 1)",
     main = "G3 DEBUNK  The two distributions almost completely overlap",
     cex.lab = 1.05)
hist(high, breaks = g3_breaks, freq = FALSE, add = TRUE,
     col = rgb(0.13, 0.55, 0.13, 0.55), border = "white")
abline(v = c(mean_low, mean_high), col = c("grey25", "darkgreen"), lwd = 2, lty = 2)
legend("topleft",
       legend = c(sprintf("Low altitude, n = %d, mean %.2f", n_low, mean_low),
                  sprintf("High altitude, n = %d, mean %.2f", n_high, mean_high)),
       fill = c(rgb(0.5, 0.5, 0.5, 0.55), rgb(0.13, 0.55, 0.13, 0.55)),
       bty = "n", cex = 0.85)
par(op)
close_png()

## Side by side with the Part 1 version, using the Part 1 bin choice.
g3_mis_width <- 2.5
g3_mis_breaks_low  <- seq(floor(min(low) / g3_mis_width) * g3_mis_width,
                          ceiling(max(low) / g3_mis_width) * g3_mis_width,
                          by = g3_mis_width)
g3_mis_breaks_high <- seq(floor(min(high) / g3_mis_width) * g3_mis_width,
                          ceiling(max(high) / g3_mis_width) * g3_mis_width,
                          by = g3_mis_width)

open_png("G3_comparison_misleading_vs_honest.png", width = 2400, height = 2000)
op <- par(mfrow = c(2, 2), mar = c(5, 5, 4, 1), oma = c(0, 0, 4, 0))
hist(low, breaks = g3_mis_breaks_low, col = "grey70", border = "white",
     xlim = range(g3_mis_breaks_low), freq = TRUE,
     xlab = "Total cup points", ylab = "Number of coffees",
     main = "MISLEADING (G3): Low, own bins and own x-range")
hist(high, breaks = g3_mis_breaks_high, col = "forestgreen", border = "white",
     xlim = range(g3_mis_breaks_high), freq = TRUE,
     xlab = "Total cup points", ylab = "Number of coffees",
     main = "MISLEADING (G3): High, own bins and own x-range")
draw_g3_honest(titles = c("HONEST (G3 DEBUNK): Low, matched bins",
                          "HONEST (G3 DEBUNK): High, matched bins"))
mtext("G3  Matching the bins and the axes removes the apparent separation",
      outer = TRUE, cex = 1.2, font = 2, line = 1)
par(op)
close_png()

g3_tbl <- data.frame(
  label = "G3_debunk",
  quantity = c("bin width, honest version (points)",
               "bin width, misleading version (points)",
               "bins, honest version (both groups)",
               "bins, misleading version, low panel",
               "bins, misleading version, high panel",
               "x-axis range, honest version (both groups)",
               "x-axis range, misleading version, low panel",
               "x-axis range, misleading version, high panel",
               "y-axis scale, honest version",
               "y-axis scale, misleading version",
               "n low", "n high"),
  value = c(g3_width, g3_mis_width,
            length(g3_breaks) - 1,
            length(g3_mis_breaks_low) - 1, length(g3_mis_breaks_high) - 1,
            sprintf("%.1f to %.1f", g3_xlim[1], g3_xlim[2]),
            sprintf("%.1f to %.1f", min(g3_mis_breaks_low), max(g3_mis_breaks_low)),
            sprintf("%.1f to %.1f", min(g3_mis_breaks_high), max(g3_mis_breaks_high)),
            "density", "raw counts", n_low, n_high),
  stringsAsFactors = FALSE
)
save_table(g3_tbl, "G3_debunk_matched_bins.csv")

## Quantify the overlap the misleading figure hid.
g3_overlap_tbl <- data.frame(
  label = "G3_debunk",
  quantity = c("shared score range (points)",
               "percent of LOW scores inside the HIGH range",
               "percent of HIGH scores inside the LOW range",
               "histogram overlap coefficient (shared density area)"),
  value = c(min(max(low), max(high)) - max(min(low), min(high)),
            100 * mean(low >= min(high) & low <= max(high)),
            100 * mean(high >= min(low) & high <= max(low)),
            sum(pmin(h_low$density, h_high$density)) * g3_width),
  stringsAsFactors = FALSE
)
save_table(g3_overlap_tbl, "G3_debunk_overlap.csv")

cat("\nG3 DEBUNK  Matched histograms\n")
print(g3_tbl[, c("quantity", "value")], row.names = FALSE)
cat("\n")
print(g3_overlap_tbl[, c("quantity", "value")], row.names = FALSE, digits = 4)
cat("  Saved: outputs/graphs/G3_debunk_matched_histograms.png\n")
cat("         outputs/graphs/G3_debunk_overlay_density.png\n")
cat("         outputs/graphs/G3_comparison_misleading_vs_honest.png\n")

saveRDS(list(
  g1 = list(tbl = g1_tbl, diff = mean_high - mean_low,
            n_low = n_low, n_high = n_high),
  g2 = list(tbl = g2_tbl, r2_individual = r2_individual,
            r2_aggregated = r2_aggregated,
            sigma = summary(fit_individual)$sigma),
  g3 = list(tbl = g3_tbl, overlap = g3_overlap_tbl,
            honest_width = g3_width, misleading_width = g3_mis_width,
            overlap_coef = sum(pmin(h_low$density, h_high$density)) * g3_width)
), file.path("outputs", "values", "06_part2_graphs.rds"))

cat("\n[OK] 06_part2_graphs.R complete\n")
