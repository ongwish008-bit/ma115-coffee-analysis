## ---------------------------------------------------------------------------
## MA115 Project 1 -- interactive dashboard (Shiny)
## coffee_dashboard.R
##
## An R GUI version of the coffee altitude analysis canvas. Every chart is
## drawn with base R graphics from data/coffee_analysis_cleaned.csv and the
## tables in outputs/, so scripts/run_all.R must have been run first.
##
## Run from anywhere:
##   Rscript scripts/coffee_dashboard.R
## or from an R console:
##   source("scripts/coffee_dashboard.R")
## ---------------------------------------------------------------------------

## The project .Rprofile activates renv, whose library does not hold shiny.
## Fall back to the user library so the dashboard runs without changing renv.
if (!requireNamespace("shiny", quietly = TRUE)) {
  user_libs <- c(
    Sys.getenv("R_LIBS_USER"),
    file.path(Sys.getenv("LOCALAPPDATA"), "R", "win-library",
              paste(R.version$major, sub("\\..*$", "", R.version$minor), sep = "."))
  )
  user_libs <- unique(user_libs[nzchar(user_libs) & dir.exists(user_libs)])
  .libPaths(c(.libPaths(), user_libs))
}
if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("Package 'shiny' is not installed. Run install.packages(\"shiny\") and try again.",
       call. = FALSE)
}
library(shiny)

## ---------------------------------------------------------------------------
## Locate the project directory
## ---------------------------------------------------------------------------
find_project_root <- function(marker = "arabica_data_cleaned.csv", max_up = 4L) {
  starts <- getwd()
  cmd <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(cmd)) starts <- c(dirname(sub("^--file=", "", cmd[1])), starts)
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
  stop("Cannot locate the project directory containing '", marker, "'.", call. = FALSE)
}
ROOT <- find_project_root()
cat("[path] project directory:", ROOT, "\n")

cleaned_csv <- file.path(ROOT, "data", "coffee_analysis_cleaned.csv")
if (!file.exists(cleaned_csv)) {
  stop("Missing ", cleaned_csv, ". Run scripts/run_all.R first.", call. = FALSE)
}

## ---------------------------------------------------------------------------
## Data
## ---------------------------------------------------------------------------
d   <- read.csv(cleaned_csv, stringsAsFactors = FALSE)
src <- read.csv(file.path(ROOT, "arabica_data_cleaned.csv"), stringsAsFactors = FALSE)
d$altitude_group <- factor(d$altitude_group, levels = c("Low", "High"))

table_dir <- file.path(ROOT, "outputs", "tables")
TABLE_FILES <- sort(list.files(table_dir, pattern = "\\.csv$"))
TABLES <- setNames(
  lapply(TABLE_FILES, function(f)
    read.csv(file.path(table_dir, f), stringsAsFactors = FALSE, check.names = FALSE)),
  TABLE_FILES
)
tb <- function(name) TABLES[[name]]
tv <- function(name, key, key_col = names(tb(name))[2]) {
  x <- tb(name)
  x$value[x[[key_col]] == key][1]
}

SUMMARY_FILES <- sort(list.files(file.path(ROOT, "outputs", "regression_summaries"),
                                 pattern = "\\.txt$"))
GRAPH_FILES   <- sort(list.files(file.path(ROOT, "outputs", "graphs"), pattern = "\\.png$"))
REPORT_MD     <- file.path(ROOT, "analysis_results.md")

## ---------------------------------------------------------------------------
## Interpretation text, taken from analysis_results.md so it stays in sync
## ---------------------------------------------------------------------------
read_interpretations <- function(path) {
  if (!file.exists(path)) return(list())
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  heads <- grep("^## ", lines)
  out <- list()
  for (i in seq_along(heads)) {
    label <- sub("^## ([A-Z][0-9]) .*$", "\\1", lines[heads[i]])
    if (!grepl("^[A-Z][0-9]$", label)) next
    end <- if (i < length(heads)) heads[i + 1] - 1 else length(lines)
    body <- lines[heads[i]:end]
    grab <- function(prefix) {
      hit <- grep(paste0("^\\*\\*", prefix, ":\\*\\*"), body, value = TRUE)
      if (!length(hit)) return(NULL)
      gsub("\\*\\*", "", sub(paste0("^\\*\\*", prefix, ":\\*\\*\\s*"), "", hit[1]))
    }
    out[[label]] <- list(sold = grab("Interpretation being sold"),
                         why  = grab("Why it is misleading"),
                         honest = grab("Honest interpretation"))
  }
  out
}
INTERP <- read_interpretations(REPORT_MD)

## ---------------------------------------------------------------------------
## Shared computations
## ---------------------------------------------------------------------------
COL_LOW   <- "#3B82F6"
COL_HIGH  <- "#F59E0B"
COL_FLAG  <- "#DC2626"
COL_FIT   <- "#7C3AED"
COL_GREEN <- "#16A34A"
COL_GREY  <- "#9CA3AF"
GRP_COL   <- c(Low = COL_LOW, High = COL_HIGH)
GE        <- "\u2265"

pts  <- d$Total.Cup.Points
grp  <- d$altitude_group
low  <- pts[grp == "Low"]
high <- pts[grp == "High"]
n_low  <- length(low)
n_high <- length(high)

r1 <- lm(Total.Cup.Points ~ altitude_km, data = d)
r1_sum <- summary(r1)
r1_r2 <- r1_sum$r.squared
r1_p  <- coef(r1_sum)["altitude_km", "Pr(>|t|)"]
cooks <- cooks.distance(r1)
cooks_cut <- 4 / nrow(d)
cooks_flag <- cooks > cooks_cut

hq_rate <- tapply(d$high_quality, grp, mean)
hq_n    <- tapply(d$high_quality, grp, sum)
rel_risk <- unname(hq_rate["High"] / hq_rate["Low"])

cmp <- tb("R2_debunk_model_comparison.csv")
alt_change_pct <- 100 * (cmp$altitude_coefficient[2] - cmp$altitude_coefficient[1]) /
  cmp$altitude_coefficient[1]
verif <- tb("step10_verification.csv")

fmt  <- function(x, digits = 2) formatC(x, format = "f", digits = digits, big.mark = ",")
fmtp <- function(p) if (p < 1e-3) formatC(p, format = "e", digits = 2) else formatC(p, format = "f", digits = 4)

## Format a data frame for display: numbers to 5 significant figures.
fmt_df <- function(x) {
  for (j in seq_along(x)) {
    if (is.numeric(x[[j]])) {
      v <- x[[j]]
      x[[j]] <- vapply(v, function(z) {
        if (is.na(z)) "" else prettyNum(signif(z, 5), big.mark = ",")
      }, character(1))
    } else if (is.logical(x[[j]])) {
      x[[j]] <- ifelse(is.na(x[[j]]), "", as.character(x[[j]]))
    }
  }
  x
}

## ---------------------------------------------------------------------------
## Plot helpers
## ---------------------------------------------------------------------------
alpha <- function(col, a) adjustcolor(col, alpha.f = a)

base_par <- function(mar = c(4.2, 4.5, 1, 1)) {
  par(mar = mar, bg = "white", family = "sans", cex.axis = 0.9, cex.lab = 0.95,
      las = 1, mgp = c(2.6, 0.7, 0))
}

grid_lines <- function(h = TRUE, v = FALSE) {
  if (h) abline(h = axTicks(2), col = "grey92", lwd = 1)
  if (v) abline(v = axTicks(1), col = "grey92", lwd = 1)
}

legend_top <- function(labels, cols, pch = 15, ...) {
  legend("topleft", legend = labels, col = cols, pch = pch, bty = "n",
         cex = 0.85, inset = c(0, -0.02), ...)
}

forest <- function(labels, est, lo, hi, cols, xlab, mar = c(4.2, 16, 1, 1)) {
  base_par(mar)
  n <- length(est)
  y <- rev(seq_len(n))
  xr <- range(c(lo, hi, 0), na.rm = TRUE)
  plot(est, y, xlim = xr, ylim = c(0.5, n + 0.5), yaxt = "n", pch = 16, col = cols,
       xlab = xlab, ylab = "", cex = 1.2)
  abline(v = axTicks(1), col = "grey92")
  abline(v = 0, lty = 2, col = "grey40")
  segments(lo, y, hi, y, col = cols, lwd = 2.5)
  points(est, y, pch = 16, col = cols, cex = 1.2)
  text(par("usr")[1], y, labels, pos = 2, xpd = TRUE, cex = 0.8)
}

## ---------------------------------------------------------------------------
## Plots: Overview
## ---------------------------------------------------------------------------
plot_exclusions <- function() {
  x <- tb("01_exclusions_by_reason.csv")
  base_par(c(4.2, 15, 1, 3))
  bp <- barplot(rev(x$n_rows_failing), names.arg = rev(x$rule), horiz = TRUE,
                col = COL_HIGH, border = NA, xlab = "Rows removed from the source dataset",
                xlim = c(0, max(x$n_rows_failing) * 1.15), cex.names = 0.85)
  text(rev(x$n_rows_failing), bp, rev(x$n_rows_failing), pos = 4, cex = 0.85)
}

plot_group_pie <- function() {
  par(mar = c(1, 1, 1, 1))
  n <- table(grp)
  pie(n, labels = sprintf("%s altitude\nn = %d (%.1f%%)",
                          c("Low (< 1500 m)", paste0("High (", GE, " 1500 m)")),
                          n, 100 * n / sum(n)),
      col = GRP_COL, border = "white", cex = 0.95)
}

plot_countries <- function() {
  tc <- sort(table(d$Country.of.Origin), decreasing = TRUE)[1:15]
  base_par(c(4.2, 10, 1, 3))
  bp <- barplot(rev(tc), horiz = TRUE, col = COL_LOW, border = NA,
                xlab = "Coffees in the analytical sample", xlim = c(0, max(tc) * 1.15),
                cex.names = 0.85)
  text(rev(tc), bp, rev(tc), pos = 4, cex = 0.8)
}

plot_processing <- function() {
  pm <- d$Processing.Method
  pm[is.na(pm)] <- "Missing"
  tp <- sort(table(pm), decreasing = TRUE)
  base_par(c(4.2, 14, 1, 3))
  bp <- barplot(rev(tp), horiz = TRUE, border = NA, xlab = "Coffees",
                col = ifelse(names(rev(tp)) == "Missing", COL_GREY, COL_GREEN),
                xlim = c(0, max(tp) * 1.15), cex.names = 0.85)
  text(rev(tp), bp, rev(tp), pos = 4, cex = 0.8)
}

## ---------------------------------------------------------------------------
## Plots: N1
## ---------------------------------------------------------------------------
plot_n1_mis <- function() {
  m <- tapply(pts, grp, mean)
  base_par()
  bp <- barplot(m, col = GRP_COL, border = NA, ylim = c(0, 100),
                names.arg = c("Low (< 1500 m)", paste0("High (", GE, " 1500 m)")),
                ylab = "Mean Total.Cup.Points")
  text(bp, m, fmt(m), pos = 3, font = 2)
}

plot_n1_box <- function() {
  base_par(c(4.2, 9, 1, 1))
  boxplot(Total.Cup.Points ~ altitude_group, data = d, horizontal = TRUE, range = 0,
          col = alpha(GRP_COL, 0.3), border = GRP_COL, xlab = "Total.Cup.Points",
          ylab = "", names = c("Low", "High"), lwd = 1.5)
  stripchart(Total.Cup.Points ~ altitude_group, data = d, method = "jitter",
             jitter = 0.18, add = TRUE, pch = 16, cex = 0.45,
             col = alpha(GRP_COL, 0.3))
  points(tapply(pts, grp, mean), 1:2, pch = 21, bg = "white", cex = 1.8, lwd = 2)
  legend("topleft", c("Median (thick line)", "Mean (open circle)", "Whiskers = min to max"),
         bty = "n", cex = 0.8, pch = c(NA, 21, NA), lty = c(1, NA, NA), lwd = c(2, 2, NA))
}

## ---------------------------------------------------------------------------
## Plots: N2
## ---------------------------------------------------------------------------
plot_n2_mis <- function() {
  v <- c(Low = 1, High = rel_risk)
  base_par()
  bp <- barplot(v, col = GRP_COL, border = NA, ylim = c(0, max(v) * 1.2),
                ylab = "Relative chance of a high-quality rating (Low = 1)")
  text(bp, v, sprintf("%.2fx", v), pos = 3, font = 2)
}

plot_n2_hon <- function() {
  ct <- table(grp, factor(d$high_quality, levels = c(FALSE, TRUE)))
  pct <- prop.table(ct, 1) * 100
  base_par(c(4.2, 5, 1, 1))
  bp <- barplot(t(pct[, c("TRUE", "FALSE")]), horiz = TRUE, col = c(COL_GREEN, "grey85"),
                border = NA, xlab = "Percent of the altitude group", xlim = c(0, 100),
                names.arg = c("Low", "High"))
  text(pct[, "TRUE"], bp, sprintf("%d (%.1f%%)", ct[, "TRUE"], pct[, "TRUE"]),
       pos = 4, cex = 0.85, font = 2, col = "#15803d")
  text(pct[, "TRUE"] + pct[, "FALSE"] / 2, bp,
       sprintf("%d (%.1f%%)", ct[, "FALSE"], pct[, "FALSE"]), cex = 0.85)
  legend("top", c(paste(GE, "85 points"), "< 85 points"), fill = c(COL_GREEN, "grey85"),
         border = NA, bty = "n", horiz = TRUE, cex = 0.85, inset = -0.02, xpd = TRUE)
}

## ---------------------------------------------------------------------------
## Plots: N3
## ---------------------------------------------------------------------------
n3_picked <- tb("N3_cherrypicked_bands.csv")[1:2, ]
n3_gap <- tb("N3_cherrypicked_bands.csv")$mean_total_cup_points[3]
n3_used <- sum((d$altitude_mean_meters >= 800 & d$altitude_mean_meters < 1200) |
                 (d$altitude_mean_meters >= 1800 & d$altitude_mean_meters < 2200))

plot_n3_mis <- function() {
  base_par()
  bp <- barplot(n3_picked$mean_total_cup_points, col = c(COL_LOW, COL_HIGH), border = NA,
                names.arg = c("800-1200 m", "1800-2200 m"), ylim = c(0, 100),
                ylab = "Mean Total.Cup.Points")
  text(bp, n3_picked$mean_total_cup_points,
       sprintf("%s\n(n = %d)", fmt(n3_picked$mean_total_cup_points), n3_picked$n),
       pos = 3, cex = 0.9)
}

plot_n3_coverage <- function() {
  par(mar = c(1, 1, 1, 1))
  v <- c(n3_used, nrow(d) - n3_used)
  pie(v, labels = sprintf("%s\nn = %d (%.1f%%)", c("Used by N3", "Silently discarded"),
                          v, 100 * v / sum(v)),
      col = c(COL_HIGH, "grey85"), border = "white")
}

plot_n3_bands <- function() {
  b <- tb("N3_debunk_all_200m_bands.csv")
  b <- b[b$n > 0, ]
  mid <- (b$band_lower_m + b$band_upper_m) / 2
  se <- ifelse(b$n > 1, b$sd_points / sqrt(b$n), NA)
  tq <- ifelse(b$n > 1, qt(0.975, pmax(b$n - 1, 1)), NA)
  lo <- b$mean_points - tq * se
  hi <- b$mean_points + tq * se
  cols <- ifelse(b$band_lower_m >= 1500, COL_HIGH, COL_LOW)
  base_par()
  plot(mid, b$mean_points, type = "n", xlab = "Band midpoint (m)",
       ylab = "Mean Total.Cup.Points",
       ylim = range(c(lo, hi, b$mean_points), na.rm = TRUE))
  rect(800, -1e3, 1200, 1e3, col = alpha(COL_FLAG, 0.08), border = NA)
  rect(1800, -1e3, 2200, 1e3, col = alpha(COL_FLAG, 0.08), border = NA)
  grid_lines()
  segments(mid, lo, mid, hi, col = cols, lwd = 2)
  points(mid, b$mean_points, pch = 21, bg = cols, col = "white",
         cex = 0.8 + 2.2 * sqrt(b$n / max(b$n)))
  legend_top(c("Band below 1500 m", "Band at or above 1500 m", "Bands picked by N3"),
             c(COL_LOW, COL_HIGH, alpha(COL_FLAG, 0.25)), pch = 15)
}

n3_pairs <- local({
  b <- tb("N3_debunk_all_400m_bands.csv")
  b <- b[b$n >= 30, ]
  b <- b[order(b$band_lower_m), ]
  cmb <- combn(nrow(b), 2)
  data.frame(lower = b$band[cmb[1, ]], upper = b$band[cmb[2, ]],
             diff = b$mean_points[cmb[2, ]] - b$mean_points[cmb[1, ]],
             stringsAsFactors = FALSE)
})

plot_n3_pairs <- function() {
  p <- n3_pairs[order(n3_pairs$diff), ]
  base_par()
  plot(p$diff, seq_len(nrow(p)), pch = 16, yaxt = "n",
       col = ifelse(p$diff < 0, COL_FLAG, COL_LOW),
       xlab = "Mean of higher band minus mean of lower band (cup points)",
       ylab = "Band pairs, sorted", xlim = range(c(p$diff, n3_gap, 0)))
  abline(v = 0, col = "grey40")
  abline(v = n3_gap, lty = 2, col = COL_HIGH, lwd = 2)
  abline(v = mean(p$diff), lty = 3, col = "grey30")
  legend("topleft", bty = "n", cex = 0.85,
         legend = c("Higher band scored better", "Higher band scored worse",
                    sprintf("Gap reported by N3 (%.2f)", n3_gap),
                    sprintf("Mean of all pairs (%.2f)", mean(p$diff))),
         col = c(COL_LOW, COL_FLAG, COL_HIGH, "grey30"),
         pch = c(16, 16, NA, NA), lty = c(NA, NA, 2, 3), lwd = c(NA, NA, 2, 1))
}

## ---------------------------------------------------------------------------
## Plots: G1
## ---------------------------------------------------------------------------
g1_lo <- tv("G1_truncated_axis_details.csv", "y-axis lower limit used")
g1_hi <- tv("G1_truncated_axis_details.csv", "y-axis upper limit used")

plot_g1_mis <- function() {
  m <- tapply(pts, grp, mean)
  base_par()
  bp <- barplot(m, col = GRP_COL, border = NA, ylim = c(g1_lo, g1_hi), xpd = FALSE,
                ylab = "Mean Total.Cup.Points", names.arg = c("Low", "High"))
  box(bty = "l")
  text(bp, m, fmt(m), pos = 1, col = "white", font = 2)
}

plot_g1_hon <- function() {
  g <- tb("G1_debunk_group_means_with_n.csv")
  base_par()
  bp <- barplot(g$mean_points, names.arg = g$altitude_group, col = GRP_COL[g$altitude_group],
                border = NA, ylim = c(0, 100), ylab = "Mean Total.Cup.Points")
  arrows(bp, g$ci_lower_95, bp, g$ci_upper_95, angle = 90, code = 3, length = 0.08, lwd = 2)
  text(bp, g$mean_points / 2, sprintf("%s\nn = %d", fmt(g$mean_points), g$n),
       col = "white", font = 2)
}

## ---------------------------------------------------------------------------
## Plots: G2
## ---------------------------------------------------------------------------
g2_bands <- tb("G2_altitude_band_means.csv")
g2_shown <- g2_bands[as.logical(g2_bands$shown_in_G1_part1), ]
g2_fit <- lm(mean_points ~ band_midpoint_m, data = g2_shown)

plot_g2_mis <- function() {
  base_par()
  plot(g2_shown$band_midpoint_m, g2_shown$mean_points, pch = 16, cex = 1.6, col = COL_LOW,
       xlab = "Altitude band midpoint (m)", ylab = "Band mean Total.Cup.Points")
  grid_lines()
  abline(g2_fit, col = COL_FIT, lwd = 2.5)
  points(g2_shown$band_midpoint_m, g2_shown$mean_points, pch = 16, cex = 1.6, col = COL_LOW)
  legend("topleft", bty = "n", cex = 0.85,
         legend = c("Band mean", sprintf("OLS fit on band means, R\u00b2 = %.3f",
                                         summary(g2_fit)$r.squared)),
         col = c(COL_LOW, COL_FIT), pch = c(16, NA), lty = c(NA, 1), lwd = c(NA, 2.5))
}

plot_scatter_r1 <- function() {
  base_par()
  plot(d$altitude_mean_meters, pts, pch = 16, cex = 0.55,
       col = alpha(GRP_COL[as.character(grp)], 0.45),
       xlab = "altitude_mean_meters", ylab = "Total.Cup.Points")
  grid_lines(TRUE, TRUE)
  points(d$altitude_mean_meters, pts, pch = 16, cex = 0.55,
         col = alpha(GRP_COL[as.character(grp)], 0.45))
  abline(a = coef(r1)[1], b = coef(r1)[2] / 1000, col = COL_FIT, lwd = 2.5)
  legend("bottomright", bty = "n", cex = 0.85,
         legend = c("Low (< 1500 m)", paste0("High (", GE, " 1500 m)"),
                    sprintf("R1 fit: %.2f + %.3f x altitude_km", coef(r1)[1], coef(r1)[2])),
         col = c(COL_LOW, COL_HIGH, COL_FIT), pch = c(16, 16, NA),
         lty = c(NA, NA, 1), lwd = c(NA, NA, 2.5))
}

## ---------------------------------------------------------------------------
## Plots: G3
## ---------------------------------------------------------------------------
g3v <- function(item) as.numeric(tv("G3_histogram_details.csv", item))
g3_breaks <- function(lo, hi, x, w) {
  seq(min(lo, floor(min(x) / w) * w), max(hi, ceiling(max(x) / w) * w), by = w)
}
g3_w <- g3v("chosen bin width (points)")

plot_g3_low <- function() {
  base_par()
  hist(low, breaks = g3_breaks(g3v("x-axis lower limit, low panel"),
                               g3v("x-axis upper limit, low panel"), low, g3_w),
       col = COL_LOW, border = "white", main = "", xlab = "Total.Cup.Points", ylab = "Count",
       xlim = c(g3v("x-axis lower limit, low panel"), g3v("x-axis upper limit, low panel")))
}

plot_g3_high <- function() {
  base_par()
  hist(high, breaks = g3_breaks(g3v("x-axis lower limit, high panel"),
                                g3v("x-axis upper limit, high panel"), high, g3_w),
       col = COL_HIGH, border = "white", main = "", xlab = "Total.Cup.Points", ylab = "Count",
       xlim = c(g3v("x-axis lower limit, high panel"), g3v("x-axis upper limit, high panel")))
}

plot_g3_matched <- function() {
  brks <- seq(floor(min(pts)), ceiling(max(pts)), by = 1)
  pl <- hist(low,  breaks = brks, plot = FALSE)$counts / n_low * 100
  ph <- hist(high, breaks = brks, plot = FALSE)$counts / n_high * 100
  base_par()
  barplot(rbind(pl, ph), beside = TRUE, col = GRP_COL, border = NA, space = c(0, 0.3),
          names.arg = head(brks, -1), ylab = "Percent of altitude group",
          xlab = "Total.Cup.Points (1-point bins, labelled by lower edge)", cex.names = 0.75)
  legend("topleft", c(sprintf("Low (n = %d)", n_low), sprintf("High (n = %d)", n_high)),
         fill = GRP_COL, border = NA, bty = "n", cex = 0.85)
}

plot_g3_bins <- function() {
  bw <- tb("G3_bin_width_investigation.csv")
  base_par()
  bp <- barplot(bw$modal_gap_points, names.arg = bw$bin_width_points, border = NA,
                col = ifelse(bw$bin_width_points == g3_w, COL_FLAG, COL_GREY),
                xlab = "Bin width tried (points)",
                ylab = "Gap between modal bins (points)",
                ylim = c(min(0, bw$modal_gap_points), max(bw$modal_gap_points) * 1.2))
  text(bp, bw$modal_gap_points, fmt(bw$modal_gap_points), pos = 3, cex = 0.8)
}

## ---------------------------------------------------------------------------
## Plots: R1
## ---------------------------------------------------------------------------
plot_r1_pie <- function() {
  par(mar = c(1, 1, 1, 1))
  v <- c(r1_r2, 1 - r1_r2) * 100
  pie(v, labels = sprintf("%s\n%.2f%%", c("Explained by altitude", "Unexplained"), v),
      col = c(COL_HIGH, "grey85"), border = "white")
}

plot_r1_country <- function() {
  cs <- tb("01_summary_country.csv")
  base_par()
  symbols(cs$mean_altitude_m, cs$mean_points, circles = sqrt(cs$n), inches = 0.3,
          bg = alpha(ifelse(cs$mean_altitude_m >= 1500, COL_HIGH, COL_LOW), 0.5),
          fg = "white", xlab = "Country mean altitude (m)",
          ylab = "Country mean Total.Cup.Points")
  big <- cs$n >= 15
  text(cs$mean_altitude_m[big], cs$mean_points[big], cs$country[big], cex = 0.75, pos = 3)
}

plot_r1_resid <- function() {
  e <- resid(r1)
  base_par()
  hist(e, breaks = seq(floor(min(e)), ceiling(max(e)), by = 1), col = COL_LOW,
       border = "white", main = "", xlab = "R1 residual (cup points)", ylab = "Count")
  abline(v = 0, lty = 2)
}

plot_r1_infl <- function() {
  s <- tb("R1_debunk_influence_sensitivity.csv")
  base_par(c(4.2, 18, 1, 3))
  lbl <- sub("^R1 ", "", s$model)
  bp <- barplot(rev(s$altitude_coefficient), names.arg = rev(lbl), horiz = TRUE,
                col = c(COL_GREY, COL_GREY, COL_FIT), border = NA, cex.names = 0.8,
                xlab = "Altitude slope (cup points per km)",
                xlim = c(0, max(s$altitude_coefficient) * 1.25))
  text(rev(s$altitude_coefficient), bp,
       sprintf("%.3f (n = %d)", rev(s$altitude_coefficient), rev(s$n)), pos = 4, cex = 0.8)
}

## ---------------------------------------------------------------------------
## Plots: R2
## ---------------------------------------------------------------------------
r1_ci <- tb("R1_debunk_coefficients_with_ci.csv")
r1_ci <- r1_ci[r1_ci$term == "altitude_km", ]
diag_s <- tb("DIAG_model_diagnostics_summary.csv")
coefs <- tb("R2_debunk_all_coefficients.csv")

plot_r2_forest <- function() {
  forest(labels = c(sprintf("R1: altitude only (n = %d)", nrow(d)),
                    "R2: + moisture + processing", "R2 corrected: + country"),
         est = c(r1_ci$estimate, cmp$altitude_coefficient),
         lo  = c(r1_ci$ci_lower_95, cmp$altitude_ci_lower_95),
         hi  = c(r1_ci$ci_upper_95, cmp$altitude_ci_upper_95),
         cols = c(COL_FLAG, COL_FLAG, COL_GREEN),
         xlab = "Altitude coefficient with 95% CI (cup points per km)")
}

plot_r2_fit <- function() {
  m <- rbind(diag_s$r_squared, diag_s$adj_r_squared)
  base_par()
  bp <- barplot(m, beside = TRUE, names.arg = diag_s$model, col = c(COL_LOW, COL_GREY),
                border = NA, ylim = c(0, max(m) * 1.25), ylab = "Proportion of variance")
  text(bp, m, sprintf("%.3f", m), pos = 3, cex = 0.8)
  legend("topleft", c("R-squared", "Adjusted R-squared"), fill = c(COL_LOW, COL_GREY),
         border = NA, bty = "n", cex = 0.85)
}

plot_r2_noncountry <- function() {
  keep <- function(lbl) {
    x <- coefs[coefs$label == lbl & coefs$term != "(Intercept)" &
                 !grepl("^Country\\.of\\.Origin", coefs$term), ]
    setNames(x$estimate, x$term)
  }
  a <- keep("R2_part1")
  b <- keep("R2_corrected")[names(a)]
  m <- rbind(a, b)
  colnames(m) <- sub("^Processing\\.Method", "Processing: ", names(a))
  base_par(c(4.2, 17, 1, 1))
  barplot(m[, ncol(m):1], beside = TRUE, horiz = TRUE, col = c(COL_FLAG, COL_GREEN),
          border = NA, xlab = "Coefficient (cup points)", cex.names = 0.8)
  abline(v = 0, col = "grey40")
  legend("bottomleft", c("Country omitted (R2)", "Country included (R2 corrected)"),
         fill = c(COL_FLAG, COL_GREEN), border = NA, bty = "n", cex = 0.85)
}

plot_r2_country <- function() {
  cc <- coefs[coefs$label == "R2_corrected" & grepl("^Country\\.of\\.Origin", coefs$term), ]
  cc <- cc[order(cc$estimate, decreasing = TRUE), ]
  forest(labels = sub("^Country\\.of\\.Origin", "", cc$term),
         est = cc$estimate, lo = cc$ci_lower_95, hi = cc$ci_upper_95,
         cols = ifelse(cc$p_value < 0.05, COL_FIT, COL_GREY),
         xlab = "Effect relative to Brazil, 95% CI (cup points)",
         mar = c(4.2, 14, 1, 1))
  legend("bottomright", c("p < 0.05", "p >= 0.05"), col = c(COL_FIT, COL_GREY), pch = 16,
         bty = "n", cex = 0.85)
}

## ---------------------------------------------------------------------------
## Plots: Diagnostics and verification
## ---------------------------------------------------------------------------
plot_diag_problems <- function() {
  m <- as.matrix(diag_s[, c("n_above_cooks_threshold", "n_high_leverage",
                            "n_standardised_resid_beyond_3", "n_leverage_one")])
  colnames(m) <- c("Cook's D > 4/n", "High leverage", "|std. resid| > 3", "Leverage = 1")
  base_par()
  bp <- barplot(m, beside = TRUE, col = c(COL_LOW, COL_HIGH, COL_GREEN), border = NA,
                ylim = c(0, max(m) * 1.25), ylab = "Observations", cex.names = 0.85)
  text(bp, m, m, pos = 3, cex = 0.8)
  legend("topright", diag_s$model, fill = c(COL_LOW, COL_HIGH, COL_GREEN), border = NA,
         bty = "n", cex = 0.85)
}

plot_diag_skew <- function() {
  m <- rbind(diag_s$residual_skewness, diag_s$residual_se)
  base_par()
  bp <- barplot(m, beside = TRUE, names.arg = diag_s$model, col = c(COL_FLAG, COL_LOW),
                border = NA, ylim = c(min(m) * 1.3, max(m) * 1.3), ylab = "Value")
  abline(h = 0, col = "grey40")
  text(bp, m, sprintf("%.3f", m), pos = ifelse(m < 0, 1, 3), cex = 0.8)
  legend("topleft", c("Residual skewness", "Residual SE (points)"),
         fill = c(COL_FLAG, COL_LOW), border = NA, bty = "n", cex = 0.85)
}

plot_diag_resid <- function() {
  base_par()
  plot(fitted(r1), resid(r1), pch = 16, cex = 0.6,
       col = ifelse(cooks_flag, COL_FLAG, alpha(COL_LOW, 0.45)),
       xlab = "R1 fitted value", ylab = "R1 residual")
  abline(h = 0, lty = 2)
  legend("bottomleft", c("Cook's D > 4/n", "Other observations"), pch = 16,
         col = c(COL_FLAG, COL_LOW), bty = "n", cex = 0.85)
}

plot_diag_cooks <- function() {
  base_par()
  plot(seq_along(cooks), cooks, type = "h", col = ifelse(cooks_flag, COL_FLAG, COL_GREY),
       xlab = "Observation (row in cleaned CSV)", ylab = "Cook's distance")
  abline(h = cooks_cut, lty = 2, col = COL_FLAG)
  i <- which.max(cooks)
  text(i, cooks[i], sprintf("%s, %d m, %.2f points", d$Country.of.Origin[i],
                            round(d$altitude_mean_meters[i]), pts[i]),
       pos = 4, cex = 0.8)
}

plot_timings <- function() {
  t <- tb("run_all_timings.csv")
  base_par(c(4.2, 13, 1, 3))
  bp <- barplot(rev(t$seconds), names.arg = rev(t$script), horiz = TRUE, col = COL_LOW,
                border = NA, xlab = "Seconds", xlim = c(0, max(t$seconds) * 1.25),
                cex.names = 0.8)
  text(rev(t$seconds), bp, sprintf("%.2f s", rev(t$seconds)), pos = 4, cex = 0.8)
}

PLOTS <- list(
  p_exclusions = plot_exclusions, p_group_pie = plot_group_pie,
  p_countries = plot_countries, p_processing = plot_processing,
  p_n1_mis = plot_n1_mis, p_n1_box = plot_n1_box,
  p_n2_mis = plot_n2_mis, p_n2_hon = plot_n2_hon,
  p_n3_mis = plot_n3_mis, p_n3_cov = plot_n3_coverage,
  p_n3_bands = plot_n3_bands, p_n3_pairs = plot_n3_pairs,
  p_g1_mis = plot_g1_mis, p_g1_hon = plot_g1_hon,
  p_g2_mis = plot_g2_mis, p_g2_hon = plot_scatter_r1,
  p_g3_low = plot_g3_low, p_g3_high = plot_g3_high,
  p_g3_matched = plot_g3_matched, p_g3_bins = plot_g3_bins,
  p_r1_mis = plot_scatter_r1, p_r1_pie = plot_r1_pie,
  p_r1_country = plot_r1_country, p_r1_resid = plot_r1_resid, p_r1_infl = plot_r1_infl,
  p_r2_forest = plot_r2_forest, p_r2_fit = plot_r2_fit,
  p_r2_noncountry = plot_r2_noncountry, p_r2_country = plot_r2_country,
  p_diag_problems = plot_diag_problems, p_diag_skew = plot_diag_skew,
  p_diag_resid = plot_diag_resid, p_diag_cooks = plot_diag_cooks,
  p_timings = plot_timings
)

## ---------------------------------------------------------------------------
## UI helpers
## ---------------------------------------------------------------------------
tbl_id <- function(name) paste0("tbl_", gsub("[^A-Za-z0-9]", "_", name))

panel <- function(id, title, kind = c("honest", "misleading", "info"),
                  caption = NULL, height = 330) {
  kind <- match.arg(kind)
  tag <- switch(kind, misleading = "PART 1 - MISLEADING", honest = "PART 2 - HONEST",
                info = "DATA")
  div(class = paste("panel-card", kind),
      div(class = "panel-kind", tag),
      div(class = "panel-title", title),
      plotOutput(id, height = height),
      if (!is.null(caption)) div(class = "panel-caption", caption))
}

two <- function(a, b) fluidRow(column(6, a), column(6, b))

section_head <- function(label, title, technique) {
  div(class = "section-head",
      span(class = "label-pill", label), span(class = "section-title", title),
      div(class = "technique", paste("Misleading technique:", technique)))
}

interp_block <- function(label) {
  x <- INTERP[[label]]
  if (is.null(x)) return(NULL)
  div(class = "interp",
      if (!is.null(x$sold)) div(class = "callout sold", strong("Interpretation being sold: "), x$sold),
      if (!is.null(x$why)) div(class = "callout why", strong("Why it is misleading: "), x$why),
      if (!is.null(x$honest)) div(class = "callout honest", strong("Honest interpretation: "), x$honest))
}

tables_block <- function(names) {
  names <- names[names %in% TABLE_FILES]
  tagList(
    h4(class = "tables-head", "Output tables"),
    lapply(names, function(n) {
      div(class = "tbl-wrap",
          div(class = "tbl-name", paste0("outputs/tables/", n)),
          div(class = "tbl-scroll", tableOutput(tbl_id(n))))
    })
  )
}

stat <- function(value, label, tone = "neutral") {
  div(class = paste("stat", tone), div(class = "stat-value", value), div(class = "stat-label", label))
}

CSS <- "
body { background: #f6f7f9; font-family: 'Segoe UI', Arial, sans-serif; color: #1f2937; }
.container-fluid { max-width: 1500px; }
h2.app-title { font-weight: 600; margin-top: 18px; }
.subtitle { color: #6b7280; margin-bottom: 14px; }
.stats { display: flex; flex-wrap: wrap; gap: 12px; margin-bottom: 16px; }
.stat { background: white; border: 1px solid #e5e7eb; border-radius: 8px; padding: 10px 16px; min-width: 150px; }
.stat-value { font-size: 22px; font-weight: 600; }
.stat-label { font-size: 12px; color: #6b7280; }
.stat.warning .stat-value { color: #b45309; }
.stat.danger .stat-value { color: #b91c1c; }
.stat.success .stat-value { color: #15803d; }
.nav-tabs { margin-bottom: 14px; }
.panel-card { background: white; border: 1px solid #e5e7eb; border-left-width: 5px; border-radius: 8px; padding: 10px 14px; margin-bottom: 16px; }
.panel-card.misleading { border-left-color: #dc2626; }
.panel-card.honest { border-left-color: #16a34a; }
.panel-card.info { border-left-color: #3b82f6; }
.panel-kind { font-size: 11px; font-weight: 700; letter-spacing: .05em; }
.misleading .panel-kind { color: #dc2626; }
.honest .panel-kind { color: #16a34a; }
.info .panel-kind { color: #3b82f6; }
.panel-title { font-weight: 600; margin-bottom: 4px; }
.panel-caption { font-size: 12.5px; color: #6b7280; margin-top: 4px; }
.section-head { margin: 4px 0 14px 0; }
.label-pill { background: #1f2937; color: white; border-radius: 6px; padding: 3px 9px; font-weight: 700; margin-right: 10px; }
.section-title { font-size: 20px; font-weight: 600; }
.technique { color: #6b7280; margin-top: 6px; }
.callout { border-radius: 8px; padding: 10px 14px; margin-bottom: 10px; }
.callout.sold { background: #fef2f2; border: 1px solid #fecaca; }
.callout.why { background: #fffbeb; border: 1px solid #fde68a; }
.callout.honest { background: #f0fdf4; border: 1px solid #bbf7d0; }
.tables-head { margin-top: 18px; }
.tbl-wrap { background: white; border: 1px solid #e5e7eb; border-radius: 8px; padding: 8px 12px; margin-bottom: 12px; }
.tbl-name { font-family: Consolas, monospace; font-size: 12.5px; color: #374151; margin-bottom: 6px; }
.tbl-scroll { overflow-x: auto; max-height: 520px; overflow-y: auto; }
.tbl-scroll table { font-size: 12.5px; margin-bottom: 0; }
pre.summary { background: white; font-size: 12px; max-height: 760px; overflow: auto; }
.report { background: white; border: 1px solid #e5e7eb; border-radius: 8px; padding: 18px 26px; }
.report table { border-collapse: collapse; margin-bottom: 14px; font-size: 13px; }
.report th, .report td { border: 1px solid #e5e7eb; padding: 4px 8px; }
.report img { max-width: 100%; }
.graph-img img { max-width: 100%; border: 1px solid #e5e7eb; border-radius: 8px; background: white; }
"

## ---------------------------------------------------------------------------
## UI
## ---------------------------------------------------------------------------
shiny::addResourcePath("outputs", file.path(ROOT, "outputs"))

src_cols <- names(src)
ana_cols <- names(d)
key_cols <- c("Country.of.Origin", "altitude_mean_meters", "altitude_group",
              "Total.Cup.Points", "high_quality", "Moisture", "Processing.Method",
              "Variety", "Region", "Harvest.Year")

ui <- fluidPage(
  tags$head(tags$style(HTML(CSS)), tags$title("Coffee altitude analysis")),
  h2(class = "app-title", "MA115 Project 1: is higher-altitude coffee proven to taste better?"),
  div(class = "subtitle",
      "Every result from scripts/run_all.R. Part 1 builds a deliberately misleading case for the claim, ",
      "and Part 2 corrects each item on the same data. Source: arabica_data_cleaned.csv, analytical sample n = ",
      format(nrow(d), big.mark = ","), "."),
  div(class = "stats",
      stat(format(nrow(d), big.mark = ","), sprintf("Analytical sample (of %s)", format(nrow(src), big.mark = ","))),
      stat(sprintf("%d / %d", n_low, n_high), "Low / High altitude"),
      stat(length(unique(d$Country.of.Origin)), "Countries"),
      stat(sprintf("%.1f%%", 100 * r1_r2), "Score variance explained by altitude", "warning"),
      stat(sprintf("%+.1f%%", alt_change_pct), "Altitude effect once country is added", "danger"),
      stat(sprintf("%d / %d", sum(as.logical(verif$passed)), nrow(verif)),
           "Verification checks passed", "success")),
  tabsetPanel(
    id = "tabs",

    tabPanel("Overview",
      two(panel("p_exclusions", "Rows removed by each exclusion rule", "info",
                sprintf("%s source rows minus %d unique exclusions = %s analytical rows.",
                        format(nrow(src), big.mark = ","), nrow(src) - nrow(d),
                        format(nrow(d), big.mark = ","))),
          panel("p_group_pie", "Altitude groups (1500 m split)", "info")),
      two(panel("p_countries", sprintf("Coffees per country of origin (top 15 of %d)",
                                       length(unique(d$Country.of.Origin))), "info"),
          panel("p_processing", "Processing method", "info")),
      tables_block(c("technique_register.csv", "means_all_observations.csv",
                     "01_sample_size_verification.csv", "01_exclusions_by_reason.csv",
                     "01_exclusion_waterfall.csv", "01_exclusion_combinations.csv",
                     "01_exclusion_overlap_depth.csv", "01_invalid_entries_source.csv",
                     "01_summary_group_sizes.csv", "01_summary_quality.csv",
                     "01_summary_altitude.csv", "01_summary_missing_data.csv",
                     "01_summary_country.csv", "01_summary_processing_method.csv",
                     "01_variable_roles.csv", "01_source_structure.csv"))),

    tabPanel("N1",
      section_head("N1", "Comparing average quality", "reporting means without measures of spread"),
      two(panel("p_n1_mis", "Two means, no spread", "misleading",
                sprintf("The pitch: a clear %.2f point quality advantage at high altitude.",
                        mean(high) - mean(low))),
          panel("p_n1_box", "Full distributions of Total.Cup.Points", "honest",
                sprintf("Every coffee shown as a dot. n = %d low, %d high.", n_low, n_high))),
      interp_block("N1"),
      tables_block(c("N1_misleading_means.csv", "N1_full_computation.csv",
                     "N1_debunk_full_descriptives.csv", "N1_debunk_overlap_and_effect_size.csv"))),

    tabPanel("N2",
      section_head("N2", paste0("High-quality coffee probabilities (", GE, " 85 points)"),
                   "relative differences instead of absolute differences"),
      two(panel("p_n2_mis", "Relative chance of a high-quality rating (Low = 1)", "misleading",
                sprintf("The pitch: %.1f times more likely, a %.0f%% increase. No counts are shown.",
                        rel_risk, 100 * (rel_risk - 1))),
          panel("p_n2_hon", paste0("Share of coffees scoring ", GE, " 85 vs < 85"), "honest",
                sprintf("Low: %d of %d (%.2f%%). High: %d of %d (%.2f%%). Absolute difference %.2f percentage points.",
                        hq_n["Low"], n_low, 100 * hq_rate["Low"], hq_n["High"], n_high,
                        100 * hq_rate["High"], 100 * (hq_rate["High"] - hq_rate["Low"])))),
      interp_block("N2"),
      tables_block(c("N2_misleading_presentation.csv", "N2_effect_measures.csv",
                     "N2_full_computation.csv", "N2_debunk_counts.csv",
                     "N2_debunk_effect_measures.csv"))),

    tabPanel("N3",
      section_head("N3", "Selective altitude comparison", "cherry-picking subgroups"),
      two(panel("p_n3_mis", "Two hand-picked 400 m bands", "misleading",
                sprintf("The pitch: 1800-2200 m coffee scores %.2f points higher than 800-1200 m coffee.", n3_gap)),
          panel("p_n3_cov", "How much of the sample N3 uses", "honest")),
      panel("p_n3_bands", "Every 200 m band: mean with 95% CI", "honest",
            "Point size scales with n. Shaded areas are the two bands N3 picked. Bands with n = 1 have no interval.",
            height = 380),
      panel("p_n3_pairs", sprintf("All %d comparable band pairs (400 m bands, both n %s 30)",
                                  nrow(n3_pairs), GE), "honest",
            sprintf("In %d of %d pairs the higher band scored worse.",
                    sum(n3_pairs$diff < 0), nrow(n3_pairs)), height = 380),
      interp_block("N3"),
      tables_block(c("N3_cherrypicked_bands.csv", "N3_coverage.csv",
                     "N3_debunk_all_200m_bands.csv", "N3_debunk_all_400m_bands.csv",
                     "N3_debunk_cherrypick_context.csv", "N3_debunk_monotonicity.csv"))),

    tabPanel("G1",
      section_head("G1", "Bar chart of mean quality by altitude group", "truncated axis"),
      two(panel("p_g1_mis", sprintf("Mean Total.Cup.Points, y-axis %.2f to %.2f", g1_lo, g1_hi),
                "misleading",
                sprintf("Drawn bar-height ratio %.1f to 1. The true ratio of the means is %.3f to 1.",
                        tv("G1_truncated_axis_details.csv", "visual height ratio as drawn"),
                        tv("G1_truncated_axis_details.csv", "true ratio of the means"))),
          panel("p_g1_hon", "Mean Total.Cup.Points, full 0-100 scale, 95% CI", "honest")),
      interp_block("G1"),
      tables_block(c("G1_truncated_axis_details.csv", "G1_debunk_group_means_with_n.csv",
                     "G_graph_type_inventory.csv"))),

    tabPanel("G2",
      section_head("G2", "Altitude versus quality scatterplot", "aggregation hiding individual variation"),
      two(panel("p_g2_mis", sprintf("%d band means (200 m bands)", nrow(g2_shown)), "misleading",
                sprintf("R-squared = %.3f on band means. Each dot stands for between %d and %d coffees.",
                        summary(g2_fit)$r.squared, min(g2_shown$n_observations),
                        max(g2_shown$n_observations)), height = 380),
          panel("p_g2_hon", sprintf("All %s individual coffees", format(nrow(d), big.mark = ",")),
                "honest", sprintf("R-squared = %.3f on individual coffees.", r1_r2), height = 380)),
      interp_block("G2"),
      tables_block(c("G2_altitude_band_means.csv", "G2_aggregation_details.csv",
                     "G2_debunk_individual_vs_aggregated.csv"))),

    tabPanel("G3",
      section_head("G3", "Histograms of quality by altitude group", "misleading histogram bin selection"),
      two(panel("p_g3_low", sprintf("Low group: %.1f-point bins, raw counts", g3_w), "misleading",
                sprintf("n = %d", n_low)),
          panel("p_g3_high", sprintf("High group: %.1f-point bins, raw counts", g3_w), "misleading",
                sprintf("n = %d. A different x-range and bin count than the low panel.", n_high))),
      panel("p_g3_matched", "Both groups on identical 1-point bins, as % of each group", "honest",
            height = 380),
      panel("p_g3_bins", "How the bin width was chosen: gap between modal bins", "honest",
            sprintf("Red = the %.1f-point width used in Part 1.", g3_w)),
      interp_block("G3"),
      tables_block(c("G3_histogram_details.csv", "G3_bin_width_investigation.csv",
                     "G3_automatic_bin_rules.csv", "G3_debunk_matched_bins.csv",
                     "G3_debunk_overlap.csv"))),

    tabPanel("R1",
      section_head("R1", "Simple linear regression: Total.Cup.Points ~ altitude_km",
                   "correlation interpreted as causation"),
      two(panel("p_r1_mis", sprintf("Fitted line, p = %s, n = %s", fmtp(r1_p),
                                    format(nrow(d), big.mark = ",")), "misleading",
                sprintf("The pitch: each extra kilometre of elevation causes a %.2f-point higher score.",
                        coef(r1)[2]), height = 380),
          panel("p_r1_pie", "Variance in score explained by altitude", "honest",
                sprintf("R-squared = %.4f. Residual SE %.3f vs score SD %.3f.",
                        r1_r2, r1_sum$sigma, sd(pts)), height = 380)),
      two(panel("p_r1_country", "Confounding: country means of altitude and score", "honest",
                "Bubble area scales with the number of coffees. Labels for countries with n >= 15."),
          panel("p_r1_resid", "R1 residuals, 1-point bins", "honest")),
      panel("p_r1_infl", "Altitude slope under influence checks", "honest"),
      interp_block("R1"),
      tables_block(c("R1_fitted_equation.csv", "R1_coefficients.csv", "R1_model_statistics.csv",
                     "R1_debunk_coefficients_with_ci.csv", "R1_debunk_explanatory_power.csv",
                     "R1_debunk_correlation_vs_causation.csv", "R1_debunk_residual_diagnostics.csv",
                     "R1_debunk_influence_sensitivity.csv"))),

    tabPanel("R2",
      section_head("R2", sprintf("Multiple linear regression (n = %d, identical rows)", cmp$n[1]),
                   "omitted-variable bias (Country.of.Origin left out)"),
      panel("p_r2_forest", "Altitude coefficient with 95% CI", "honest",
            sprintf("Adding country of origin changes the altitude effect by %.1f%%. It remains significant (p = %s).",
                    alt_change_pct, fmtp(cmp$altitude_p_value[2])), height = 260),
      two(panel("p_r2_fit", "Model fit: R-squared and adjusted R-squared", "honest"),
          panel("p_r2_noncountry", "Non-country coefficients, before vs after adding country", "honest")),
      panel("p_r2_country", "Country of origin effects in the corrected model (relative to Brazil)",
            "honest", "Holding altitude, moisture and processing constant.", height = 720),
      interp_block("R2"),
      tables_block(c("R2_fitted_equation.csv", "R2_coefficients.csv", "R2_model_statistics.csv",
                     "R2_omitted_variable_conditions.csv", "R2_country_confounding_evidence.csv",
                     "R2_debunk_model_comparison.csv", "R2_debunk_nested_test.csv",
                     "R2_debunk_fitted_equations.csv", "R2_debunk_all_coefficients.csv"))),

    tabPanel("Diagnostics",
      section_head("Diagnostics", "Regression diagnostics for R1, R2 and R2 corrected",
                   "none: these are limitations of the models themselves"),
      two(panel("p_diag_problems", "Problem observations by model", "honest"),
          panel("p_diag_skew", "Residual skewness and residual SE", "honest")),
      two(panel("p_diag_resid", "R1 residuals vs fitted", "honest"),
          panel("p_diag_cooks", "R1 Cook's distance by observation", "honest",
                sprintf("Dashed line = 4/n = %.4f. %d observations above it.",
                        cooks_cut, sum(cooks_flag)))),
      tables_block(c("DIAG_model_diagnostics_summary.csv", "DIAG_R1_top_influential_observations.csv"))),

    tabPanel("Verification",
      section_head("Step 10", "Verification checklist and run timings", "none"),
      panel("p_timings", "Seconds per pipeline script", "info", height = 300),
      tables_block(c("step10_verification.csv", "run_all_timings.csv"))),

    tabPanel("Data",
      fluidRow(
        column(3,
          div(class = "tbl-wrap",
            radioButtons("ds", "Dataset",
                         c("Analytical sample (cleaned)" = "ana", "Source file (every row)" = "src")),
            selectizeInput("f_country", "Country of origin", multiple = TRUE,
                           choices = sort(unique(src$Country.of.Origin)),
                           options = list(placeholder = "All countries")),
            checkboxGroupInput("f_group", "Altitude group", c("Low", "High", "Missing altitude"),
                               selected = c("Low", "High", "Missing altitude")),
            sliderInput("f_points", "Total.Cup.Points", min = 0, max = 100,
                        value = c(0, 100), step = 0.5),
            checkboxInput("f_hq", paste("Only high quality (", GE, "85 points)"), FALSE),
            selectizeInput("f_cols", "Columns", multiple = TRUE, choices = ana_cols,
                           selected = key_cols),
            actionLink("all_cols", "Show all columns"),
            hr(),
            numericInput("page_size", "Rows per page", 50, min = 10, max = 2000, step = 10),
            uiOutput("pager"),
            downloadButton("dl_data", "Download filtered rows (CSV)"))),
        column(9,
          div(class = "stats", uiOutput("data_stats", inline = TRUE)),
          div(class = "tbl-wrap", div(class = "tbl-scroll", tableOutput("data_table")))))),

    tabPanel("All tables",
      fluidRow(
        column(4, selectInput("pick_table", "Table", TABLE_FILES, width = "100%")),
        column(2, br(), downloadButton("dl_table", "Download CSV"))),
      div(class = "tbl-wrap", div(class = "tbl-scroll", tableOutput("any_table")))),

    tabPanel("Regression summaries",
      selectInput("pick_summary", "Summary file", SUMMARY_FILES, width = "40%"),
      tags$pre(class = "summary", textOutput("summary_text"))),

    tabPanel("Saved graphs",
      selectInput("pick_graph", "Graph written by run_all.R", GRAPH_FILES, width = "40%"),
      div(class = "graph-img", uiOutput("graph_img"))),

    tabPanel("Report",
      div(class = "report", uiOutput("report")))
  )
)

## ---------------------------------------------------------------------------
## Server
## ---------------------------------------------------------------------------
server <- function(input, output, session) {

  ## Base R stops with "figure margins too large" in a narrow window, so plots
  ## never draw narrower than 400 px and show a message if they still fail.
  for (nm in names(PLOTS)) local({
    f <- PLOTS[[nm]]
    id <- nm
    output[[id]] <- renderPlot({
      tryCatch(f(), error = function(e) {
        par(mar = c(0, 0, 0, 0))
        plot.new()
        text(0.5, 0.5, "Widen the window to draw this chart.", col = "grey40")
      })
    }, res = 96, width = function() {
      max(400, session$clientData[[paste0("output_", id, "_width")]] %||% 400)
    })
  })

  for (nm in TABLE_FILES) local({
    n <- nm
    output[[tbl_id(n)]] <- renderTable(fmt_df(TABLES[[n]]), striped = TRUE, hover = TRUE,
                                       bordered = TRUE, spacing = "xs", na = "")
  })

  ## Data explorer
  observeEvent(input$ds, {
    cols <- if (input$ds == "ana") ana_cols else src_cols
    keep <- intersect(key_cols, cols)
    updateSelectizeInput(session, "f_cols", choices = cols, selected = keep)
  })
  observeEvent(input$all_cols, {
    cols <- if (input$ds == "ana") ana_cols else src_cols
    updateSelectizeInput(session, "f_cols", selected = cols)
  })

  filtered <- reactive({
    x <- if (input$ds == "ana") d else src
    x$altitude_group <- as.character(x$altitude_group %||%
      ifelse(is.na(x$altitude_mean_meters), NA,
             ifelse(x$altitude_mean_meters >= 1500, "High", "Low")))
    grp_x <- ifelse(is.na(x$altitude_group), "Missing altitude", x$altitude_group)
    keep <- grp_x %in% input$f_group &
      x$Total.Cup.Points >= input$f_points[1] & x$Total.Cup.Points <= input$f_points[2]
    if (length(input$f_country)) keep <- keep & x$Country.of.Origin %in% input$f_country
    if (isTRUE(input$f_hq)) keep <- keep & x$Total.Cup.Points >= 85
    x[keep, , drop = FALSE]
  })

  n_pages <- reactive(max(1, ceiling(nrow(filtered()) / max(1, input$page_size %||% 50))))
  output$pager <- renderUI({
    numericInput("page", sprintf("Page (of %d)", n_pages()), 1, min = 1, max = n_pages())
  })

  output$data_stats <- renderUI({
    x <- filtered()
    tp <- x$Total.Cup.Points
    al <- x$altitude_mean_meters
    f1 <- function(v, k = 2) if (length(v) && any(!is.na(v))) fmt(v, k) else "-"
    tagList(
      stat(format(nrow(x), big.mark = ","), "Rows matching filters"),
      stat(f1(mean(tp, na.rm = TRUE), 3), "Mean Total.Cup.Points"),
      stat(f1(median(tp, na.rm = TRUE), 3), "Median Total.Cup.Points"),
      stat(f1(mean(al, na.rm = TRUE), 1), sprintf("Mean altitude (m), n = %d", sum(!is.na(al)))),
      stat(f1(median(al, na.rm = TRUE), 1), "Median altitude (m)"))
  })

  output$data_table <- renderTable({
    x <- filtered()
    cols <- intersect(input$f_cols, names(x))
    if (!length(cols)) cols <- names(x)
    ps <- max(1, input$page_size %||% 50)
    pg <- min(max(1, input$page %||% 1), n_pages())
    idx <- seq_len(nrow(x))
    idx <- idx[idx > (pg - 1) * ps & idx <= pg * ps]
    out <- x[idx, cols, drop = FALSE]
    cbind(row = idx, fmt_df(out))
  }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "xs", na = "")

  output$dl_data <- downloadHandler(
    filename = function() sprintf("coffee_%s_filtered.csv", input$ds),
    content = function(file) {
      x <- filtered()
      cols <- intersect(input$f_cols, names(x))
      write.csv(x[, if (length(cols)) cols else names(x), drop = FALSE], file, row.names = FALSE)
    })

  ## All tables, summaries, graphs, report
  output$any_table <- renderTable(fmt_df(TABLES[[input$pick_table]]), striped = TRUE,
                                  hover = TRUE, bordered = TRUE, spacing = "xs", na = "")
  output$dl_table <- downloadHandler(
    filename = function() input$pick_table,
    content = function(file) file.copy(file.path(table_dir, input$pick_table), file))

  output$summary_text <- renderText({
    req(input$pick_summary)
    paste(readLines(file.path(ROOT, "outputs", "regression_summaries", input$pick_summary),
                    warn = FALSE), collapse = "\n")
  })

  output$graph_img <- renderUI({
    req(input$pick_graph)
    tags$img(src = paste0("outputs/graphs/", input$pick_graph), alt = input$pick_graph)
  })

  output$report <- renderUI({
    if (!file.exists(REPORT_MD)) return(p("analysis_results.md not found. Run scripts/run_all.R."))
    md <- paste(readLines(REPORT_MD, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    HTML(shiny::markdown(md))
  })
}

app <- shinyApp(ui, server)
runApp(app, launch.browser = TRUE)
