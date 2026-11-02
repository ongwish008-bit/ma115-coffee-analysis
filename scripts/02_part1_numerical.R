## ---------------------------------------------------------------------------
## MA115 Project 1 -- "How to Lie with Statistics (and How to Catch It)"
## 02_part1_numerical.R
##
## PART 1 (deliberately misleading) numerical calculations N1, N2, N3.
##
## Every number below is computed from the cleaned CSV. Nothing is fabricated:
## the deception is entirely in WHICH legitimate number is presented and which
## equally legitimate number is withheld.
##
##   N1 -- technique: reporting means without any measure of spread
##   N2 -- technique: relative differences instead of absolute differences
##   N3 -- technique: cherry-picking subgroups
##
## Run from the project directory:  Rscript scripts/02_part1_numerical.R
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
for (d in c("outputs/tables", "outputs/values")) if (!dir.exists(d)) dir.create(d, recursive = TRUE)

save_table <- function(x, name) {
  write.csv(x, file.path("outputs", "tables", name), row.names = FALSE)
}

dat <- read.csv(clean_path, stringsAsFactors = FALSE)
dat$altitude_group <- factor(dat$altitude_group, levels = c("Low", "High"))
dat$high_quality   <- as.logical(dat$high_quality)

cat("\n==========================================================\n")
cat("PART 1: MISLEADING NUMERICAL CALCULATIONS (N1-N3)\n")
cat("Headline claim being sold:\n")
cat("  \"Higher-altitude coffee is scientifically proven to taste better.\"\n")
cat("==========================================================\n")

low  <- dat$Total.Cup.Points[dat$altitude_group == "Low"]
high <- dat$Total.Cup.Points[dat$altitude_group == "High"]

## ---------------------------------------------------------------------------
## N1 -- COMPARING AVERAGE QUALITY
## Technique: reporting means without measures of spread.
## ---------------------------------------------------------------------------
n1_mean_low  <- mean(low)
n1_mean_high <- mean(high)
n1_diff      <- n1_mean_high - n1_mean_low

n1_tbl <- data.frame(
  label = "N1",
  altitude_group = c("Low (< 1500 m)", "High (>= 1500 m)", "difference (High - Low)"),
  mean_total_cup_points = c(n1_mean_low, n1_mean_high, n1_diff),
  stringsAsFactors = FALSE
)
save_table(n1_tbl, "N1_misleading_means.csv")

## The full computation is retained so Part 2 can expose exactly what was
## withheld here (the spread, the overlap and the sample sizes).
n1_full <- data.frame(
  label = "N1",
  altitude_group = c("Low", "High"),
  n = c(length(low), length(high)),
  mean = c(n1_mean_low, n1_mean_high),
  sd = c(sd(low), sd(high)),
  min = c(min(low), min(high)),
  max = c(max(low), max(high)),
  stringsAsFactors = FALSE
)
save_table(n1_full, "N1_full_computation.csv")

cat("\n----------------------------------------------------------\n")
cat("N1  Mean quality score by altitude group\n")
cat("----------------------------------------------------------\n")
cat("  Technique ........ reporting means without measures of spread\n")
cat(sprintf("  Mean below 1500 m ............. %.3f cup points\n", n1_mean_low))
cat(sprintf("  Mean at or above 1500 m ....... %.3f cup points\n", n1_mean_high))
cat(sprintf("  Difference in means ........... %.3f cup points\n", n1_diff))
cat("\n  MISLEADING PRESENTATION (the only sentence Part 1 reports):\n")
cat(sprintf("    \"High-altitude coffee averages %.2f cup points against %.2f for\n",
            n1_mean_high, n1_mean_low))
cat(sprintf("     low-altitude coffee -- a clear %.2f point quality advantage.\"\n", n1_diff))
cat("\n  Withheld on purpose: both standard deviations, both ranges, the\n")
cat("  overlap of the two distributions, and the group sample sizes.\n")

## ---------------------------------------------------------------------------
## N2 -- HIGH-QUALITY COFFEE PROBABILITIES
## Technique: relative differences instead of absolute differences.
## "High quality" is defined by the assignment as Total.Cup.Points >= 85.
## ---------------------------------------------------------------------------
n2_n_low   <- length(low)
n2_n_high  <- length(high)
n2_hq_low  <- sum(dat$high_quality[dat$altitude_group == "Low"])
n2_hq_high <- sum(dat$high_quality[dat$altitude_group == "High"])
n2_pct_low  <- 100 * n2_hq_low / n2_n_low
n2_pct_high <- 100 * n2_hq_high / n2_n_high
n2_rel_risk <- (n2_hq_high / n2_n_high) / (n2_hq_low / n2_n_low)
n2_abs_pp   <- n2_pct_high - n2_pct_low
n2_pct_increase <- 100 * (n2_rel_risk - 1)

n2_full <- data.frame(
  label = "N2",
  altitude_group = c("Low (< 1500 m)", "High (>= 1500 m)"),
  n_high_quality = c(n2_hq_low, n2_hq_high),
  n_total = c(n2_n_low, n2_n_high),
  pct_high_quality = c(n2_pct_low, n2_pct_high),
  stringsAsFactors = FALSE
)
save_table(n2_full, "N2_full_computation.csv")

n2_effect <- data.frame(
  label = "N2",
  measure = c("relative risk ratio (High / Low)",
              "relative increase (percent)",
              "absolute difference (percentage points)"),
  value = c(n2_rel_risk, n2_pct_increase, n2_abs_pp),
  stringsAsFactors = FALSE
)
save_table(n2_effect, "N2_effect_measures.csv")

## The misleading presentation keeps ONLY the ratio row.
save_table(n2_effect[1:2, ], "N2_misleading_presentation.csv")

cat("\n----------------------------------------------------------\n")
cat("N2  Probability of a high-quality coffee (>= 85 points)\n")
cat("----------------------------------------------------------\n")
cat("  Technique ........ relative differences instead of absolute differences\n")
cat(sprintf("  Low  altitude ... %d of %d high quality (%.2f%%)\n",
            n2_hq_low, n2_n_low, n2_pct_low))
cat(sprintf("  High altitude ... %d of %d high quality (%.2f%%)\n",
            n2_hq_high, n2_n_high, n2_pct_high))
cat(sprintf("  Relative risk ratio ........... %.3f\n", n2_rel_risk))
cat(sprintf("  Absolute difference ........... %.2f percentage points\n", n2_abs_pp))
cat("\n  MISLEADING PRESENTATION (the only sentence Part 1 reports):\n")
cat(sprintf("    \"Coffee grown at or above 1500 m is %.1f times more likely to be\n",
            n2_rel_risk))
cat(sprintf("     rated high quality -- a %.0f%% increase in the chance of excellence.\"\n",
            n2_pct_increase))
cat("\n  Withheld on purpose: the counts and denominators. Both percentages are\n")
cat("  small, so the impressive ratio rests on a modest absolute gap.\n")

## ---------------------------------------------------------------------------
## N3 -- SELECTIVE ALTITUDE COMPARISON
## Technique: cherry-picking subgroups.
##
## The two bands below are chosen BECAUSE they maximise the apparent gap while
## still being describable as "typical low" and "typical high" altitude. Every
## other band in the data is simply not mentioned (Part 2 lists them all).
## ---------------------------------------------------------------------------
band_lo_lower <- 800;  band_lo_upper <- 1200
band_hi_lower <- 1800; band_hi_upper <- 2200

sel_lo <- dat$altitude_mean_meters >= band_lo_lower & dat$altitude_mean_meters < band_lo_upper
sel_hi <- dat$altitude_mean_meters >= band_hi_lower & dat$altitude_mean_meters < band_hi_upper

n3_low_scores  <- dat$Total.Cup.Points[sel_lo]
n3_high_scores <- dat$Total.Cup.Points[sel_hi]
n3_mean_low  <- mean(n3_low_scores)
n3_mean_high <- mean(n3_high_scores)
n3_diff      <- n3_mean_high - n3_mean_low

n3_tbl <- data.frame(
  label = "N3",
  band = c(sprintf("%d m <= altitude < %d m", band_lo_lower, band_lo_upper),
           sprintf("%d m <= altitude < %d m", band_hi_lower, band_hi_upper),
           "difference (high band - low band)"),
  n = c(length(n3_low_scores), length(n3_high_scores), NA),
  mean_total_cup_points = c(n3_mean_low, n3_mean_high, n3_diff),
  stringsAsFactors = FALSE
)
save_table(n3_tbl, "N3_cherrypicked_bands.csv")

## Share of the analytical sample that the cherry-pick quietly discards.
n3_coverage <- data.frame(
  label = "N3",
  quantity = c("observations in the two selected bands",
               "observations in the analytical sample",
               "percent of the sample actually used",
               "percent of the sample silently discarded"),
  value = c(length(n3_low_scores) + length(n3_high_scores), nrow(dat),
            100 * (length(n3_low_scores) + length(n3_high_scores)) / nrow(dat),
            100 - 100 * (length(n3_low_scores) + length(n3_high_scores)) / nrow(dat)),
  stringsAsFactors = FALSE
)
save_table(n3_coverage, "N3_coverage.csv")

cat("\n----------------------------------------------------------\n")
cat("N3  Selected altitude bands\n")
cat("----------------------------------------------------------\n")
cat("  Technique ........ cherry-picking subgroups\n")
cat(sprintf("  %4d-%4d m ... n = %3d, mean = %.3f\n",
            band_lo_lower, band_lo_upper, length(n3_low_scores), n3_mean_low))
cat(sprintf("  %4d-%4d m ... n = %3d, mean = %.3f\n",
            band_hi_lower, band_hi_upper, length(n3_high_scores), n3_mean_high))
cat(sprintf("  Difference in means ........... %.3f cup points\n", n3_diff))
cat("\n  MISLEADING PRESENTATION (the only sentence Part 1 reports):\n")
cat(sprintf("    \"Comparing representative growing altitudes, coffee from %d-%d m\n",
            band_hi_lower, band_hi_upper))
cat(sprintf("     scores %.2f points higher than coffee from %d-%d m.\"\n",
            n3_diff, band_lo_lower, band_lo_upper))
cat(sprintf("\n  Withheld on purpose: these two bands hold only %.1f%% of the sample\n",
            n3_coverage$value[3]))
cat("  and the bands in between and above are never shown.\n")

## ---------------------------------------------------------------------------
## Hand key values to the report generator
## ---------------------------------------------------------------------------
saveRDS(list(
  n1 = list(mean_low = n1_mean_low, mean_high = n1_mean_high, diff = n1_diff,
            n_low = n2_n_low, n_high = n2_n_high),
  n2 = list(hq_low = n2_hq_low, hq_high = n2_hq_high,
            n_low = n2_n_low, n_high = n2_n_high,
            pct_low = n2_pct_low, pct_high = n2_pct_high,
            rel_risk = n2_rel_risk, pct_increase = n2_pct_increase,
            abs_pp = n2_abs_pp),
  n3 = list(band_lo = c(band_lo_lower, band_lo_upper),
            band_hi = c(band_hi_lower, band_hi_upper),
            n_lo = length(n3_low_scores), n_hi = length(n3_high_scores),
            mean_lo = n3_mean_low, mean_hi = n3_mean_high, diff = n3_diff,
            pct_used = n3_coverage$value[3], pct_discarded = n3_coverage$value[4])
), file.path("outputs", "values", "02_part1_numerical.rds"))

cat("\n[OK] 02_part1_numerical.R complete\n")
