## ---------------------------------------------------------------------------
## MA115 Project 1 -- "How to Lie with Statistics (and How to Catch It)"
## 05_part2_numerical.R
##
## PART 2 (honest) numerical corrections. Labels match Part 1 exactly.
##
##   N1 DEBUNK -- restores the spread that N1 suppressed
##   N2 DEBUNK -- restores the counts and the absolute difference
##   N3 DEBUNK -- restores every altitude band that N3 left out
##
## Run from the project directory:  Rscript scripts/05_part2_numerical.R
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
cat("PART 2: HONEST NUMERICAL ANALYSIS (N1-N3 DEBUNKS)\n")
cat("==========================================================\n")

low  <- dat$Total.Cup.Points[dat$altitude_group == "Low"]
high <- dat$Total.Cup.Points[dat$altitude_group == "High"]

## ---------------------------------------------------------------------------
## N1 DEBUNK -- the full distribution, not just the two means
## Part 1 technique corrected: reporting means without measures of spread.
## ---------------------------------------------------------------------------
describe <- function(x, label) {
  data.frame(
    label = "N1_debunk", group = label, n = length(x),
    mean = mean(x), median = median(x), sd = sd(x),
    iqr = IQR(x), q1 = unname(quantile(x, 0.25)), q3 = unname(quantile(x, 0.75)),
    min = min(x), max = max(x), range_width = diff(range(x)),
    stringsAsFactors = FALSE
  )
}
n1_desc <- rbind(describe(low, "Low (< 1500 m)"),
                 describe(high, "High (>= 1500 m)"),
                 describe(c(low, high), "All coffees"))
save_table(n1_desc, "N1_debunk_full_descriptives.csv")

## How much of the two distributions actually overlap.
n1_mean_low  <- mean(low)
n1_mean_high <- mean(high)
n1_diff      <- n1_mean_high - n1_mean_low
n1_pooled_sd <- sqrt(((length(low) - 1) * var(low) + (length(high) - 1) * var(high)) /
                       (length(low) + length(high) - 2))
n1_cohens_d  <- n1_diff / n1_pooled_sd
n1_low_above_high_mean  <- 100 * mean(low > n1_mean_high)
n1_high_below_low_mean  <- 100 * mean(high < n1_mean_low)
n1_tt <- t.test(high, low)

n1_overlap <- data.frame(
  label = "N1_debunk",
  quantity = c("difference in means (High - Low)",
               "pooled standard deviation",
               "difference as a fraction of 1 pooled SD (Cohen's d)",
               "difference as a percent of the observed score range",
               "percent of LOW-altitude coffees scoring above the HIGH mean",
               "percent of HIGH-altitude coffees scoring below the LOW mean",
               "Welch 95% CI lower bound for the difference",
               "Welch 95% CI upper bound for the difference",
               "Welch t-statistic", "Welch p-value"),
  value = c(n1_diff, n1_pooled_sd, n1_cohens_d,
            100 * n1_diff / diff(range(c(low, high))),
            n1_low_above_high_mean, n1_high_below_low_mean,
            n1_tt$conf.int[1], n1_tt$conf.int[2],
            unname(n1_tt$statistic), n1_tt$p.value),
  stringsAsFactors = FALSE
)
save_table(n1_overlap, "N1_debunk_overlap_and_effect_size.csv")

cat("\n----------------------------------------------------------\n")
cat("N1 DEBUNK  Full distributions, not just the means\n")
cat("----------------------------------------------------------\n")
print(n1_desc[, c("group", "n", "mean", "median", "sd", "iqr", "min", "max")],
      row.names = FALSE, digits = 4)
cat("\n")
print(n1_overlap[, c("quantity", "value")], row.names = FALSE, digits = 4)
cat("\n  HONEST INTERPRETATION:\n")
cat(sprintf("    The gap of %.2f points is about %.2f of one standard deviation,\n",
            n1_diff, n1_cohens_d))
cat(sprintf("    and the two groups span almost the same range (Low %.2f-%.2f,\n",
            min(low), max(low)))
cat(sprintf("    High %.2f-%.2f). %.1f%% of low-altitude coffees already score\n",
            min(high), max(high), n1_low_above_high_mean))
cat(sprintf("    above the high-altitude average and %.1f%% of high-altitude\n",
            n1_high_below_low_mean))
cat("    coffees score below the low-altitude average. A difference between\n")
cat("    group averages is not a ranking of individual coffees: picking one\n")
cat("    coffee from each group, the high-altitude one frequently loses.\n")

## ---------------------------------------------------------------------------
## N2 DEBUNK -- counts and absolute difference alongside the ratio
## Part 1 technique corrected: relative differences instead of absolute.
## ---------------------------------------------------------------------------
n2_n_low   <- length(low)
n2_n_high  <- length(high)
n2_hq_low  <- sum(dat$high_quality[dat$altitude_group == "Low"])
n2_hq_high <- sum(dat$high_quality[dat$altitude_group == "High"])
n2_p_low   <- n2_hq_low / n2_n_low
n2_p_high  <- n2_hq_high / n2_n_high
n2_rel_risk <- n2_p_high / n2_p_low
n2_abs_pp   <- 100 * (n2_p_high - n2_p_low)

n2_counts <- data.frame(
  label = "N2_debunk",
  altitude_group = c("Low (< 1500 m)", "High (>= 1500 m)"),
  n_high_quality = c(n2_hq_low, n2_hq_high),
  n_not_high_quality = c(n2_n_low - n2_hq_low, n2_n_high - n2_hq_high),
  n_total = c(n2_n_low, n2_n_high),
  pct_high_quality = 100 * c(n2_p_low, n2_p_high),
  pct_not_high_quality = 100 * (1 - c(n2_p_low, n2_p_high)),
  stringsAsFactors = FALSE
)
save_table(n2_counts, "N2_debunk_counts.csv")

## Number needed to treat: how many low-altitude coffees would have to become
## high-altitude to gain one extra high-quality coffee.
n2_nnt <- 1 / (n2_p_high - n2_p_low)
n2_prop_test <- prop.test(c(n2_hq_high, n2_hq_low), c(n2_n_high, n2_n_low))

n2_effect <- data.frame(
  label = "N2_debunk",
  measure = c("relative risk ratio (High / Low)",
              "relative increase (percent)",
              "absolute difference (percentage points)",
              "number needed to switch for one extra high-quality coffee",
              "percent of HIGH-altitude coffees that are NOT high quality",
              "95% CI lower bound, difference in proportions (pp)",
              "95% CI upper bound, difference in proportions (pp)",
              "chi-squared p-value for the difference in proportions"),
  value = c(n2_rel_risk, 100 * (n2_rel_risk - 1), n2_abs_pp, n2_nnt,
            100 * (1 - n2_p_high),
            100 * n2_prop_test$conf.int[1], 100 * n2_prop_test$conf.int[2],
            n2_prop_test$p.value),
  stringsAsFactors = FALSE
)
save_table(n2_effect, "N2_debunk_effect_measures.csv")

cat("\n----------------------------------------------------------\n")
cat("N2 DEBUNK  Counts and absolute difference restored\n")
cat("----------------------------------------------------------\n")
print(n2_counts[, c("altitude_group", "n_high_quality", "n_total",
                    "pct_high_quality", "pct_not_high_quality")],
      row.names = FALSE, digits = 4)
cat("\n")
print(n2_effect[, c("measure", "value")], row.names = FALSE, digits = 4)
cat("\n  HONEST INTERPRETATION:\n")
cat(sprintf("    The %.1f-times figure is arithmetically correct but it compares\n",
            n2_rel_risk))
cat(sprintf("    two small probabilities: %.2f%% against %.2f%%. In absolute terms\n",
            100 * n2_p_low, 100 * n2_p_high))
cat(sprintf("    the gain is %.2f percentage points, so about %.0f coffees would have\n",
            n2_abs_pp, n2_nnt))
cat("    to be moved from low to high altitude to produce one additional\n")
cat(sprintf("    high-quality lot. Meanwhile %.1f%% of high-altitude coffees still\n",
            100 * (1 - n2_p_high)))
cat("    fail to reach 85 points. A large ratio built on small baseline rates\n")
cat("    exaggerates the practical effect.\n")

## ---------------------------------------------------------------------------
## N3 DEBUNK -- every altitude band, not just the two that were shown
## Part 1 technique corrected: cherry-picking subgroups.
## ---------------------------------------------------------------------------
## (a) Every valid 200 m band across the whole retained altitude range.
n3_breaks_200 <- seq(100, 3500, by = 200)
band_200 <- cut(dat$altitude_mean_meters, breaks = n3_breaks_200,
                include.lowest = TRUE, right = FALSE)
n3_all_bands <- data.frame(
  label = "N3_debunk",
  band = levels(band_200),
  band_lower_m = head(n3_breaks_200, -1),
  band_upper_m = tail(n3_breaks_200, -1),
  n = as.integer(table(band_200)),
  mean_points = as.numeric(tapply(dat$Total.Cup.Points, band_200, mean)),
  median_points = as.numeric(tapply(dat$Total.Cup.Points, band_200, median)),
  sd_points = as.numeric(tapply(dat$Total.Cup.Points, band_200, sd)),
  stringsAsFactors = FALSE
)
save_table(n3_all_bands, "N3_debunk_all_200m_bands.csv")

## (b) Every 400 m band on a 200 m grid: the same band WIDTH that N3 chose, so
## the two cherry-picked bands can be seen in their full context.
band_lo <- c(800, 1200); band_hi <- c(1800, 2200)
win_starts <- seq(100, 3100, by = 200)
n3_windows <- do.call(rbind, lapply(win_starts, function(s) {
  sel <- dat$altitude_mean_meters >= s & dat$altitude_mean_meters < s + 400
  x <- dat$Total.Cup.Points[sel]
  data.frame(
    label = "N3_debunk",
    band = sprintf("%d-%d m", s, s + 400),
    band_lower_m = s, band_upper_m = s + 400,
    n = length(x),
    mean_points = if (length(x)) mean(x) else NA_real_,
    median_points = if (length(x)) median(x) else NA_real_,
    sd_points = if (length(x) > 1) sd(x) else NA_real_,
    used_in_N3 = (s == band_lo[1]) | (s == band_hi[1]),
    stringsAsFactors = FALSE
  )
}))
save_table(n3_windows, "N3_debunk_all_400m_bands.csv")

## (c) What the cherry-pick omitted: the chosen pair against every other pair
## of 400 m bands that both hold a reasonable number of coffees.
n3_min_n <- 30
elig <- n3_windows[!is.na(n3_windows$mean_points) & n3_windows$n >= n3_min_n, ]
pairs <- expand.grid(lo = seq_len(nrow(elig)), hi = seq_len(nrow(elig)))
pairs <- pairs[elig$band_lower_m[pairs$lo] < elig$band_lower_m[pairs$hi], ]
pair_diffs <- elig$mean_points[pairs$hi] - elig$mean_points[pairs$lo]

n3_chosen_diff <- mean(dat$Total.Cup.Points[dat$altitude_mean_meters >= band_hi[1] &
                                              dat$altitude_mean_meters < band_hi[2]]) -
                  mean(dat$Total.Cup.Points[dat$altitude_mean_meters >= band_lo[1] &
                                              dat$altitude_mean_meters < band_lo[2]])

n3_context <- data.frame(
  label = "N3_debunk",
  quantity = c("band pairs available (both bands n >= 30, lower vs higher)",
               "difference reported by N3",
               "mean difference across all available pairs",
               "median difference across all available pairs",
               "smallest difference across all available pairs",
               "largest difference across all available pairs",
               "percentile rank of the N3 pair among all pairs",
               "pairs where the HIGHER band scored WORSE",
               "percent of pairs where the HIGHER band scored WORSE"),
  value = c(length(pair_diffs), n3_chosen_diff,
            mean(pair_diffs), median(pair_diffs),
            min(pair_diffs), max(pair_diffs),
            100 * mean(pair_diffs <= n3_chosen_diff),
            sum(pair_diffs < 0), 100 * mean(pair_diffs < 0)),
  stringsAsFactors = FALSE
)
save_table(n3_context, "N3_debunk_cherrypick_context.csv")

## (d) Is the relationship monotone? Correlation between band midpoint and
## band mean, which the two-band comparison implicitly asserts.
valid_200 <- n3_all_bands[!is.na(n3_all_bands$mean_points), ]
n3_mono <- data.frame(
  label = "N3_debunk",
  quantity = c("200 m bands defined", "200 m bands containing data",
               "200 m bands with fewer than 10 coffees",
               "highest-scoring 200 m band (midpoint, m)",
               "mean of the highest-scoring 200 m band",
               "mean of the highest altitude band containing data",
               "Spearman correlation, band midpoint vs band mean"),
  value = c(nrow(n3_all_bands), nrow(valid_200),
            sum(valid_200$n < 10),
            (valid_200$band_lower_m[which.max(valid_200$mean_points)] +
               valid_200$band_upper_m[which.max(valid_200$mean_points)]) / 2,
            max(valid_200$mean_points),
            valid_200$mean_points[nrow(valid_200)],
            suppressWarnings(cor((valid_200$band_lower_m + valid_200$band_upper_m) / 2,
                                 valid_200$mean_points, method = "spearman"))),
  stringsAsFactors = FALSE
)
save_table(n3_mono, "N3_debunk_monotonicity.csv")

cat("\n----------------------------------------------------------\n")
cat("N3 DEBUNK  Every altitude band, including the ones omitted\n")
cat("----------------------------------------------------------\n")
cat("  Every valid 200 m band:\n")
print(n3_all_bands[, c("band", "n", "mean_points", "median_points", "sd_points")],
      row.names = FALSE, digits = 4)
cat("\n  Every 400 m band, the same width N3 used (* = band shown in N3):\n")
nw <- n3_windows
nw$flag <- ifelse(nw$used_in_N3, "*", "")
print(nw[, c("band", "n", "mean_points", "median_points", "sd_points", "flag")],
      row.names = FALSE, digits = 4)
cat("\n  The cherry-pick in context:\n")
print(n3_context[, c("quantity", "value")], row.names = FALSE, digits = 4)
cat("\n")
print(n3_mono[, c("quantity", "value")], row.names = FALSE, digits = 4)
cat("\n  HONEST INTERPRETATION:\n")
cat(sprintf("    N3 reported a %.2f point gap, which sits at the %.0fth percentile of\n",
            n3_chosen_diff, 100 * mean(pair_diffs <= n3_chosen_diff)))
cat(sprintf("    the %d band pairs that were available. ", length(pair_diffs)))
cat(sprintf("In %d of those pairs the\n", sum(pair_diffs < 0)))
cat("    higher band actually scored worse. The full band table also shows the\n")
cat("    relationship is not a steady climb: scores plateau through the middle\n")
cat("    altitudes and fall again in the sparse bands above roughly 2200 m,\n")
cat("    which the two selected bands conceal.\n")

saveRDS(list(
  n1 = list(desc = n1_desc, overlap = n1_overlap,
            diff = n1_diff, pooled_sd = n1_pooled_sd, cohens_d = n1_cohens_d,
            low_above_high_mean = n1_low_above_high_mean,
            high_below_low_mean = n1_high_below_low_mean,
            ci = n1_tt$conf.int, p = n1_tt$p.value,
            sd_low = sd(low), sd_high = sd(high),
            median_low = median(low), median_high = median(high),
            iqr_low = IQR(low), iqr_high = IQR(high),
            min_low = min(low), max_low = max(low),
            min_high = min(high), max_high = max(high)),
  n2 = list(counts = n2_counts, effect = n2_effect,
            nnt = n2_nnt, abs_pp = n2_abs_pp, rel_risk = n2_rel_risk,
            pct_high_not_hq = 100 * (1 - n2_p_high),
            ci = 100 * n2_prop_test$conf.int),
  n3 = list(all_bands = n3_all_bands, windows = n3_windows,
            context = n3_context, mono = n3_mono,
            chosen_diff = n3_chosen_diff,
            n_pairs = length(pair_diffs),
            pct_rank = 100 * mean(pair_diffs <= n3_chosen_diff),
            n_negative_pairs = sum(pair_diffs < 0),
            max_pair = max(pair_diffs), min_pair = min(pair_diffs),
            mean_pair = mean(pair_diffs), min_n = n3_min_n)
), file.path("outputs", "values", "05_part2_numerical.rds"))

cat("\n[OK] 05_part2_numerical.R complete\n")
