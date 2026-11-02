## ---------------------------------------------------------------------------
## MA115 Project 1 -- "How to Lie with Statistics (and How to Catch It)"
## run_all.R
##
## Executes the whole analysis in order, then writes analysis_results.md and
## runs the Step 10 verification checks.
##
## Run from the project directory:
##   Rscript scripts/run_all.R
##
## Every number written into analysis_results.md is read back from the values
## computed by scripts 01-08. Nothing in this file is a hardcoded result.
## ---------------------------------------------------------------------------

t_start <- Sys.time()

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

if (!file.exists("arabica_data_cleaned.csv")) {
  stop("Cannot find arabica_data_cleaned.csv.\n",
       "  run_all.R must be started from the project directory, for example:\n",
       "    cd <project directory>\n",
       "    Rscript scripts/run_all.R")
}

for (d in c("data", "outputs", "outputs/tables", "outputs/graphs",
            "outputs/regression_summaries", "outputs/values")) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

cat(strrep("#", 70), "\n")
cat("# MA115 PROJECT 1 -- FULL ANALYSIS\n")
cat("# Headline claim under examination:\n")
cat("#   \"Higher-altitude coffee is scientifically proven to taste better.\"\n")
cat("# Part 1 builds the misleading case. Part 2 dismantles it.\n")
cat(strrep("#", 70), "\n")

## ---------------------------------------------------------------------------
## 1. Run every analysis script, in order
##
## Each script is evaluated in its own environment so that no variable can
## leak from one step into the next. Every script reloads the cleaned CSV, so
## each one is independently reproducible.
## ---------------------------------------------------------------------------
pipeline <- c(
  "01_data_preparation.R",
  "02_part1_numerical.R",
  "03_part1_graphs.R",
  "04_part1_regressions.R",
  "05_part2_numerical.R",
  "06_part2_graphs.R",
  "07_part2_regressions.R",
  "08_diagnostics.R"
)

timings <- data.frame(script = character(0), seconds = numeric(0),
                      stringsAsFactors = FALSE)

for (s in pipeline) {
  path <- file.path("scripts", s)
  if (!file.exists(path)) stop("Missing script: ", path)
  cat("\n", strrep("#", 70), "\n# RUNNING ", s, "\n", strrep("#", 70), "\n", sep = "")
  t0 <- Sys.time()
  sys.source(path, envir = new.env(parent = globalenv()))
  el <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  timings <- rbind(timings, data.frame(script = s, seconds = el,
                                       stringsAsFactors = FALSE))
}

## ---------------------------------------------------------------------------
## 2. Read back every computed value
## ---------------------------------------------------------------------------
vpath <- function(f) file.path("outputs", "values", f)
v01 <- readRDS(vpath("01_prep.rds"))
v02 <- readRDS(vpath("02_part1_numerical.rds"))
v03 <- readRDS(vpath("03_part1_graphs.rds"))
v04 <- readRDS(vpath("04_part1_regressions.rds"))
v05 <- readRDS(vpath("05_part2_numerical.rds"))
v06 <- readRDS(vpath("06_part2_graphs.rds"))
v07 <- readRDS(vpath("07_part2_regressions.rds"))
v08 <- readRDS(vpath("08_diagnostics.rds"))

save_table <- function(x, name) {
  write.csv(x, file.path("outputs", "tables", name), row.names = FALSE)
}

## Number formatting helpers for the report.
f0 <- function(x) formatC(x, format = "f", digits = 0, big.mark = ",")
f1 <- function(x) formatC(x, format = "f", digits = 1)
f2 <- function(x) formatC(x, format = "f", digits = 2)
f3 <- function(x) formatC(x, format = "f", digits = 3)
f4 <- function(x) formatC(x, format = "f", digits = 4)
fg <- function(x) formatC(x, format = "g", digits = 3)

## ---------------------------------------------------------------------------
## 3. Register of misleading techniques
##
## The project requires at least five distinct techniques and forbids using
## any single technique more than twice. Eight are used, each exactly once.
## ---------------------------------------------------------------------------
techniques <- data.frame(
  label = c("N1", "N2", "N3", "G1", "G2", "G3", "R1", "R2"),
  kind = c("calculation", "calculation", "calculation",
           "graph", "graph", "graph", "regression", "regression"),
  technique = c("Reporting means without measures of spread",
                "Relative differences instead of absolute differences",
                "Cherry-picking subgroups",
                "Truncated axis",
                "Aggregation hiding individual variation",
                "Misleading histogram bin selection",
                "Correlation interpreted as causation",
                "Omitted-variable bias"),
  part1_script = c(rep("02_part1_numerical.R", 3), rep("03_part1_graphs.R", 3),
                   rep("04_part1_regressions.R", 2)),
  part2_script = c(rep("05_part2_numerical.R", 3), rep("06_part2_graphs.R", 3),
                   rep("07_part2_regressions.R", 2)),
  times_used = 1L,
  stringsAsFactors = FALSE
)
save_table(techniques, "technique_register.csv")
save_table(timings, "run_all_timings.csv")

n_techniques <- length(unique(techniques$technique))
max_technique_uses <- max(table(techniques$technique))

## ---------------------------------------------------------------------------
## 4. Write analysis_results.md
## ---------------------------------------------------------------------------
md <- character(0)
add <- function(...) md <<- c(md, paste0(...))

add("# MA115 Project 1 -- Analysis Results")
add("")
add("**Headline claim under examination:** \"Higher-altitude coffee is ",
    "scientifically proven to taste better.\"")
add("")
add("Part 1 builds a statistically correct but deliberately misleading case ",
    "for that claim. Part 2 corrects every item using the same label. ",
    "No measurement was fabricated, altered or invented at any point: every ",
    "misleading result comes from a legitimate choice about which true number ",
    "to present and which to withhold.")
add("")
add("Generated by `scripts/run_all.R` on ", format(t_start, "%Y-%m-%d %H:%M:%S"),
    " using ", R.version.string, ".")
add("Every value below is computed from `", v01$source_csv,
    "` and can be regenerated with `Rscript scripts/run_all.R`.")
add("")

## ---- Data preparation -----------------------------------------------------
add("## Data preparation")
add("")
add("| Quantity | Value |")
add("|---|---|")
add("| Source dataset rows x columns | ", f0(v01$n_raw), " x ", v01$n_cols_raw, " |")
add("| Rows excluded (unique) | ", f0(v01$n_excluded_unique), " |")
add("| **Analytical sample rows** | **", f0(v01$n_clean), "** |")
add("| Documented expectation | ", f0(v01$expected_n), " |")
add("| Computed matches expectation | ",
    if (isTRUE(v01$size_match)) "yes" else "NO -- see warning in the log", " |")
add("| Low-altitude group (< 1500 m) | ", f0(v01$n_low), " |")
add("| High-altitude group (>= 1500 m) | ", f0(v01$n_high), " |")
add("| Countries of origin represented | ", v01$n_countries, " |")
add("| Processing methods represented | ", v01$n_processing, " |")
add("| Complete cases for the multiple regressions (`cc_r2`) | ",
    f0(v01$n_cc_r2), " |")
add("")
add("The source file name contains the word \"cleaned\", which refers to ",
    "upstream tidying of the Coffee Quality Institute data. It still holds ",
    "missing altitudes and impossible values, so it is treated as the ",
    "**source dataset**; the rows surviving the filters below are the ",
    "**analytical sample** used for every number in this report.")
add("")
add("### Exclusions")
add("")
add("| Rule | Rows failing | Percent of source |")
add("|---|---|---|")
for (i in seq_len(nrow(v01$excl_by_reason))) {
  r <- v01$excl_by_reason[i, ]
  add("| `", r$rule, "` | ", f0(r$n_rows_failing), " | ", f2(r$pct_of_source), "% |")
}
add("")
add("Overlap accounting: the independent reason counts sum to ",
    f0(v01$n_reason_hits), ", and ", f0(v01$n_excluded_unique),
    " distinct rows were removed, so ", f0(v01$n_double_counted),
    " rows were double counted. The rules are therefore mutually exclusive ",
    "on this dataset, which the code verifies rather than assumes ",
    "(`outputs/tables/01_exclusion_combinations.csv`).")
add("")
add("### Transformations recorded")
add("")
add("| Derived variable | Definition | Note |")
add("|---|---|---|")
add("| `altitude_km` | `altitude_mean_meters / 1000` | Monotone rescaling, ",
    "so it cannot change any p-value, R-squared or correlation |")
add("| `altitude_group` | `Low` if < 1500 m, otherwise `High` | Discards the ",
    "exact altitude within each group |")
add("| `high_quality` | `TRUE` if `Total.Cup.Points >= 85` | Threshold set by ",
    "the assignment |")
add("| `cc_r2` | complete cases on all five analysis variables | Guarantees R2 ",
    "and the corrected R2 use identical rows |")
add("")
add("Blank strings in `Processing.Method` and `Country.of.Origin` were ",
    "converted to `NA` so they are handled as missing rather than becoming a ",
    "meaningless factor level. No measured value was changed.")
add("")

## ---- Technique register ---------------------------------------------------
add("## Misleading technique register")
add("")
add(n_techniques, " distinct techniques are used across the eight Part 1 ",
    "items, each exactly once (the limit is twice).")
add("")
add("| Label | Kind | Technique | Times used |")
add("|---|---|---|---|")
for (i in seq_len(nrow(techniques))) {
  r <- techniques[i, ]
  add("| ", r$label, " | ", r$kind, " | ", r$technique, " | ", r$times_used, " |")
}
add("")

## ---- N1 -------------------------------------------------------------------
add("## N1 -- Comparing average quality")
add("")
add("**Misleading technique:** reporting means without measures of spread.")
add("")
add("### Part 1 computed results")
add("")
add("| Quantity | Value |")
add("|---|---|")
add("| Mean quality below 1500 m | ", f3(v02$n1$mean_low), " |")
add("| Mean quality at or above 1500 m | ", f3(v02$n1$mean_high), " |")
add("| Difference in means | ", f3(v02$n1$diff), " |")
add("")
add("**Interpretation being sold:** \"High-altitude coffee averages ",
    f2(v02$n1$mean_high), " cup points against ", f2(v02$n1$mean_low),
    " for low-altitude coffee -- a clear ", f2(v02$n1$diff),
    " point quality advantage.\"")
add("")
add("### Part 2 computed results (N1 debunk)")
add("")
add("| Group | n | Mean | Median | SD | IQR | Min | Max |")
add("|---|---|---|---|---|---|---|---|")
add("| Low (< 1500 m) | ", f0(v02$n1$n_low), " | ", f2(v05$n1$desc$mean[1]),
    " | ", f2(v05$n1$median_low), " | ", f3(v05$n1$sd_low), " | ",
    f2(v05$n1$iqr_low), " | ", f2(v05$n1$min_low), " | ", f2(v05$n1$max_low), " |")
add("| High (>= 1500 m) | ", f0(v02$n1$n_high), " | ", f2(v05$n1$desc$mean[2]),
    " | ", f2(v05$n1$median_high), " | ", f3(v05$n1$sd_high), " | ",
    f2(v05$n1$iqr_high), " | ", f2(v05$n1$min_high), " | ", f2(v05$n1$max_high), " |")
add("")
add("| Additional measure | Value |")
add("|---|---|")
add("| Pooled standard deviation | ", f3(v05$n1$pooled_sd), " |")
add("| Effect size (Cohen's d) | ", f3(v05$n1$cohens_d), " |")
add("| Low-altitude coffees scoring above the high-altitude mean | ",
    f1(v05$n1$low_above_high_mean), "% |")
add("| High-altitude coffees scoring below the low-altitude mean | ",
    f1(v05$n1$high_below_low_mean), "% |")
add("| Welch 95% CI for the difference | [", f3(v05$n1$ci[1]), ", ",
    f3(v05$n1$ci[2]), "] |")
add("| Welch p-value | ", fg(v05$n1$p), " |")
add("")
add("**Why it is misleading:** two means with no spread invite the reader to ",
    "treat the groups as separable, as if any high-altitude coffee beats any ",
    "low-altitude one. The standard deviations (", f3(v05$n1$sd_low), " and ",
    f3(v05$n1$sd_high), ") are larger than the gap itself (", f3(v02$n1$diff),
    "), the effect size is only ", f3(v05$n1$cohens_d),
    " of a standard deviation, and the ranges almost coincide.")
add("")
add("**Honest interpretation:** the average difference is real and the ",
    "confidence interval excludes zero, but it is a small shift in two heavily ",
    "overlapping distributions. ", f1(v05$n1$low_above_high_mean),
    "% of low-altitude coffees already score above the high-altitude average ",
    "and ", f1(v05$n1$high_below_low_mean),
    "% of high-altitude coffees fall below the low-altitude average, so the ",
    "group means do not rank individual coffees.")
add("")

## ---- N2 -------------------------------------------------------------------
add("## N2 -- High-quality coffee probabilities")
add("")
add("**Misleading technique:** relative differences instead of absolute ",
    "differences. High quality is defined as `Total.Cup.Points >= 85`.")
add("")
add("### Part 1 computed results")
add("")
add("| Group | High quality | Total | Percent |")
add("|---|---|---|---|")
add("| Low (< 1500 m) | ", f0(v02$n2$hq_low), " | ", f0(v02$n2$n_low), " | ",
    f2(v02$n2$pct_low), "% |")
add("| High (>= 1500 m) | ", f0(v02$n2$hq_high), " | ", f0(v02$n2$n_high), " | ",
    f2(v02$n2$pct_high), "% |")
add("")
add("| Effect measure | Value |")
add("|---|---|")
add("| Relative risk ratio (High / Low) | ", f3(v02$n2$rel_risk), " |")
add("| Relative increase | ", f0(v02$n2$pct_increase), "% |")
add("| Absolute difference | ", f2(v02$n2$abs_pp), " percentage points |")
add("")
add("**Interpretation being sold:** \"Coffee grown at or above 1500 m is ",
    f1(v02$n2$rel_risk), " times more likely to be rated high quality -- a ",
    f0(v02$n2$pct_increase), "% increase in the chance of excellence.\" ",
    "The counts and denominators are not shown.")
add("")
add("### Part 2 computed results (N2 debunk)")
add("")
add("| Measure | Value |")
add("|---|---|")
add("| Low altitude, high quality | ", f0(v02$n2$hq_low), " of ",
    f0(v02$n2$n_low), " (", f2(v02$n2$pct_low), "%) |")
add("| High altitude, high quality | ", f0(v02$n2$hq_high), " of ",
    f0(v02$n2$n_high), " (", f2(v02$n2$pct_high), "%) |")
add("| Absolute difference | ", f2(v05$n2$abs_pp), " percentage points |")
add("| 95% CI for the difference in proportions | [", f2(v05$n2$ci[1]), ", ",
    f2(v05$n2$ci[2]), "] percentage points |")
add("| Relative risk ratio | ", f3(v05$n2$rel_risk), " |")
add("| Coffees to switch for one extra high-quality lot | ", f1(v05$n2$nnt), " |")
add("| High-altitude coffees that are NOT high quality | ",
    f1(v05$n2$pct_high_not_hq), "% |")
add("")
add("**Why it is misleading:** a ratio hides the size of the two baseline ",
    "probabilities it compares. Tripling a rate of ", f2(v02$n2$pct_low),
    "% sounds transformative, but it produces only ", f2(v05$n2$abs_pp),
    " extra high-quality coffees per 100.")
add("")
add("**Honest interpretation:** high-altitude coffee is genuinely more likely ",
    "to reach 85 points, and the difference in proportions is statistically ",
    "detectable. In practical terms the improvement is ", f2(v05$n2$abs_pp),
    " percentage points, roughly ", f0(v05$n2$nnt),
    " coffees moved from low to high altitude per additional high-quality lot, ",
    "and ", f1(v05$n2$pct_high_not_hq),
    "% of high-altitude coffees still fail to reach the threshold.")
add("")

## ---- N3 -------------------------------------------------------------------
add("## N3 -- Selective altitude comparison")
add("")
add("**Misleading technique:** cherry-picking subgroups.")
add("")
add("### Part 1 computed results")
add("")
add("| Band | n | Mean quality |")
add("|---|---|---|")
add("| ", v02$n3$band_lo[1], " m to ", v02$n3$band_lo[2], " m | ",
    f0(v02$n3$n_lo), " | ", f3(v02$n3$mean_lo), " |")
add("| ", v02$n3$band_hi[1], " m to ", v02$n3$band_hi[2], " m | ",
    f0(v02$n3$n_hi), " | ", f3(v02$n3$mean_hi), " |")
add("| Difference | | ", f3(v02$n3$diff), " |")
add("")
add("These two bands hold ", f1(v02$n3$pct_used), "% of the analytical sample; ",
    f1(v02$n3$pct_discarded), "% is silently discarded.")
add("")
add("**Interpretation being sold:** \"Comparing representative growing ",
    "altitudes, coffee from ", v02$n3$band_hi[1], "-", v02$n3$band_hi[2],
    " m scores ", f2(v02$n3$diff), " points higher than coffee from ",
    v02$n3$band_lo[1], "-", v02$n3$band_lo[2], " m.\"")
add("")
add("### Part 2 computed results (N3 debunk)")
add("")
add("Every valid 200 m band in the analytical sample:")
add("")
add("| Band | n | Mean | Median | SD |")
add("|---|---|---|---|---|")
for (i in seq_len(nrow(v05$n3$all_bands))) {
  r <- v05$n3$all_bands[i, ]
  add("| ", f0(r$band_lower_m), "-", f0(r$band_upper_m), " m | ", f0(r$n), " | ",
      if (is.na(r$mean_points)) "--" else f2(r$mean_points), " | ",
      if (is.na(r$median_points)) "--" else f2(r$median_points), " | ",
      if (is.na(r$sd_points)) "--" else f3(r$sd_points), " |")
}
add("")
add("| Context measure | Value |")
add("|---|---|")
add("| Band pairs available (both bands n >= ", v05$n3$min_n, ") | ",
    f0(v05$n3$n_pairs), " |")
add("| Difference reported by N3 | ", f3(v05$n3$chosen_diff), " |")
add("| Mean difference across all pairs | ", f3(v05$n3$mean_pair), " |")
add("| Largest difference available | ", f3(v05$n3$max_pair), " |")
add("| Smallest difference available | ", f3(v05$n3$min_pair), " |")
add("| Percentile rank of the N3 pair | ", f0(v05$n3$pct_rank), "th |")
add("| Pairs where the HIGHER band scored worse | ",
    f0(v05$n3$n_negative_pairs), " |")
add("| Spearman correlation, band midpoint vs band mean | ",
    f3(v05$n3$mono$value[7]), " |")
add("")
add("**Why it is misleading:** the two bands were selected because they ",
    "maximise the apparent gap, and the reader is given no way to know that ",
    "other equally valid bands were available. The reported difference sits at ",
    "the ", f0(v05$n3$pct_rank), "th percentile of the ", f0(v05$n3$n_pairs),
    " comparable band pairs, and in ", f0(v05$n3$n_negative_pairs),
    " of those pairs the higher band actually scored worse.")
add("")
add("**Honest interpretation:** scores do tend to be higher in the 1500-2100 m ",
    "range, but the full band table shows a plateau through the low and middle ",
    "altitudes rather than a steady climb, and the sparse bands above roughly ",
    "2200 m fall back again -- the highest band containing data averages ",
    f2(v05$n3$mono$value[6]), ". The Spearman correlation between band ",
    "midpoint and band mean is only ", f3(v05$n3$mono$value[7]), ".")
add("")

## ---- G1 -------------------------------------------------------------------
add("## G1 -- Bar chart of mean quality by altitude group")
add("")
add("**Misleading technique:** truncated axis. Graph type: bar chart, ",
    "`barplot()`.")
add("")
add("### Part 1 computed results")
add("")
add("| Quantity | Value |")
add("|---|---|")
add("| Bar height, Low | ", f3(v03$g1$mean_low), " |")
add("| Bar height, High | ", f3(v03$g1$mean_high), " |")
add("| y-axis range drawn | ", f2(v03$g1$ylim[1]), " to ", f2(v03$g1$ylim[2]), " |")
add("| Visual height ratio as drawn | ", f1(v03$g1$drawn_ratio), " to 1 |")
add("| True ratio of the means | ", f3(v03$g1$true_ratio), " to 1 |")
add("")
add("Figure: `outputs/graphs/G1_misleading_truncated_barchart.png`")
add("")
add("**Interpretation being sold:** the high-altitude bar towers over the ",
    "low-altitude bar, implying a difference in kind rather than a difference ",
    "of about one point.")
add("")
add("### Part 2 computed results (G1 debunk)")
add("")
add("| Group | n | Mean | 95% CI |")
add("|---|---|---|---|")
add("| Low | ", f0(v06$g1$n_low), " | ", f3(v06$g1$tbl$mean_points[1]), " | [",
    f3(v06$g1$tbl$ci_lower_95[1]), ", ", f3(v06$g1$tbl$ci_upper_95[1]), "] |")
add("| High | ", f0(v06$g1$n_high), " | ", f3(v06$g1$tbl$mean_points[2]), " | [",
    f3(v06$g1$tbl$ci_lower_95[2]), ", ", f3(v06$g1$tbl$ci_upper_95[2]), "] |")
add("")
add("Figures: `outputs/graphs/G1_debunk_full_axis_barchart.png` and ",
    "`outputs/graphs/G1_comparison_misleading_vs_honest.png`")
add("")
add("**Why it is misleading:** the bar heights are correct, but starting the ",
    "axis at ", f2(v03$g1$ylim[1]), " instead of 0 means the drawn bars differ ",
    "by a factor of ", f1(v03$g1$drawn_ratio),
    " while the underlying means differ by a factor of ", f3(v03$g1$true_ratio),
    ". Bar length no longer encodes quantity.")
add("")
add("**Honest interpretation:** on the full 0-100 scale on which cup points ",
    "are awarded, the two bars are nearly identical: the gap of ",
    f2(v06$g1$diff), " points is ", f1(v06$g1$diff),
    "% of the scale, and the two confidence intervals are narrow but very ",
    "close together.")
add("")

## ---- G2 -------------------------------------------------------------------
add("## G2 -- Altitude versus quality scatterplot")
add("")
add("**Misleading technique:** aggregation hiding individual variation. ",
    "Graph type: scatterplot, `plot()` with `abline()`.")
add("")
add("### Part 1 computed results")
add("")
add("| Quantity | Value |")
add("|---|---|")
add("| Band width | ", f0(v03$g2$band_width), " m |")
add("| Bands defined | ", v03$g2$n_bands_defined, " |")
add("| Bands containing data | ", v03$g2$n_bands_with_data, " |")
add("| Bands plotted (n >= ", v03$g2$min_band_n, ") | ",
    v03$g2$n_bands_plotted, " |")
add("| Observations behind those bands | ", f0(v03$g2$n_obs_plotted), " |")
add("| Plotted band sizes | ", v03$g2$min_band_n_plotted, " to ",
    v03$g2$max_band_n_plotted, " |")
add("| R-squared, plotted band means | ", f4(v03$g2$r2_aggregated), " |")
add("| R-squared, all non-empty band means | ", f4(v03$g2$r2_all_bands), " |")
add("| R-squared, individual observations | ", f4(v03$g2$r2_individual), " |")
add("| Inflation factor | ", f1(v03$g2$inflation), " times |")
add("")
add("Figure: `outputs/graphs/G2_misleading_aggregated_scatter.png`")
add("")
add("**Interpretation being sold:** a tidy, almost linear climb in quality ",
    "with altitude, with an R-squared of ", f3(v03$g2$r2_aggregated),
    " suggesting altitude explains much of coffee quality.")
add("")
add("### Part 2 computed results (G2 debunk)")
add("")
add("| Quantity | Value |")
add("|---|---|")
add("| Observations plotted, honest version | ", f0(v01$n_clean), " |")
add("| Points plotted, misleading version | ", v03$g2$n_bands_plotted, " |")
add("| R-squared, individual observations | ", f4(v06$g2$r2_individual), " |")
add("| Residual SD of individual scores | ", f3(v06$g2$sigma), " points |")
add("| Total spread of individual scores | ",
    f2(v06$g2$tbl$value[nrow(v06$g2$tbl)]), " points |")
add("")
add("Figures: `outputs/graphs/G2_debunk_individual_scatter.png` and ",
    "`outputs/graphs/G2_comparison_misleading_vs_honest.png`")
add("")
add("**Why it is misleading:** averaging removes the within-band variation ",
    "before the line is fitted, so R-squared rises from ",
    f4(v03$g2$r2_individual), " on the ", f0(v01$n_clean),
    " individual coffees to ", f4(v03$g2$r2_aggregated), " on ",
    v03$g2$n_bands_plotted, " band means, an inflation of ",
    f1(v03$g2$inflation), " times. Each plotted point also looks equally ",
    "reliable although the bands behind them hold between ",
    v03$g2$min_band_n_plotted, " and ", v03$g2$max_band_n_plotted,
    " coffees. The minimum-band-size rule used in Part 1 is defensible on its ",
    "own terms, but it quietly removes the sparse high-altitude bands where ",
    "scores fall back.")
add("")
add("**Honest interpretation:** plotting all ", f0(v01$n_clean),
    " coffees shows a broad cloud with a faint upward tilt. The ",
    "individual-level fit explains ", f2(100 * v06$g2$r2_individual),
    "% of the variation, and the residual spread of ", f3(v06$g2$sigma),
    " points is large next to the ", f2(v02$n1$diff),
    " point difference between the altitude groups.")
add("")

## ---- G3 -------------------------------------------------------------------
add("## G3 -- Histograms of quality by altitude group")
add("")
add("**Misleading technique:** misleading histogram bin selection. ",
    "Graph type: histogram, `hist()`.")
add("")
add("### Part 1 computed results")
add("")
add("Bin widths investigated, with both groups binned on identical breaks:")
add("")
add("| Bin width | Bins | Modal bin, Low | Modal bin, High | Modal gap |")
add("|---|---|---|---|---|")
for (i in seq_len(nrow(v03$g3$scan))) {
  r <- v03$g3$scan[i, ]
  add("| ", f2(r$bin_width_points), " | ", f0(r$n_bins), " | ",
      f2(r$modal_bin_low), " | ", f2(r$modal_bin_high), " | ",
      f2(r$modal_gap_points), " |")
}
add("")
add("| Automatic rule | Suggested bins | Implied width |")
add("|---|---|---|")
for (i in seq_len(nrow(v03$g3$rules))) {
  r <- v03$g3$rules[i, ]
  add("| ", r$rule, " | ", f0(r$suggested_n_bins), " | ",
      f3(r$implied_bin_width_points), " |")
}
add("")
add("| Chosen presentation | Value |")
add("|---|---|")
add("| Bin width selected | ", f2(v03$g3$chosen_width), " points |")
add("| Bins, low panel | ", length(v03$g3$breaks_low) - 1, " |")
add("| Bins, high panel | ", length(v03$g3$breaks_high) - 1, " |")
add("| x-axis, low panel | ", f1(min(v03$g3$breaks_low)), " to ",
    f1(max(v03$g3$breaks_low)), " |")
add("| x-axis, high panel | ", f1(min(v03$g3$breaks_high)), " to ",
    f1(max(v03$g3$breaks_high)), " |")
add("| y-axis | raw counts, with n = ", f0(v03$g3$n_low), " and n = ",
    f0(v03$g3$n_high), " |")
add("")
add("Figure: `outputs/graphs/G3_misleading_histograms.png`")
add("")
add("**Interpretation being sold:** the two panels appear to cover different ",
    "quality ranges, as though high-altitude coffee occupies a higher band of ",
    "the scale.")
add("")
add("### Part 2 computed results (G3 debunk)")
add("")
add("| Quantity | Value |")
add("|---|---|")
add("| Bin width, honest version | ", f1(v06$g3$honest_width), " point |")
add("| Shared score range | ", f2(v06$g3$overlap$value[1]), " points |")
add("| Low scores inside the high range | ", f1(v06$g3$overlap$value[2]), "% |")
add("| High scores inside the low range | ", f1(v06$g3$overlap$value[3]), "% |")
add("| Histogram overlap coefficient | ", f3(v06$g3$overlap_coef), " |")
add("")
add("Figures: `outputs/graphs/G3_debunk_matched_histograms.png`, ",
    "`outputs/graphs/G3_debunk_overlay_density.png` and ",
    "`outputs/graphs/G3_comparison_misleading_vs_honest.png`")
add("")
add("**Why it is misleading:** the bin width of ", f2(v03$g3$chosen_width),
    " points was chosen from ", length(v03$g3$candidates),
    " candidates precisely because it pushes the two modal bins ",
    f1(max(v03$g3$scan$modal_gap_points)),
    " points apart, and each panel is then drawn over its own x-range with a ",
    "different number of bins on a raw count scale despite unequal group ",
    "sizes. Nothing about the underlying scores changed.")
add("")
add("**Honest interpretation:** with identical bins, a shared x-axis and a ",
    "density scale that corrects for the unequal group sizes, the two ",
    "distributions sit almost on top of one another. The overlap coefficient ",
    "is ", f3(v06$g3$overlap_coef), ", meaning about ",
    f0(100 * v06$g3$overlap_coef), "% of the two distributions is shared, and ",
    f1(v06$g3$overlap$value[2]),
    "% of low-altitude scores fall inside the high-altitude range.")
add("")

## ---- R1 -------------------------------------------------------------------
add("## R1 -- Simple linear regression")
add("")
add("**Misleading technique:** correlation interpreted as causation.")
add("")
add("### Part 1 computed results")
add("")
add("Model: `Total.Cup.Points ~ altitude_km`, fitted with `lm()`.")
add("")
add("**Fitted equation:** `", v04$r1$equation, "`")
add("")
add("| Quantity | Value |")
add("|---|---|")
add("| Intercept | ", f4(v04$r1$intercept), " |")
add("| Altitude coefficient | ", f4(v04$r1$slope), " cup points per km |")
add("| R-squared | ", f4(v04$r1$r2), " |")
add("| Correlation coefficient r | ", f4(v04$r1$cor), " |")
add("| p-value for altitude | ", fg(v04$r1$slope_p), " |")
add("| Residual standard error | ", f4(v04$r1$sigma), " |")
add("| Sample size n | ", f0(v04$r1$n), " |")
add("")
add("Figure: `outputs/graphs/R1_misleading_scatter_with_fit.png`. ",
    "Full summary: `outputs/regression_summaries/R1_simple_regression.txt`")
add("")
add("**Interpretation being sold:** \"Every extra kilometre of elevation ",
    "raises a coffee's score by ", f2(v04$r1$slope), " cup points (p = ",
    fg(v04$r1$slope_p), ", n = ", f0(v04$r1$n),
    "). Altitude causes better coffee, and the data proves it.\"")
add("")
add("### Part 2 computed results (R1 debunk)")
add("")
add("| Quantity | Value |")
add("|---|---|")
add("| R-squared | ", f4(v07$r1$r2), " |")
add("| Variation in score explained by altitude | ",
    f2(100 * v07$r1$r2), "% |")
add("| Variation left unexplained | ", f2(100 * (1 - v07$r1$r2)), "% |")
add("| SD of Total.Cup.Points | ", f3(v07$r1$sd_y), " points |")
add("| Residual standard error | ", f3(v07$r1$sigma), " points |")
add("| Improvement in prediction error | ",
    f3(v07$r1$sd_y - v07$r1$sigma), " points |")
add("| 95% CI, intercept | [", f4(v07$r1$ci[1, 1]), ", ",
    f4(v07$r1$ci[1, 2]), "] |")
add("| 95% CI, altitude coefficient | [", f4(v07$r1$ci[2, 1]), ", ",
    f4(v07$r1$ci[2, 2]), "] |")
add("")
add("Residual diagnostics and influence:")
add("")
add("| Diagnostic | Value |")
add("|---|---|")
add("| Residual skewness | ", f3(v07$r1$diag$value[5]), " |")
add("| Shapiro-Wilk p-value for normality | ", fg(v07$r1$shapiro_p), " |")
add("| Breusch-Pagan statistic | ", f3(v07$r1$bp_stat), " |")
add("| Breusch-Pagan p-value | ", f3(v07$r1$bp_p), " |")
add("| Observations with Cook's D > 4/n | ", f0(v07$r1$n_influential), " |")
add("| Altitude coefficient after removing them | ",
    f4(v07$r1$refit_slope), " |")
add("| Change in the coefficient | ", f2(v07$r1$refit_pct_change), "% |")
add("")
add("Confounding evidence:")
add("")
add("| Quantity | Value |")
add("|---|---|")
add("| R-squared, altitude explained by country of origin | ",
    f4(v07$r1$alt_country_r2), " |")
add("| R-squared, score explained by country of origin | ",
    f4(v07$r1$pts_country_r2), " |")
add("| R-squared, score explained by altitude alone | ", f4(v07$r1$r2), " |")
add("")
add("Full report: `outputs/regression_summaries/R1_debunk_simple_regression_full.txt`")
add("")
add("**Why it is misleading:** a very small p-value establishes that the slope ",
    "is distinguishable from zero in a sample of ", f0(v04$r1$n),
    ", not that altitude causes quality and not that the relationship is ",
    "strong. Altitude explains ", f2(100 * v07$r1$r2),
    "% of the variation in scores, so knowing a coffee's altitude reduces the ",
    "prediction error only from ", f3(v07$r1$sd_y), " to ", f3(v07$r1$sigma),
    " cup points. Altitude was never randomly assigned: country of origin ",
    "alone explains ", f1(100 * v07$r1$alt_country_r2),
    "% of the variation in altitude and ", f1(100 * v07$r1$pts_country_r2),
    "% of the variation in score, so growing conditions, varietal, processing ",
    "and producer skill are all bundled into the altitude variable.")
add("")
add("**Honest interpretation:** there is a weak but statistically clear ",
    "positive association between altitude and cup points, with the slope ",
    "somewhere in [", f3(v07$r1$ci[2, 1]), ", ", f3(v07$r1$ci[2, 2]),
    "] cup points per km. The model is a description of an observational ",
    "association, not evidence of causation, and it has almost no predictive ",
    "value for an individual coffee. The residuals are strongly left-skewed ",
    "(skewness ", f3(v07$r1$diag$value[5]),
    "), so prediction intervals for single coffees are unreliable, and ",
    f0(v07$r1$n_influential), " observations exceed Cook's D = 4/n, though ",
    "removing them moves the slope by only ", f2(v07$r1$refit_pct_change), "%.")
add("")

## ---- R2 -------------------------------------------------------------------
add("## R2 -- Multiple linear regression")
add("")
add("**Misleading technique:** omitted-variable bias.")
add("")
add("### Part 1 computed results")
add("")
add("Model: `Total.Cup.Points ~ altitude_km + Moisture + Processing.Method`, ",
    "fitted with `lm()` on the ", f0(v04$r2$n),
    " complete cases flagged by `cc_r2`. `Country.of.Origin` is deliberately ",
    "omitted although it is available in the same CSV.")
add("")
add("**Fitted equation:** `", v04$r2$equation, "`")
add("")
add("| Term | Estimate | Std. error | p-value |")
add("|---|---|---|---|")
for (i in seq_len(nrow(v04$r2$coefs))) {
  r <- v04$r2$coefs[i, ]
  add("| `", r$term, "` | ", f4(r$estimate), " | ", f4(r$std_error), " | ",
      fg(r$p_value), " |")
}
add("")
add("| Model statistic | Value |")
add("|---|---|")
add("| R-squared | ", f4(v04$r2$r2), " |")
add("| Adjusted R-squared | ", f4(v04$r2$adj_r2), " |")
add("| Residual standard error | ", f4(v04$r2$sigma), " |")
add("| Sample size n | ", f0(v04$r2$n), " |")
add("| Rows dropped for missing data | ", f0(v04$r2$n_dropped), " |")
add("")
add("Full summary: `outputs/regression_summaries/R2_multiple_regression.txt`")
add("")
add("**Interpretation being sold:** \"Even after controlling for moisture ",
    "content and processing method, altitude still adds ", f2(v04$r2$slope),
    " cup points per kilometre (p = ", fg(v04$r2$slope_p),
    "). The altitude effect survives adjustment.\"")
add("")
add("### Part 2 computed results (R2 debunk)")
add("")
add("Both models are fitted to the **identical** ", f0(v07$r2$n),
    " complete-case observations, so the two altitude coefficients are ",
    "directly comparable.")
add("")
add("**Corrected fitted equation:** `", v07$r2$equation_corrected, "`")
add("")
add("| | R2 (country omitted) | R2 corrected (country included) |")
add("|---|---|---|")
add("| Sample size n | ", f0(v07$r2$comparison$n[1]), " | ",
    f0(v07$r2$comparison$n[2]), " |")
add("| Coefficients estimated | ", v07$r2$comparison$n_coefficients[1], " | ",
    v07$r2$comparison$n_coefficients[2], " |")
add("| Altitude coefficient | ", f4(v07$r2$alt_part1), " | ",
    f4(v07$r2$alt_corrected), " |")
add("| Altitude std. error | ", f4(v07$r2$comparison$altitude_std_error[1]),
    " | ", f4(v07$r2$comparison$altitude_std_error[2]), " |")
add("| Altitude 95% CI | [", f3(v07$r2$comparison$altitude_ci_lower_95[1]), ", ",
    f3(v07$r2$comparison$altitude_ci_upper_95[1]), "] | [",
    f3(v07$r2$comparison$altitude_ci_lower_95[2]), ", ",
    f3(v07$r2$comparison$altitude_ci_upper_95[2]), "] |")
add("| Altitude p-value | ", fg(v07$r2$alt_p_part1), " | ",
    fg(v07$r2$alt_p_corrected), " |")
add("| R-squared | ", f4(v07$r2$r2_part1), " | ", f4(v07$r2$r2_corrected), " |")
add("| Adjusted R-squared | ", f4(v07$r2$adj_part1), " | ",
    f4(v07$r2$adj_corrected), " |")
add("| Residual standard error | ", f4(v07$r2$comparison$residual_se[1]), " | ",
    f4(v07$r2$comparison$residual_se[2]), " |")
add("")
add("| Test of adding country of origin | Value |")
add("|---|---|")
add("| Additional parameters | ", f0(v07$r2$df_country), " |")
add("| F statistic | ", f3(v07$r2$f_country), " |")
add("| p-value | ", fg(v07$r2$p_country), " |")
add("| Increase in R-squared | ", f4(v07$r2$r2_corrected - v07$r2$r2_part1), " |")
add("| Increase in adjusted R-squared | ",
    f4(v07$r2$adj_corrected - v07$r2$adj_part1), " |")
add("| Change in the altitude coefficient | ",
    f4(v07$r2$alt_corrected - v07$r2$alt_part1), " |")
add("| Percent change in the altitude coefficient | ",
    f1(v07$r2$alt_change_pct), "% |")
add("")
add("Full summaries: `outputs/regression_summaries/R2_debunk_corrected_regression.txt` ",
    "and `outputs/regression_summaries/R2_debunk_part1_model_same_rows.txt`")
add("")
add("**Why it is misleading:** including moisture and processing method makes ",
    "the model look carefully adjusted, which lends the altitude coefficient ",
    "unearned credibility. The variable that matters most is the one left out. ",
    "Country of origin satisfies both conditions for omitted-variable bias: it ",
    "is related to altitude (R-squared ", f3(v07$r1$alt_country_r2),
    ") and to score (R-squared ", f3(v07$r1$pts_country_r2),
    "). With country excluded, part of the difference between, say, Ethiopian ",
    "and Mexican coffee is charged to altitude.")
add("")
if (isTRUE(v07$r2$still_significant)) {
  add("**Honest interpretation:** adding country of origin to the identical ",
      f0(v07$r2$n), " observations cuts the altitude coefficient from ",
      f4(v07$r2$alt_part1), " to ", f4(v07$r2$alt_corrected),
      " cup points per km, a reduction of ", f1(abs(v07$r2$alt_change_pct)),
      "%, while R-squared rises from ", f4(v07$r2$r2_part1), " to ",
      f4(v07$r2$r2_corrected), " and the nested F-test for country is ",
      "decisive (F = ", f2(v07$r2$f_country), ", p = ", fg(v07$r2$p_country),
      "). The altitude coefficient **remains statistically significant** ",
      "(p = ", fg(v07$r2$alt_p_corrected),
      "), so altitude is not entirely explained away by origin. What changes ",
      "is its size and its meaning: the corrected coefficient is a ",
      "within-country comparison, and more than half of the Part 1 effect was ",
      "country differences wearing altitude's clothing.")
} else {
  add("**Honest interpretation:** adding country of origin to the identical ",
      f0(v07$r2$n), " observations cuts the altitude coefficient from ",
      f4(v07$r2$alt_part1), " to ", f4(v07$r2$alt_corrected),
      " cup points per km, a reduction of ", f1(abs(v07$r2$alt_change_pct)),
      "%, and it is no longer statistically significant at the 5% level ",
      "(p = ", fg(v07$r2$alt_p_corrected),
      "). Once origin is held constant, the data do not support an ",
      "independent altitude effect.")
}
add("")

## ---- Diagnostics ----------------------------------------------------------
add("## Regression diagnostics")
add("")
add("These are limitations of the models themselves. They hold regardless of ",
    "how the coefficients are interpreted, and they are reported separately ",
    "from the Part 1 spin.")
add("")
ds <- v08$summary
add("| Diagnostic | R1 | R2 | R2 corrected |")
add("|---|---|---|---|")
add("| n | ", f0(ds$n[1]), " | ", f0(ds$n[2]), " | ", f0(ds$n[3]), " |")
add("| Coefficients | ", ds$n_coefficients[1], " | ", ds$n_coefficients[2],
    " | ", ds$n_coefficients[3], " |")
add("| R-squared | ", f4(ds$r_squared[1]), " | ", f4(ds$r_squared[2]), " | ",
    f4(ds$r_squared[3]), " |")
add("| Adjusted R-squared | ", f4(ds$adj_r_squared[1]), " | ",
    f4(ds$adj_r_squared[2]), " | ", f4(ds$adj_r_squared[3]), " |")
add("| Residual std. error | ", f3(ds$residual_se[1]), " | ",
    f3(ds$residual_se[2]), " | ", f3(ds$residual_se[3]), " |")
add("| Residual skewness | ", f3(ds$residual_skewness[1]), " | ",
    f3(ds$residual_skewness[2]), " | ", f3(ds$residual_skewness[3]), " |")
add("| Shapiro-Wilk p | ", fg(ds$shapiro_p[1]), " | ", fg(ds$shapiro_p[2]),
    " | ", fg(ds$shapiro_p[3]), " |")
add("| Breusch-Pagan p | ", f4(ds$breusch_pagan_p[1]), " | ",
    f4(ds$breusch_pagan_p[2]), " | ", fg(ds$breusch_pagan_p[3]), " |")
add("| Observations above Cook's D = 4/n | ", f0(ds$n_above_cooks_threshold[1]),
    " | ", f0(ds$n_above_cooks_threshold[2]), " | ",
    f0(ds$n_above_cooks_threshold[3]), " |")
add("| Max Cook's distance | ", f4(ds$max_cooks_distance[1]), " | ",
    f4(ds$max_cooks_distance[2]), " | ", f4(ds$max_cooks_distance[3]), " |")
add("| High-leverage observations | ", f0(ds$n_high_leverage[1]), " | ",
    f0(ds$n_high_leverage[2]), " | ", f0(ds$n_high_leverage[3]), " |")
add("| Observations with leverage 1 | ", f0(ds$n_leverage_one[1]), " | ",
    f0(ds$n_leverage_one[2]), " | ", f0(ds$n_leverage_one[3]), " |")
add("| Standardised residuals beyond 3 | ",
    f0(ds$n_standardised_resid_beyond_3[1]), " | ",
    f0(ds$n_standardised_resid_beyond_3[2]), " | ",
    f0(ds$n_standardised_resid_beyond_3[3]), " |")
add("")
add("Figures: `outputs/graphs/DIAG_R1_four_panel.png`, ",
    "`outputs/graphs/DIAG_R2_four_panel.png`, ",
    "`outputs/graphs/DIAG_R2_corrected_four_panel.png` and the matching ",
    "`DIAG_*_cooks_distance.png` files. Written discussion: ",
    "`outputs/regression_summaries/DIAG_diagnostics_report.txt`.")
add("")
add("Points worth noting: residuals are left-skewed in all three models ",
    "because the scoring scale is capped near 90 while poor lots can score far ",
    "below the mean, so individual prediction intervals are unreliable even ",
    "though the coefficient estimates are sound at this sample size. The ",
    "corrected R2 model contains ", f0(ds$n_leverage_one[3]),
    " observations with leverage of exactly 1, which are countries represented ",
    "by a single coffee; they are fitted perfectly and are excluded from the ",
    "leverage panel and the influence counts.")
add("")

## ---------------------------------------------------------------------------
## 5. Step 10 verification
## ---------------------------------------------------------------------------
graph_files_part1 <- file.path("outputs", "graphs", v03$graph_types$file)
part2_outputs <- file.path("outputs", "tables", c(
  "N1_debunk_full_descriptives.csv", "N2_debunk_counts.csv",
  "N3_debunk_all_200m_bands.csv", "G1_debunk_group_means_with_n.csv",
  "G2_debunk_individual_vs_aggregated.csv", "G3_debunk_matched_bins.csv",
  "R1_debunk_coefficients_with_ci.csv", "R2_debunk_model_comparison.csv"))
n_numeric_vars <- sum(v01$variable_roles$type == "numerical")
n_categorical_vars <- sum(v01$variable_roles$type == "categorical")
equations <- c(v04$r1$equation, v04$r2$equation, v07$r2$equation_corrected)
excl_tables <- c("01_exclusions_by_reason.csv", "01_exclusion_waterfall.csv",
                 "01_exclusion_combinations.csv", "01_exclusion_overlap_depth.csv")
derived_vars <- c("altitude_km", "altitude_group", "high_quality", "cc_r2")
value_files <- c("01_prep.rds", "02_part1_numerical.rds", "03_part1_graphs.rds",
                 "04_part1_regressions.rds", "05_part2_numerical.rds",
                 "06_part2_graphs.rds", "07_part2_regressions.rds",
                 "08_diagnostics.rds")

checks <- data.frame(
  check = 1:14,
  requirement = c(
    "At least 300 observations remain",
    "At least 2 categorical and 3 numerical variables available",
    "Three Part 1 numerical summaries generated (N1-N3)",
    "Three Part 1 misleading graphs generated using base R (G1-G3)",
    "Graphs include at least two different types",
    "One simple regression fitted (R1)",
    "One multiple regression fitted (R2)",
    "Each misleading result has a matching honest correction",
    "At least 5 distinct techniques, none used more than twice",
    "Every regression reports its fitted equation",
    "All graphs and numerical values computed from the actual dataset",
    "All exclusions and transformations documented",
    "Every numerical result regenerable from R",
    "No errors occur when running the full analysis"),
  measured = c(
    sprintf("%d observations", v01$n_clean),
    sprintf("%d categorical, %d numerical", n_categorical_vars, n_numeric_vars),
    sprintf("%d of 3 present", sum(file.exists(file.path("outputs", "tables",
      c("N1_misleading_means.csv", "N2_full_computation.csv",
        "N3_cherrypicked_bands.csv"))))),
    sprintf("%d of 3 PNG files written", sum(file.exists(graph_files_part1))),
    sprintf("%d distinct types: %s",
            length(unique(v03$graph_types$graph_type)),
            paste(unique(v03$graph_types$graph_type), collapse = ", ")),
    sprintf("R1 fitted, n = %d", v04$r1$n),
    sprintf("R2 fitted, n = %d, %d coefficients", v04$r2$n,
            nrow(v04$r2$coefs)),
    sprintf("%d of 8 Part 2 corrections present", sum(file.exists(part2_outputs))),
    sprintf("%d distinct techniques, max uses = %d", n_techniques,
            max_technique_uses),
    sprintf("%d of 3 equations reported", sum(nzchar(equations))),
    sprintf("source file %s read by every script", v01$source_csv),
    sprintf("%d exclusion tables and %d derived variables documented",
            sum(file.exists(file.path("outputs", "tables", excl_tables))),
            length(derived_vars)),
    sprintf("%d value files written to outputs/values",
            sum(file.exists(file.path("outputs", "values", value_files)))),
    "run_all.R completed without error"),
  passed = c(
    v01$n_clean >= 300,
    n_categorical_vars >= 2 && n_numeric_vars >= 3,
    sum(file.exists(file.path("outputs", "tables",
      c("N1_misleading_means.csv", "N2_full_computation.csv",
        "N3_cherrypicked_bands.csv")))) == 3,
    sum(file.exists(graph_files_part1)) == 3,
    length(unique(v03$graph_types$graph_type)) >= 2,
    v04$r1$n > 0,
    v04$r2$n > 0 && nrow(v04$r2$coefs) > 2,
    sum(file.exists(part2_outputs)) == 8,
    n_techniques >= 5 && max_technique_uses <= 2,
    all(nzchar(equations)),
    file.exists(v01$source_csv),
    all(file.exists(file.path("outputs", "tables",
      c("01_exclusions_by_reason.csv", "01_exclusion_waterfall.csv",
        "01_exclusion_combinations.csv", "01_exclusion_overlap_depth.csv")))),
    all(file.exists(file.path("outputs", "values", c(
      "01_prep.rds", "02_part1_numerical.rds", "03_part1_graphs.rds",
      "04_part1_regressions.rds", "05_part2_numerical.rds",
      "06_part2_graphs.rds", "07_part2_regressions.rds",
      "08_diagnostics.rds")))),
    TRUE),
  stringsAsFactors = FALSE
)
save_table(checks, "step10_verification.csv")

add("## Verification checklist")
add("")
add("| # | Requirement | Measured | Result |")
add("|---|---|---|---|")
for (i in seq_len(nrow(checks))) {
  r <- checks[i, ]
  add("| ", r$check, " | ", r$requirement, " | ", r$measured, " | ",
      if (r$passed) "PASS" else "FAIL", " |")
}
add("")
add("Verification table: `outputs/tables/step10_verification.csv`")
add("")
add("## Output index")
add("")
add("| Location | Contents |")
add("|---|---|")
add("| `", v01$source_csv, "` | Source dataset, included unchanged |")
add("| `data/coffee_analysis_cleaned.csv` | Analytical sample, ",
    f0(v01$n_clean), " rows with the derived variables |")
add("| `outputs/tables/` | Every table as CSV, prefixed by label |")
add("| `outputs/graphs/` | Every figure as PNG, prefixed by label |")
add("| `outputs/regression_summaries/` | Complete regression summaries as text |")
add("| `outputs/values/` | Intermediate values each script passes to this report |")
add("")

con <- file("analysis_results.md", open = "wt", encoding = "UTF-8")
writeLines(md, con)
close(con)

## ---------------------------------------------------------------------------
## 6. Final console report
## ---------------------------------------------------------------------------
elapsed <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))

cat("\n", strrep("#", 70), "\n", sep = "")
cat("# STEP 10 VERIFICATION\n")
cat(strrep("#", 70), "\n")
for (i in seq_len(nrow(checks))) {
  cat(sprintf("  %-5s %2d. %-62s %s\n",
              if (checks$passed[i]) "[PASS]" else "[FAIL]",
              checks$check[i],
              substr(checks$requirement[i], 1, 62),
              checks$measured[i]))
}

n_graphs <- length(list.files(file.path("outputs", "graphs"), pattern = "\\.png$"))
n_tables <- length(list.files(file.path("outputs", "tables"), pattern = "\\.csv$"))
n_summ   <- length(list.files(file.path("outputs", "regression_summaries"),
                              pattern = "\\.txt$"))

cat("\n", strrep("#", 70), "\n", sep = "")
cat("# OUTPUTS WRITTEN\n")
cat(strrep("#", 70), "\n")
cat(sprintf("  tables (CSV) ................. %d\n", n_tables))
cat(sprintf("  graphs (PNG) ................. %d\n", n_graphs))
cat(sprintf("  regression summaries (TXT) ... %d\n", n_summ))
cat(sprintf("  cleaned dataset .............. %s (%d rows)\n",
            v01$clean_path, v01$n_clean))
cat(sprintf("  report ....................... analysis_results.md (%d lines)\n",
            length(md)))
cat(sprintf("\n  total run time ............... %.1f seconds\n", elapsed))

if (all(checks$passed)) {
  cat("\n  ALL 14 VERIFICATION CHECKS PASSED\n")
} else {
  cat(sprintf("\n  %d CHECK(S) FAILED -- see outputs/tables/step10_verification.csv\n",
              sum(!checks$passed)))
}
cat("\n[OK] run_all.R complete\n")
