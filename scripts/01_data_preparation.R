## ---------------------------------------------------------------------------
## MA115 Project 1 -- "How to Lie with Statistics (and How to Catch It)"
## 01_data_preparation.R
##
## Builds the cleaned analytical sample from the source CSV and records every
## filtering, cleaning and transformation decision.
##
## Run from the project directory:  Rscript scripts/01_data_preparation.R
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
source_csv <- "arabica_data_cleaned.csv"
if (!file.exists(source_csv)) {
  stop("Cannot find '", source_csv, "' in ", getwd(), ".")
}

## Create the output folder tree if it does not already exist.
for (d in c("data", "outputs", "outputs/tables", "outputs/graphs",
            "outputs/regression_summaries", "outputs/values")) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

save_table <- function(x, name) {
  write.csv(x, file.path("outputs", "tables", name), row.names = FALSE)
}

cat("\n==========================================================\n")
cat("STEP 1: DATA PREPARATION\n")
cat("==========================================================\n")

## ---------------------------------------------------------------------------
## 1.1 Import the SOURCE dataset
##
## Vocabulary used throughout the project:
##   SOURCE DATASET     = every row of arabica_data_cleaned.csv as distributed.
##   ANALYTICAL SAMPLE  = the rows surviving the Step 1 filters below. Every
##                        number, graph and regression in Parts 1 and 2 is
##                        computed on the ANALYTICAL SAMPLE only.
## Note: the file name contains the word "cleaned", but that refers to the
## upstream Kaggle tidying of the Coffee Quality Institute data. It still
## contains missing altitudes and physically impossible values, so it is the
## source dataset for this project, not the analytical sample.
## ---------------------------------------------------------------------------
raw <- read.csv(source_csv, stringsAsFactors = FALSE)

n_raw <- nrow(raw)
cat("\n[1.1] Source dataset imported\n")
cat("      file ................ ", source_csv, "\n", sep = "")
cat("      rows x columns ...... ", n_raw, " x ", ncol(raw), "\n", sep = "")

## ---------------------------------------------------------------------------
## 1.2 Inspect column names, types, missing values and invalid entries
## ---------------------------------------------------------------------------
structure_tbl <- data.frame(
  column    = names(raw),
  class     = vapply(raw, function(z) class(z)[1], character(1)),
  n_missing = vapply(raw, function(z) sum(is.na(z)), integer(1)),
  n_blank   = vapply(raw, function(z) {
    if (is.character(z)) sum(trimws(z) == "") else 0L
  }, integer(1)),
  n_unique  = vapply(raw, function(z) length(unique(z)), integer(1)),
  row.names = NULL,
  stringsAsFactors = FALSE
)
save_table(structure_tbl, "01_source_structure.csv")

analysis_vars <- c("altitude_mean_meters", "Total.Cup.Points", "Moisture",
                   "Processing.Method", "Country.of.Origin")

cat("\n[1.2] Structure of the five analysis variables in the source dataset\n")
print(structure_tbl[structure_tbl$column %in% analysis_vars, ], row.names = FALSE)

## Variable roles, for the Step 10 verification of variable types.
variable_roles <- data.frame(
  variable = analysis_vars,
  type     = c("numerical", "numerical", "numerical", "categorical", "categorical"),
  role     = c("explanatory (altitude)", "response (quality score)",
               "explanatory control", "explanatory control",
               "explanatory control / confounder"),
  stringsAsFactors = FALSE
)
save_table(variable_roles, "01_variable_roles.csv")

## Invalid / implausible entries in the source dataset. These are *recorded*,
## not silently deleted, so the exclusion accounting below is auditable.
alt <- raw$altitude_mean_meters
tcp <- raw$Total.Cup.Points
invalid_tbl <- data.frame(
  check = c("altitude_mean_meters missing",
            "altitude_mean_meters < 100 m (implausibly low / unit error)",
            "altitude_mean_meters > 3500 m (above the arabica growing limit)",
            "Total.Cup.Points missing",
            "Total.Cup.Points <= 0 (unscored sample)",
            "Moisture == 0 (not physically possible; left in sample, see notes)",
            "Processing.Method blank",
            "Country.of.Origin blank"),
  n_rows = c(sum(is.na(alt)),
             sum(!is.na(alt) & alt < 100),
             sum(!is.na(alt) & alt > 3500),
             sum(is.na(tcp)),
             sum(!is.na(tcp) & tcp <= 0),
             sum(raw$Moisture == 0, na.rm = TRUE),
             sum(trimws(raw$Processing.Method) == ""),
             sum(trimws(raw$Country.of.Origin) == "")),
  stringsAsFactors = FALSE
)
save_table(invalid_tbl, "01_invalid_entries_source.csv")

cat("\n[1.2] Invalid or implausible entries in the source dataset\n")
print(invalid_tbl, row.names = FALSE)
cat("\n      Most extreme altitude values in the source dataset (metres):\n      ")
cat(head(sort(alt, decreasing = TRUE), 5), sep = ", ")
cat("\n      The largest values are data-entry errors, which is why the\n")
cat("      100-3500 m plausibility window is applied.\n")

## ---------------------------------------------------------------------------
## 1.3 Cleaning decision: blank strings become NA
##
## Processing.Method and Country.of.Origin use "" for unknown. R would treat ""
## as a genuine factor level, creating a meaningless "blank" category in the
## regressions, so blanks are converted to NA and handled as missing data.
## This changes no measured value.
## ---------------------------------------------------------------------------
raw$Processing.Method[trimws(raw$Processing.Method) == ""] <- NA
raw$Country.of.Origin[trimws(raw$Country.of.Origin) == ""] <- NA

## ---------------------------------------------------------------------------
## 1.4 Exclusion flags for the primary analysis
##
## Retention rule (as specified):
##   * altitude_mean_meters between 100 and 3500 inclusive
##   * Total.Cup.Points > 0
## Each row can fail more than one rule, so flags are computed independently
## and the overlap is accounted for explicitly in 1.5.
## ---------------------------------------------------------------------------
excl <- data.frame(
  row_id           = raw$X,
  alt_missing      = is.na(raw$altitude_mean_meters),
  alt_below_100    = !is.na(raw$altitude_mean_meters) & raw$altitude_mean_meters < 100,
  alt_above_3500   = !is.na(raw$altitude_mean_meters) & raw$altitude_mean_meters > 3500,
  tcp_missing      = is.na(raw$Total.Cup.Points),
  tcp_not_positive = !is.na(raw$Total.Cup.Points) & raw$Total.Cup.Points <= 0,
  stringsAsFactors = FALSE
)
reason_cols <- setdiff(names(excl), "row_id")
excl$n_reasons <- rowSums(excl[, reason_cols])
excl$excluded  <- excl$n_reasons > 0

keep <- !excl$excluded

## ---------------------------------------------------------------------------
## 1.5 Exclusion accounting, including overlapping reasons
## ---------------------------------------------------------------------------
## (a) Rows failing each rule, counted independently (these can double count).
excl_by_reason <- data.frame(
  reason = reason_cols,
  rule = c("altitude_mean_meters is NA",
           "altitude_mean_meters < 100",
           "altitude_mean_meters > 3500",
           "Total.Cup.Points is NA",
           "Total.Cup.Points <= 0"),
  n_rows_failing = vapply(excl[, reason_cols], sum, integer(1)),
  row.names = NULL,
  stringsAsFactors = FALSE
)
excl_by_reason$pct_of_source <- round(100 * excl_by_reason$n_rows_failing / n_raw, 2)
save_table(excl_by_reason, "01_exclusions_by_reason.csv")

## (b) How many rules each excluded row fails (the overlap structure).
overlap_depth <- as.data.frame(table(n_reasons = excl$n_reasons[excl$excluded]),
                               stringsAsFactors = FALSE)
names(overlap_depth) <- c("n_rules_failed", "n_rows")
save_table(overlap_depth, "01_exclusion_overlap_depth.csv")

## (c) The exact combination of rules failed, so overlaps are fully visible.
combo_label <- apply(excl[, reason_cols], 1, function(z) {
  if (!any(z)) "kept (no rule failed)" else paste(reason_cols[z], collapse = " + ")
})
overlap_combos <- as.data.frame(table(combination = combo_label),
                               stringsAsFactors = FALSE)
names(overlap_combos) <- c("rules_failed", "n_rows")
overlap_combos <- overlap_combos[order(-overlap_combos$n_rows), ]
save_table(overlap_combos, "01_exclusion_combinations.csv")

## (d) Unique-row reconciliation: independent counts vs rows actually removed.
n_excluded_unique <- sum(excl$excluded)
n_reason_hits     <- sum(excl_by_reason$n_rows_failing)
n_double_counted  <- n_reason_hits - n_excluded_unique

## (e) Sequential waterfall, applying the rules one at a time.
waterfall <- data.frame(
  step = c("source dataset",
           "drop altitude_mean_meters missing",
           "drop altitude_mean_meters < 100",
           "drop altitude_mean_meters > 3500",
           "drop Total.Cup.Points missing",
           "drop Total.Cup.Points <= 0"),
  stringsAsFactors = FALSE
)
alive <- rep(TRUE, n_raw)
remaining <- integer(0); removed_here <- integer(0)
for (rc in c(NA, reason_cols)) {
  if (is.na(rc)) {
    removed_here <- c(removed_here, 0L)
  } else {
    drop_now <- alive & excl[[rc]]
    removed_here <- c(removed_here, sum(drop_now))
    alive <- alive & !excl[[rc]]
  }
  remaining <- c(remaining, sum(alive))
}
waterfall$rows_removed_at_this_step <- removed_here
waterfall$rows_remaining <- remaining
save_table(waterfall, "01_exclusion_waterfall.csv")

cat("\n[1.5] Exclusion accounting\n")
print(excl_by_reason, row.names = FALSE)
cat("\n      rows failing at least one rule (unique) .... ", n_excluded_unique, "\n", sep = "")
cat("      sum of the independent reason counts ....... ", n_reason_hits, "\n", sep = "")
cat("      double counted because of overlap .......... ", n_double_counted, "\n", sep = "")
cat("\n      Sequential waterfall:\n")
print(waterfall, row.names = FALSE)

## ---------------------------------------------------------------------------
## 1.6 Build the analytical sample and the derived variables
## ---------------------------------------------------------------------------
clean <- raw[keep, , drop = FALSE]

## Transformation 1: altitude in kilometres, so a regression coefficient reads
## as "cup points per 1000 m" instead of "per metre". Monotone rescaling only;
## it cannot change any p-value, R-squared or correlation.
clean$altitude_km <- clean$altitude_mean_meters / 1000

## Transformation 2: binary altitude group at the specified 1500 m cut point.
## This discards the exact altitude within each group (revisited in Part 2).
clean$altitude_group <- factor(ifelse(clean$altitude_mean_meters < 1500, "Low", "High"),
                               levels = c("Low", "High"))

## Transformation 3: binary quality indicator at the specified 85 point cut.
clean$high_quality <- clean$Total.Cup.Points >= 85

## Flag the complete-case rows for the multiple regressions. Defining this
## once here guarantees that R2 (Part 1) and the corrected R2 (Part 2) are
## fitted to the identical set of observations, which is what makes the
## altitude coefficients comparable.
clean$cc_r2 <- stats::complete.cases(
  clean[, c("Total.Cup.Points", "altitude_km", "Moisture",
            "Processing.Method", "Country.of.Origin")]
)

n_clean <- nrow(clean)
cat("\n[1.6] Derived variables created: altitude_km, altitude_group,",
    "high_quality, cc_r2\n")
cat("      analytical sample size ...... ", n_clean, "\n", sep = "")
cat("      complete cases for R2 ....... ", sum(clean$cc_r2), "\n", sep = "")

## ---------------------------------------------------------------------------
## 1.7 Verify the sample size against the actual CSV (not hardcoded)
##
## 1055 is the documented expectation for this assignment. It is recomputed
## from the source file every run and only *compared* with the expectation.
## ---------------------------------------------------------------------------
expected_n <- 1055L
size_check <- data.frame(
  quantity = c("source dataset rows", "rows excluded (unique)",
               "analytical sample rows (computed)",
               "analytical sample rows (documented expectation)",
               "computed matches expectation"),
  value = c(n_raw, n_excluded_unique, n_clean, expected_n,
            as.integer(n_clean == expected_n)),
  stringsAsFactors = FALSE
)
save_table(size_check, "01_sample_size_verification.csv")

cat("\n[1.7] Sample size verification\n")
cat("      computed from CSV ... ", n_clean, "\n", sep = "")
cat("      expected ............ ", expected_n, "\n", sep = "")
if (n_clean == expected_n) {
  cat("      MATCH: the computed analytical sample equals the expectation.\n")
} else {
  warning("Analytical sample is ", n_clean, " rows but ", expected_n,
          " was expected. All downstream results use the computed sample.")
  cat("      MISMATCH -- see the warning above.\n")
}
if (n_clean < 300) stop("Fewer than 300 observations remain; the project requires at least 300.")

## ---------------------------------------------------------------------------
## 1.8 Save the cleaned dataset
## ---------------------------------------------------------------------------
clean_path <- file.path("data", "coffee_analysis_cleaned.csv")
write.csv(clean, clean_path, row.names = FALSE)
cat("\n[1.8] Cleaned dataset written to ", clean_path, "\n", sep = "")

## ---------------------------------------------------------------------------
## 1.9 Summary tables for the analytical sample
## ---------------------------------------------------------------------------
num_summary <- function(x, label) {
  x <- x[!is.na(x)]
  data.frame(
    variable = label, n = length(x), mean = mean(x), sd = sd(x),
    min = min(x), q1 = unname(quantile(x, 0.25)), median = median(x),
    q3 = unname(quantile(x, 0.75)), max = max(x), iqr = IQR(x),
    stringsAsFactors = FALSE
  )
}

## (a) Altitude
altitude_summary <- rbind(
  num_summary(clean$altitude_mean_meters, "altitude_mean_meters (all)"),
  num_summary(clean$altitude_mean_meters[clean$altitude_group == "Low"],
              "altitude_mean_meters (Low group, < 1500 m)"),
  num_summary(clean$altitude_mean_meters[clean$altitude_group == "High"],
              "altitude_mean_meters (High group, >= 1500 m)"),
  num_summary(clean$altitude_km, "altitude_km (all)")
)
save_table(altitude_summary, "01_summary_altitude.csv")

## (b) Total quality scores
quality_summary <- rbind(
  num_summary(clean$Total.Cup.Points, "Total.Cup.Points (all)"),
  num_summary(clean$Total.Cup.Points[clean$altitude_group == "Low"],
              "Total.Cup.Points (Low altitude)"),
  num_summary(clean$Total.Cup.Points[clean$altitude_group == "High"],
              "Total.Cup.Points (High altitude)"),
  num_summary(clean$Moisture, "Moisture (all)")
)
save_table(quality_summary, "01_summary_quality.csv")

## (c) Country of origin
country_summary <- do.call(rbind, lapply(
  split(clean, addNA(clean$Country.of.Origin, ifany = TRUE)),
  function(g) data.frame(
    country = if (is.na(g$Country.of.Origin[1])) "(missing)" else g$Country.of.Origin[1],
    n = nrow(g),
    pct_of_sample = 100 * nrow(g) / n_clean,
    mean_points = mean(g$Total.Cup.Points),
    sd_points = sd(g$Total.Cup.Points),
    mean_altitude_m = mean(g$altitude_mean_meters),
    n_high_quality = sum(g$high_quality),
    stringsAsFactors = FALSE
  )
))
country_summary <- country_summary[order(-country_summary$n), ]
save_table(country_summary, "01_summary_country.csv")

## (d) Processing methods
processing_summary <- do.call(rbind, lapply(
  split(clean, addNA(clean$Processing.Method, ifany = TRUE)),
  function(g) data.frame(
    processing_method = if (is.na(g$Processing.Method[1])) "(missing)" else g$Processing.Method[1],
    n = nrow(g),
    pct_of_sample = 100 * nrow(g) / n_clean,
    mean_points = mean(g$Total.Cup.Points),
    sd_points = sd(g$Total.Cup.Points),
    mean_altitude_m = mean(g$altitude_mean_meters),
    stringsAsFactors = FALSE
  )
))
processing_summary <- processing_summary[order(-processing_summary$n), ]
save_table(processing_summary, "01_summary_processing_method.csv")

## (e) Missing data in the analytical sample
missing_summary <- data.frame(
  variable = analysis_vars,
  n_missing_source = vapply(analysis_vars, function(v) sum(is.na(raw[[v]])), integer(1)),
  n_missing_analytical = vapply(analysis_vars, function(v) sum(is.na(clean[[v]])), integer(1)),
  row.names = NULL,
  stringsAsFactors = FALSE
)
missing_summary$pct_missing_analytical <-
  round(100 * missing_summary$n_missing_analytical / n_clean, 2)
missing_summary <- rbind(missing_summary, data.frame(
  variable = "complete cases on all five variables",
  n_missing_source = NA_integer_,
  n_missing_analytical = n_clean - sum(clean$cc_r2),
  pct_missing_analytical = round(100 * (n_clean - sum(clean$cc_r2)) / n_clean, 2),
  stringsAsFactors = FALSE
))
save_table(missing_summary, "01_summary_missing_data.csv")

## (f) Group sizes used repeatedly in Parts 1 and 2
group_sizes <- data.frame(
  altitude_group = c("Low", "High"),
  definition = c("altitude_mean_meters < 1500", "altitude_mean_meters >= 1500"),
  n = c(sum(clean$altitude_group == "Low"), sum(clean$altitude_group == "High")),
  stringsAsFactors = FALSE
)
save_table(group_sizes, "01_summary_group_sizes.csv")

cat("\n[1.9] Summary tables\n")
cat("\n      Altitude:\n"); print(altitude_summary, row.names = FALSE, digits = 5)
cat("\n      Quality scores:\n"); print(quality_summary, row.names = FALSE, digits = 5)
cat("\n      Group sizes:\n"); print(group_sizes, row.names = FALSE)
cat("\n      Processing methods:\n"); print(processing_summary, row.names = FALSE, digits = 4)
cat("\n      Missing data:\n"); print(missing_summary, row.names = FALSE)
cat("\n      Countries (top 10 of ", nrow(country_summary), "):\n", sep = "")
print(head(country_summary, 10), row.names = FALSE, digits = 4)

## ---------------------------------------------------------------------------
## 1.10 Pass key values to the report generator
## ---------------------------------------------------------------------------
saveRDS(list(
  source_csv = source_csv,
  n_raw = n_raw, n_cols_raw = ncol(raw),
  n_clean = n_clean, expected_n = expected_n,
  size_match = n_clean == expected_n,
  n_excluded_unique = n_excluded_unique,
  n_reason_hits = n_reason_hits,
  n_double_counted = n_double_counted,
  excl_by_reason = excl_by_reason,
  waterfall = waterfall,
  overlap_combos = overlap_combos,
  n_cc_r2 = sum(clean$cc_r2),
  n_low = sum(clean$altitude_group == "Low"),
  n_high = sum(clean$altitude_group == "High"),
  n_countries = length(unique(clean$Country.of.Origin[!is.na(clean$Country.of.Origin)])),
  n_processing = length(unique(clean$Processing.Method[!is.na(clean$Processing.Method)])),
  variable_roles = variable_roles,
  invalid_tbl = invalid_tbl,
  clean_path = clean_path
), file.path("outputs", "values", "01_prep.rds"))

cat("\n[OK] 01_data_preparation.R complete\n")
