## ---------------------------------------------------------------------------
## MA115 Project 1 -- "How to Lie with Statistics (and How to Catch It)"
## 08_diagnostics.R
##
## Regression diagnostics for R1, R2 and the corrected R2, using base R:
##   1. residuals versus fitted values
##   2. normal Q-Q plot
##   3. scale-location plot
##   4. residuals versus leverage
##   5. influential observation diagnostics
##
## These are STATISTICAL LIMITATIONS of the models. They are reported here
## separately from the Part 1 misleading interpretations: a model can satisfy
## or violate these checks regardless of how its coefficients are spun.
##
## Run from the project directory:  Rscript scripts/08_diagnostics.R
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
for (d in c("outputs/tables", "outputs/graphs", "outputs/regression_summaries",
            "outputs/values")) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

save_table <- function(x, name) {
  write.csv(x, file.path("outputs", "tables", name), row.names = FALSE)
}
open_png <- function(name, width = 2000, height = 1700, res = 190) {
  args <- list(filename = file.path("outputs", "graphs", name),
               width = width, height = height, res = res)
  if (isTRUE(capabilities("cairo"))) args$type <- "cairo"
  do.call(png, args)
}
close_png <- function() invisible(dev.off())

dat <- read.csv(clean_path, stringsAsFactors = FALSE)
dat$cc_r2 <- as.logical(dat$cc_r2)

cat("\n==========================================================\n")
cat("REGRESSION DIAGNOSTICS\n")
cat("==========================================================\n")

mdat <- dat[dat$cc_r2, ]
mdat$Processing.Method <- factor(mdat$Processing.Method)
mdat$Country.of.Origin <- factor(mdat$Country.of.Origin)

models <- list(
  R1 = list(fit = lm(Total.Cup.Points ~ altitude_km, data = dat),
            title = "R1  Total.Cup.Points ~ altitude_km",
            data = dat),
  R2 = list(fit = lm(Total.Cup.Points ~ altitude_km + Moisture +
                       Processing.Method, data = mdat),
            title = "R2  + Moisture + Processing.Method (country omitted)",
            data = mdat),
  R2_corrected = list(fit = lm(Total.Cup.Points ~ altitude_km + Moisture +
                                 Processing.Method + Country.of.Origin, data = mdat),
                      title = "R2 corrected  + Country.of.Origin",
                      data = mdat)
)

## ---------------------------------------------------------------------------
## Diagnostic plots: the four standard panels, then Cook's distance
## ---------------------------------------------------------------------------
for (nm in names(models)) {
  fit <- models[[nm]]$fit

  ## Panels 1, 2, 3 and 5 of plot.lm are exactly residuals vs fitted, normal
  ## Q-Q, scale-location and residuals vs leverage.
  open_png(sprintf("DIAG_%s_four_panel.png", nm))
  op <- par(mfrow = c(2, 2), mar = c(4.5, 4.5, 3, 1.5), oma = c(0, 0, 3.5, 0))
  ## Observations with leverage exactly 1 cannot be shown on the leverage
  ## panel; R warns about this and omits them.
  suppressWarnings(plot(fit, which = c(1, 2, 3, 5)))
  mtext(sprintf("Diagnostics: %s", models[[nm]]$title),
        outer = TRUE, cex = 1.05, font = 2, line = 0.8)
  par(op)
  close_png()

  ## Influence: Cook's distance by observation, with the 4/n reference line.
  n_fit <- length(residuals(fit))
  open_png(sprintf("DIAG_%s_cooks_distance.png", nm), height = 1200)
  op <- par(mar = c(4.5, 4.5, 4, 1.5))
  suppressWarnings(plot(fit, which = 4))
  abline(h = 4 / n_fit, col = "firebrick", lty = 2, lwd = 2)
  mtext(sprintf("%s   dashed line = 4/n = %.5f", nm, 4 / n_fit),
        side = 3, line = 0.3, cex = 0.85)
  par(op)
  close_png()

  cat(sprintf("\n  Saved diagnostics for %s\n", nm))
}

## ---------------------------------------------------------------------------
## Numerical diagnostic summary for all three models
## ---------------------------------------------------------------------------
diag_rows <- lapply(names(models), function(nm) {
  fit <- models[[nm]]$fit
  res <- residuals(fit)
  n_fit <- length(res)
  p_fit <- length(coef(fit)[!is.na(coef(fit))])
  cook <- cooks.distance(fit)
  hat  <- hatvalues(fit)
  rstd <- rstandard(fit)
  ## Observations with leverage of exactly 1 (a country represented by a single
  ## coffee) are fitted perfectly, which makes their Cook's distance and
  ## standardised residual non-finite. They are excluded from these counts and
  ## reported separately.
  cook_ok <- cook[is.finite(cook)]
  rstd_ok <- rstd[is.finite(rstd)]
  ## Non-constant variance check built from base R.
  bp_aux <- lm(I(res^2) ~ fitted(fit))
  bp_stat <- n_fit * summary(bp_aux)$r.squared
  sw <- shapiro.test(res)
  data.frame(
    model = nm, n = n_fit, n_coefficients = p_fit,
    r_squared = summary(fit)$r.squared,
    adj_r_squared = summary(fit)$adj.r.squared,
    residual_se = summary(fit)$sigma,
    mean_residual = mean(res),
    residual_skewness = mean((res - mean(res))^3) / sd(res)^3,
    shapiro_w = unname(sw$statistic),
    shapiro_p = sw$p.value,
    breusch_pagan_stat = bp_stat,
    breusch_pagan_p = pchisq(bp_stat, df = 1, lower.tail = FALSE),
    cooks_threshold_4_over_n = 4 / n_fit,
    n_above_cooks_threshold = sum(cook_ok > 4 / n_fit),
    max_cooks_distance = max(cook_ok),
    leverage_threshold_2p_over_n = 2 * p_fit / n_fit,
    n_high_leverage = sum(hat > 2 * p_fit / n_fit),
    n_leverage_one = sum(abs(hat - 1) < 1e-8),
    n_standardised_resid_beyond_2 = sum(abs(rstd_ok) > 2),
    n_standardised_resid_beyond_3 = sum(abs(rstd_ok) > 3),
    n_nonfinite_influence = sum(!is.finite(cook)),
    stringsAsFactors = FALSE
  )
})
diag_summary <- do.call(rbind, diag_rows)
save_table(diag_summary, "DIAG_model_diagnostics_summary.csv")

cat("\n  Diagnostic summary for all three models:\n")
print(t(diag_summary), quote = FALSE)

## ---------------------------------------------------------------------------
## Influential observation detail for R1, the headline model
## ---------------------------------------------------------------------------
r1  <- models$R1$fit
n1  <- length(residuals(r1))
inf <- influence.measures(r1)
cook <- cooks.distance(r1)
ord  <- order(-cook)[1:15]

r1_influential_detail <- data.frame(
  label = "DIAG_R1",
  rank_by_cooks_d = 1:15,
  row_in_cleaned_csv = ord,
  altitude_mean_meters = dat$altitude_mean_meters[ord],
  total_cup_points = dat$Total.Cup.Points[ord],
  country_of_origin = dat$Country.of.Origin[ord],
  fitted_value = fitted(r1)[ord],
  residual = residuals(r1)[ord],
  standardised_residual = rstandard(r1)[ord],
  leverage = hatvalues(r1)[ord],
  cooks_distance = cook[ord],
  flagged_by_influence_measures = apply(inf$is.inf[ord, , drop = FALSE], 1, any),
  row.names = NULL,
  stringsAsFactors = FALSE
)
save_table(r1_influential_detail, "DIAG_R1_top_influential_observations.csv")

cat("\n  R1: the 15 most influential observations\n")
print(r1_influential_detail[, c("rank_by_cooks_d", "altitude_mean_meters",
                                "total_cup_points", "country_of_origin",
                                "standardised_residual", "leverage",
                                "cooks_distance")],
      row.names = FALSE, digits = 4)

## ---------------------------------------------------------------------------
## Written diagnostic report, kept separate from the Part 1 spin
## ---------------------------------------------------------------------------
diag_path <- file.path("outputs", "regression_summaries", "DIAG_diagnostics_report.txt")
con <- file(diag_path, open = "wt")
writeLines(c(
  strrep("=", 74),
  "REGRESSION DIAGNOSTICS REPORT",
  strrep("=", 74), "",
  "These are limitations of the statistical models themselves. They apply no",
  "matter how the coefficients are interpreted, and they are listed separately",
  "from the deliberately misleading Part 1 readings.", "",
  strrep("-", 74), "1. RESIDUALS VERSUS FITTED VALUES", strrep("-", 74),
  sprintf("R1 residual mean is %.3g, as least squares guarantees.",
          diag_summary$mean_residual[1]),
  sprintf("Breusch-Pagan statistic %.3f, p = %.4g.",
          diag_summary$breusch_pagan_stat[1], diag_summary$breusch_pagan_p[1]),
  if (diag_summary$breusch_pagan_p[1] < 0.05)
    "That p-value indicates non-constant residual variance, so the standard errors"
  else
    "That p-value gives no strong evidence against constant residual variance,",
  if (diag_summary$breusch_pagan_p[1] < 0.05)
    "and p-values from R1 should be treated as approximate."
  else
    "so the usual standard errors are reasonable on this point.",
  "",
  strrep("-", 74), "2. NORMAL Q-Q PLOT", strrep("-", 74),
  sprintf("Shapiro-Wilk W = %.4f, p = %.4g; residual skewness = %.3f.",
          diag_summary$shapiro_w[1], diag_summary$shapiro_p[1],
          diag_summary$residual_skewness[1]),
  "The residuals are left-skewed: a long lower tail of poorly scoring coffees",
  "with no matching upper tail, because the scoring scale is capped near 90.",
  "With n over 1000 the slope estimate is still reliable by the central limit",
  "theorem, but prediction intervals for individual coffees are not.",
  "",
  strrep("-", 74), "3. SCALE-LOCATION PLOT", strrep("-", 74),
  "Used to see whether residual spread changes with the fitted value. Read it",
  "together with the Breusch-Pagan result above.",
  "",
  strrep("-", 74), "4. RESIDUALS VERSUS LEVERAGE", strrep("-", 74),
  sprintf("R1: %d observations exceed the leverage guide 2p/n = %.5f.",
          diag_summary$n_high_leverage[1],
          diag_summary$leverage_threshold_2p_over_n[1]),
  "High-leverage points sit at extreme altitudes, where the sample is thin, so",
  "the fitted line is anchored by relatively few coffees at the top of the",
  "altitude range.",
  sprintf("The corrected R2 model has %d observations with leverage of exactly 1,",
          diag_summary$n_leverage_one[3]),
  "which are countries represented by a single coffee; those are fitted",
  "perfectly and are omitted from the leverage panel by R.",
  "",
  strrep("-", 74), "5. INFLUENTIAL OBSERVATIONS", strrep("-", 74),
  sprintf("R1: %d of %d observations exceed Cook's D = 4/n = %.5f; the largest",
          diag_summary$n_above_cooks_threshold[1], n1,
          diag_summary$cooks_threshold_4_over_n[1]),
  sprintf("Cook's distance is %.4f.", diag_summary$max_cooks_distance[1]),
  sprintf("%d observations have standardised residuals beyond 2 and %d beyond 3.",
          diag_summary$n_standardised_resid_beyond_2[1],
          diag_summary$n_standardised_resid_beyond_3[1]),
  "See R1_debunk_influence_sensitivity.csv for how much the slope moves when",
  "these observations are removed.",
  "",
  strrep("-", 74), "OVERALL", strrep("-", 74),
  "The diagnostics do not invalidate the altitude slope, but together with an",
  sprintf("R-squared of %.4f they show that a correctly estimated, statistically",
          diag_summary$r_squared[1]),
  "significant coefficient can still have almost no predictive value for an",
  "individual coffee.",
  ""
), con)
close(con)

cat(sprintf("\n  Written report: %s\n", diag_path))

saveRDS(list(summary = diag_summary, r1_influential = r1_influential_detail,
             report = diag_path),
        file.path("outputs", "values", "08_diagnostics.rds"))

cat("\n[OK] 08_diagnostics.R complete\n")
