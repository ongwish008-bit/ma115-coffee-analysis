## ---------------------------------------------------------------------------
## MA115 Project 1 -- "How to Lie with Statistics (and How to Catch It)"
## 07_part2_regressions.R
##
## PART 2 (honest) regression corrections. Labels match Part 1 exactly.
##
##   R1 DEBUNK -- the complete simple regression: explanatory power, intervals,
##                residual diagnostics, influence, correlation vs causation
##   R2 DEBUNK -- the same multiple regression with Country.of.Origin added
##                back, fitted on the IDENTICAL complete-case observations
##
## Run from the project directory:  Rscript scripts/07_part2_regressions.R
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

fitted_equation <- function(model, digits = 4) {
  cf <- coef(model)
  cf <- cf[!is.na(cf)]
  resp <- as.character(formula(model))[2]
  out <- paste0("predicted ", resp, " = ",
                formatC(cf[1], format = "f", digits = digits))
  for (i in seq_along(cf)[-1]) {
    out <- paste0(out, if (cf[i] < 0) " - " else " + ",
                  formatC(abs(cf[i]), format = "f", digits = digits),
                  " * ", names(cf)[i])
  }
  out
}

write_reg_summary <- function(model, label, fname, notes = character(0)) {
  path <- file.path("outputs", "regression_summaries", fname)
  con <- file(path, open = "wt")
  on.exit(close(con))
  writeLines(c(strrep("=", 74), label, strrep("=", 74), "",
               paste0("Model call: ", paste(deparse(formula(model)), collapse = " ")),
               paste0("Observations used (n): ", length(stats::residuals(model))),
               "", "Fitted equation:", fitted_equation(model), "",
               strrep("-", 74), "Full summary():", strrep("-", 74), ""), con)
  writeLines(capture.output(print(summary(model))), con)
  writeLines(c("", strrep("-", 74), "95% confidence intervals for the coefficients:",
               strrep("-", 74), ""), con)
  writeLines(capture.output(print(confint(model))), con)
  writeLines(c("", strrep("-", 74), "Analysis of variance:", strrep("-", 74), ""), con)
  writeLines(capture.output(print(anova(model))), con)
  if (length(notes)) {
    writeLines(c("", strrep("-", 74), "Notes:", strrep("-", 74), ""), con)
    writeLines(notes, con)
  }
  invisible(path)
}

coef_table <- function(model, label) {
  sm <- summary(model)$coefficients
  ci <- confint(model)
  data.frame(
    label = label, term = rownames(sm),
    estimate = sm[, "Estimate"], std_error = sm[, "Std. Error"],
    t_value = sm[, "t value"], p_value = sm[, "Pr(>|t|)"],
    ci_lower_95 = ci[rownames(sm), 1], ci_upper_95 = ci[rownames(sm), 2],
    row.names = NULL, stringsAsFactors = FALSE
  )
}

dat <- read.csv(clean_path, stringsAsFactors = FALSE)
dat$altitude_group <- factor(dat$altitude_group, levels = c("Low", "High"))
dat$cc_r2 <- as.logical(dat$cc_r2)

cat("\n==========================================================\n")
cat("PART 2: HONEST REGRESSION ANALYSIS (R1-R2 DEBUNKS)\n")
cat("==========================================================\n")

## ---------------------------------------------------------------------------
## R1 DEBUNK -- the complete simple regression
## ---------------------------------------------------------------------------
r1 <- lm(Total.Cup.Points ~ altitude_km, data = dat)
r1_sum <- summary(r1)
r1_n   <- length(residuals(r1))
r1_ci  <- confint(r1)

## (a) Explanatory power. A tiny p-value says the slope is distinguishable
## from zero; it says nothing about how much of the outcome is explained.
r1_power <- data.frame(
  label = "R1_debunk",
  quantity = c("R-squared", "percent of variation in score EXPLAINED by altitude",
               "percent of variation in score LEFT UNEXPLAINED",
               "residual standard error (cup points)",
               "standard deviation of Total.Cup.Points (cup points)",
               "reduction in prediction error from knowing altitude (points)",
               "p-value for the altitude slope", "sample size n"),
  value = c(r1_sum$r.squared, 100 * r1_sum$r.squared,
            100 * (1 - r1_sum$r.squared), r1_sum$sigma,
            sd(dat$Total.Cup.Points),
            sd(dat$Total.Cup.Points) - r1_sum$sigma,
            r1_sum$coefficients["altitude_km", "Pr(>|t|)"], r1_n),
  stringsAsFactors = FALSE
)
save_table(r1_power, "R1_debunk_explanatory_power.csv")

## (b) 95% confidence intervals for every coefficient.
r1_coefs <- coef_table(r1, "R1_debunk")
save_table(r1_coefs, "R1_debunk_coefficients_with_ci.csv")

## (c) Residual diagnostics, computed as numbers. The figures are produced by
## 08_diagnostics.R.
r1_res <- residuals(r1)
r1_fit <- fitted(r1)
## Breusch-Pagan style test for non-constant variance, built from base R: the
## auxiliary regression of squared residuals on the fitted values.
bp_aux  <- lm(I(r1_res^2) ~ r1_fit)
bp_stat <- r1_n * summary(bp_aux)$r.squared
bp_p    <- pchisq(bp_stat, df = 1, lower.tail = FALSE)
r1_shapiro <- shapiro.test(r1_res)

r1_diag <- data.frame(
  label = "R1_debunk",
  diagnostic = c("mean of residuals", "residual standard deviation",
                 "smallest residual", "largest residual",
                 "residual skewness",
                 "Shapiro-Wilk W for normality of residuals",
                 "Shapiro-Wilk p-value",
                 "correlation of |residuals| with fitted values",
                 "Breusch-Pagan statistic (non-constant variance)",
                 "Breusch-Pagan p-value"),
  value = c(mean(r1_res), sd(r1_res), min(r1_res), max(r1_res),
            mean((r1_res - mean(r1_res))^3) / sd(r1_res)^3,
            unname(r1_shapiro$statistic), r1_shapiro$p.value,
            cor(abs(r1_res), r1_fit), bp_stat, bp_p),
  stringsAsFactors = FALSE
)
save_table(r1_diag, "R1_debunk_residual_diagnostics.csv")

## (d) Sensitivity to influential observations.
r1_cook <- cooks.distance(r1)
r1_hat  <- hatvalues(r1)
cook_cut <- 4 / r1_n
hat_cut  <- 2 * length(coef(r1)) / r1_n
r1_influential <- which(r1_cook > cook_cut)

r1_refit <- lm(Total.Cup.Points ~ altitude_km, data = dat[-r1_influential, ])
r1_refit_sum <- summary(r1_refit)

## Also drop the single lowest-scoring coffee, a clear outlier in the sample.
worst_idx <- which.min(dat$Total.Cup.Points)
r1_refit_outlier <- lm(Total.Cup.Points ~ altitude_km, data = dat[-worst_idx, ])

r1_sens <- data.frame(
  label = "R1_debunk",
  model = c("R1 on all observations",
            sprintf("R1 excluding %d points with Cook's D > 4/n", length(r1_influential)),
            "R1 excluding only the single lowest-scoring coffee"),
  n = c(r1_n, length(residuals(r1_refit)), length(residuals(r1_refit_outlier))),
  intercept = c(coef(r1)[1], coef(r1_refit)[1], coef(r1_refit_outlier)[1]),
  altitude_coefficient = c(coef(r1)[2], coef(r1_refit)[2], coef(r1_refit_outlier)[2]),
  r_squared = c(r1_sum$r.squared, r1_refit_sum$r.squared,
                summary(r1_refit_outlier)$r.squared),
  altitude_p_value = c(r1_sum$coefficients[2, 4], r1_refit_sum$coefficients[2, 4],
                       summary(r1_refit_outlier)$coefficients[2, 4]),
  stringsAsFactors = FALSE
)
r1_sens$pct_change_in_coefficient <-
  100 * (r1_sens$altitude_coefficient - r1_sens$altitude_coefficient[1]) /
  r1_sens$altitude_coefficient[1]
save_table(r1_sens, "R1_debunk_influence_sensitivity.csv")

## (e) Correlation versus causation: altitude is entangled with country, so the
## simple slope cannot be read as a causal effect of elevation.
alt_country_r2 <- summary(lm(altitude_km ~ Country.of.Origin,
                             data = dat[!is.na(dat$Country.of.Origin), ]))$r.squared
pts_country_r2 <- summary(lm(Total.Cup.Points ~ Country.of.Origin,
                             data = dat[!is.na(dat$Country.of.Origin), ]))$r.squared
r1_causation <- data.frame(
  label = "R1_debunk",
  quantity = c("R-squared, altitude explained by country of origin",
               "R-squared, score explained by country of origin",
               "R-squared, score explained by altitude alone",
               "countries in the analytical sample",
               "randomised assignment of altitude",
               "coffees per country, smallest", "coffees per country, largest"),
  value = c(alt_country_r2, pts_country_r2, r1_sum$r.squared,
            length(unique(dat$Country.of.Origin[!is.na(dat$Country.of.Origin)])),
            0,
            min(table(dat$Country.of.Origin)), max(table(dat$Country.of.Origin))),
  stringsAsFactors = FALSE
)
save_table(r1_causation, "R1_debunk_correlation_vs_causation.csv")

write_reg_summary(
  r1, "R1 DEBUNK  SIMPLE LINEAR REGRESSION, reported in full",
  "R1_debunk_simple_regression_full.txt",
  notes = c(
    sprintf("R-squared = %.5f, so altitude accounts for %.2f%% of the variation",
            r1_sum$r.squared, 100 * r1_sum$r.squared),
    sprintf("in cup points and leaves %.2f%% unexplained.",
            100 * (1 - r1_sum$r.squared)),
    sprintf("Knowing a coffee's altitude reduces the prediction error only from"),
    sprintf("%.3f to %.3f cup points.", sd(dat$Total.Cup.Points), r1_sum$sigma),
    "",
    sprintf("95%% CI for the altitude slope: [%.4f, %.4f]", r1_ci[2, 1], r1_ci[2, 2]),
    "",
    "Correlation is not causation here: altitude is not randomly assigned, and",
    sprintf("country of origin alone explains %.1f%% of the variation in altitude",
            100 * alt_country_r2),
    sprintf("and %.1f%% of the variation in score.", 100 * pts_country_r2),
    "",
    sprintf("Influence: %d of %d observations exceed Cook's D = 4/n.",
            length(r1_influential), r1_n),
    sprintf("Dropping them moves the slope from %.4f to %.4f (%.1f%%).",
            coef(r1)[2], coef(r1_refit)[2], r1_sens$pct_change_in_coefficient[2])
  )
)

cat("\n----------------------------------------------------------\n")
cat("R1 DEBUNK  The complete simple regression\n")
cat("----------------------------------------------------------\n")
cat(sprintf("  Fitted equation ... %s\n", fitted_equation(r1)))
cat("\n  Explanatory power:\n")
print(r1_power[, c("quantity", "value")], row.names = FALSE, digits = 4)
cat("\n  Coefficients with 95% confidence intervals:\n")
print(r1_coefs[, c("term", "estimate", "std_error", "p_value",
                   "ci_lower_95", "ci_upper_95")], row.names = FALSE, digits = 4)
cat("\n  Residual diagnostics:\n")
print(r1_diag[, c("diagnostic", "value")], row.names = FALSE, digits = 4)
cat("\n  Sensitivity to influential observations:\n")
print(r1_sens[, c("model", "n", "altitude_coefficient", "r_squared",
                  "altitude_p_value", "pct_change_in_coefficient")],
      row.names = FALSE, digits = 4)
cat("\n  Correlation versus causation:\n")
print(r1_causation[, c("quantity", "value")], row.names = FALSE, digits = 4)

## ---------------------------------------------------------------------------
## R2 DEBUNK -- add Country.of.Origin back on the IDENTICAL observations
##
## Both models are fitted to dat[dat$cc_r2, ], the complete cases flagged in
## script 01. Identical rows in both models is what makes the two altitude
## coefficients directly comparable; otherwise a change in the coefficient
## could just be a change in the sample.
## ---------------------------------------------------------------------------
mdat <- dat[dat$cc_r2, ]
mdat$Processing.Method <- factor(mdat$Processing.Method)
mdat$Country.of.Origin <- factor(mdat$Country.of.Origin)

r2      <- lm(Total.Cup.Points ~ altitude_km + Moisture + Processing.Method,
              data = mdat)
r2_corr <- lm(Total.Cup.Points ~ altitude_km + Moisture + Processing.Method +
                Country.of.Origin, data = mdat)

r2_sum  <- summary(r2)
r2c_sum <- summary(r2_corr)

stopifnot(length(residuals(r2)) == length(residuals(r2_corr)))

r2_ci  <- confint(r2)
r2c_ci <- confint(r2_corr)

alt_r2   <- unname(coef(r2)["altitude_km"])
alt_r2c  <- unname(coef(r2_corr)["altitude_km"])
alt_p    <- r2_sum$coefficients["altitude_km", "Pr(>|t|)"]
alt_pc   <- r2c_sum$coefficients["altitude_km", "Pr(>|t|)"]
alt_change_pct <- 100 * (alt_r2c - alt_r2) / alt_r2

## Model-level comparison table.
r2_comparison <- data.frame(
  label = "R2_debunk",
  model = c("R2 (Part 1): altitude + moisture + processing",
            "R2 corrected (Part 2): + country of origin"),
  n = c(length(residuals(r2)), length(residuals(r2_corr))),
  n_coefficients = c(length(coef(r2)), length(coef(r2_corr))),
  residual_df = c(r2$df.residual, r2_corr$df.residual),
  r_squared = c(r2_sum$r.squared, r2c_sum$r.squared),
  adj_r_squared = c(r2_sum$adj.r.squared, r2c_sum$adj.r.squared),
  residual_se = c(r2_sum$sigma, r2c_sum$sigma),
  altitude_coefficient = c(alt_r2, alt_r2c),
  altitude_std_error = c(r2_sum$coefficients["altitude_km", "Std. Error"],
                         r2c_sum$coefficients["altitude_km", "Std. Error"]),
  altitude_p_value = c(alt_p, alt_pc),
  altitude_ci_lower_95 = c(r2_ci["altitude_km", 1], r2c_ci["altitude_km", 1]),
  altitude_ci_upper_95 = c(r2_ci["altitude_km", 2], r2c_ci["altitude_km", 2]),
  stringsAsFactors = FALSE
)
save_table(r2_comparison, "R2_debunk_model_comparison.csv")

## Full coefficient tables for both models.
save_table(rbind(coef_table(r2, "R2_part1"), coef_table(r2_corr, "R2_corrected")),
           "R2_debunk_all_coefficients.csv")
save_table(data.frame(
  label = c("R2_part1", "R2_corrected"),
  fitted_equation = c(fitted_equation(r2), fitted_equation(r2_corr)),
  stringsAsFactors = FALSE
), "R2_debunk_fitted_equations.csv")

## Does country add real explanatory power? A nested F-test answers it.
r2_anova <- anova(r2, r2_corr)
r2_nested <- data.frame(
  label = "R2_debunk",
  quantity = c("additional parameters for country of origin",
               "F statistic for adding country",
               "p-value for adding country",
               "increase in R-squared", "increase in adjusted R-squared",
               "change in the altitude coefficient",
               "percent change in the altitude coefficient"),
  value = c(r2_anova$Df[2], r2_anova$F[2], r2_anova$`Pr(>F)`[2],
            r2c_sum$r.squared - r2_sum$r.squared,
            r2c_sum$adj.r.squared - r2_sum$adj.r.squared,
            alt_r2c - alt_r2, alt_change_pct),
  stringsAsFactors = FALSE
)
save_table(r2_nested, "R2_debunk_nested_test.csv")

write_reg_summary(
  r2_corr, "R2 DEBUNK  MULTIPLE REGRESSION with Country.of.Origin included",
  "R2_debunk_corrected_regression.txt",
  notes = c(
    sprintf("Fitted on the same %d complete cases as the Part 1 R2 model.",
            length(residuals(r2_corr))),
    "",
    sprintf("Altitude coefficient, country omitted  : %.4f (p = %.4g)", alt_r2, alt_p),
    sprintf("Altitude coefficient, country included : %.4f (p = %.4g)", alt_r2c, alt_pc),
    sprintf("Change: %.4f cup points per km, i.e. %.1f%%.",
            alt_r2c - alt_r2, alt_change_pct),
    "",
    sprintf("Adding country raises R-squared from %.4f to %.4f and adjusted",
            r2_sum$r.squared, r2c_sum$r.squared),
    sprintf("R-squared from %.4f to %.4f; nested F = %.3f on %d and %d df, p = %.4g.",
            r2_sum$adj.r.squared, r2c_sum$adj.r.squared,
            r2_anova$F[2], r2_anova$Df[2], r2_corr$df.residual,
            r2_anova$`Pr(>F)`[2]),
    "",
    "Note: some countries contribute very few coffees, so individual country",
    "coefficients are imprecise. The nested F-test above, not any single",
    "country dummy, is the evidence that origin matters."
  )
)

## Also save the Part 1 model summary refitted here, so the two text files in
## outputs/regression_summaries/ are directly comparable.
write_reg_summary(
  r2, "R2 (Part 1 model) refitted alongside the corrected model",
  "R2_debunk_part1_model_same_rows.txt",
  notes = c("Identical rows to R2_debunk_corrected_regression.txt.",
            "Country of origin is omitted here, which is the Part 1 error.")
)

cat("\n----------------------------------------------------------\n")
cat("R2 DEBUNK  Country of origin added back, identical rows\n")
cat("----------------------------------------------------------\n")
cat(sprintf("  Both models fitted on n = %d complete cases\n",
            length(residuals(r2_corr))))
cat("\n  Model comparison:\n")
print(r2_comparison[, c("model", "n", "r_squared", "adj_r_squared",
                        "altitude_coefficient", "altitude_p_value")],
      row.names = FALSE, digits = 4)
cat("\n  Altitude coefficient, side by side:\n")
print(r2_comparison[, c("model", "altitude_coefficient", "altitude_std_error",
                        "altitude_ci_lower_95", "altitude_ci_upper_95",
                        "altitude_p_value")], row.names = FALSE, digits = 4)
cat("\n  Does country add explanatory power?\n")
print(r2_nested[, c("quantity", "value")], row.names = FALSE, digits = 4)
cat("\n  Corrected model, non-country coefficients:\n")
cc <- coef_table(r2_corr, "R2_corrected")
print(cc[!grepl("^Country.of.Origin", cc$term),
         c("term", "estimate", "std_error", "p_value",
           "ci_lower_95", "ci_upper_95")], row.names = FALSE, digits = 4)

cat("\n  HONEST INTERPRETATION:\n")
cat(sprintf("    Holding country of origin constant, the altitude coefficient moves\n"))
cat(sprintf("    from %.4f to %.4f cup points per km, a change of %.1f%%.\n",
            alt_r2, alt_r2c, alt_change_pct))
if (alt_pc >= 0.05) {
  cat(sprintf("    With country in the model the altitude slope is no longer\n"))
  cat(sprintf("    statistically significant at the 5%% level (p = %.4g).\n", alt_pc))
} else {
  cat(sprintf("    The slope remains statistically significant (p = %.4g), so\n", alt_pc))
  cat("    altitude is not wholly explained away by origin. What changes is its\n")
  cat("    size and meaning: it is now a within-country comparison.\n")
}
cat("    Part 1's coefficient was partly measuring which COUNTRY a coffee came\n")
cat("    from, because high-altitude countries also tend to be the ones with\n")
cat("    higher-scoring coffee.\n")
cat("  Saved: outputs/regression_summaries/R2_debunk_corrected_regression.txt\n")

saveRDS(list(
  r1 = list(power = r1_power, coefs = r1_coefs, diag = r1_diag,
            sens = r1_sens, causation = r1_causation,
            n = r1_n, r2 = r1_sum$r.squared, sigma = r1_sum$sigma,
            sd_y = sd(dat$Total.Cup.Points),
            ci = r1_ci, equation = fitted_equation(r1),
            n_influential = length(r1_influential), cook_cut = cook_cut,
            hat_cut = hat_cut,
            refit_slope = unname(coef(r1_refit)[2]),
            refit_pct_change = r1_sens$pct_change_in_coefficient[2],
            bp_stat = bp_stat, bp_p = bp_p,
            shapiro_p = r1_shapiro$p.value,
            alt_country_r2 = alt_country_r2, pts_country_r2 = pts_country_r2),
  r2 = list(comparison = r2_comparison, nested = r2_nested,
            n = length(residuals(r2_corr)),
            alt_part1 = alt_r2, alt_corrected = alt_r2c,
            alt_p_part1 = alt_p, alt_p_corrected = alt_pc,
            alt_change_pct = alt_change_pct,
            r2_part1 = r2_sum$r.squared, r2_corrected = r2c_sum$r.squared,
            adj_part1 = r2_sum$adj.r.squared, adj_corrected = r2c_sum$adj.r.squared,
            f_country = r2_anova$F[2], p_country = r2_anova$`Pr(>F)`[2],
            df_country = r2_anova$Df[2],
            equation_part1 = fitted_equation(r2),
            equation_corrected = fitted_equation(r2_corr),
            still_significant = alt_pc < 0.05)
), file.path("outputs", "values", "07_part2_regressions.rds"))

cat("\n[OK] 07_part2_regressions.R complete\n")
