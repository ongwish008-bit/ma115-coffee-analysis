## ---------------------------------------------------------------------------
## MA115 Project 1 -- "How to Lie with Statistics (and How to Catch It)"
## 04_part1_regressions.R
##
## PART 1 (deliberately misleading) regressions R1 and R2.
##
##   R1 -- simple linear regression,   technique: correlation read as causation
##   R2 -- multiple linear regression, technique: omitted-variable bias
##
## Both models are correctly estimated. The deception is in the interpretation
## attached to R1, and in the variable deliberately left out of R2.
##
## Run from the project directory:  Rscript scripts/04_part1_regressions.R
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
open_png <- function(name, width = 2000, height = 1400, res = 200) {
  args <- list(filename = file.path("outputs", "graphs", name),
               width = width, height = height, res = res)
  if (isTRUE(capabilities("cairo"))) args$type <- "cairo"
  do.call(png, args)
}

## Builds the fitted regression equation from the estimated coefficients, so
## the reported equation can never drift out of step with the model.
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

## Writes a complete regression summary, plus the fitted equation, to a text
## file in outputs/regression_summaries/.
write_reg_summary <- function(model, label, fname, notes = character(0)) {
  path <- file.path("outputs", "regression_summaries", fname)
  con <- file(path, open = "wt")
  on.exit(close(con))
  writeLines(c(strrep("=", 74), paste0(label), strrep("=", 74), "",
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

## Tidy coefficient table from a fitted model.
coef_table <- function(model, label) {
  sm <- summary(model)$coefficients
  ci <- confint(model)
  data.frame(
    label = label,
    term = rownames(sm),
    estimate = sm[, "Estimate"],
    std_error = sm[, "Std. Error"],
    t_value = sm[, "t value"],
    p_value = sm[, "Pr(>|t|)"],
    ci_lower_95 = ci[rownames(sm), 1],
    ci_upper_95 = ci[rownames(sm), 2],
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}

dat <- read.csv(clean_path, stringsAsFactors = FALSE)
dat$altitude_group <- factor(dat$altitude_group, levels = c("Low", "High"))
dat$cc_r2 <- as.logical(dat$cc_r2)

cat("\n==========================================================\n")
cat("PART 1: MISLEADING REGRESSIONS (R1-R2)\n")
cat("==========================================================\n")

## ---------------------------------------------------------------------------
## R1 -- SIMPLE LINEAR REGRESSION
## Total.Cup.Points ~ altitude_km
## Technique: correlation interpreted as causation.
## ---------------------------------------------------------------------------
r1 <- lm(Total.Cup.Points ~ altitude_km, data = dat)
r1_sum <- summary(r1)

r1_n         <- length(residuals(r1))
r1_intercept <- unname(coef(r1)[1])
r1_slope     <- unname(coef(r1)[2])
r1_r2        <- r1_sum$r.squared
r1_adj_r2    <- r1_sum$adj.r.squared
r1_slope_p   <- r1_sum$coefficients["altitude_km", "Pr(>|t|)"]
r1_eq        <- fitted_equation(r1)

## Pearson correlation between altitude and score, with its own test.
r1_cor_test <- cor.test(dat$altitude_km, dat$Total.Cup.Points)
r1_cor      <- unname(r1_cor_test$estimate)

r1_stats <- data.frame(
  label = "R1",
  quantity = c("intercept", "altitude_km coefficient", "R-squared",
               "adjusted R-squared", "correlation coefficient r",
               "p-value for altitude_km", "p-value for the correlation test",
               "residual standard error", "sample size n"),
  value = c(r1_intercept, r1_slope, r1_r2, r1_adj_r2, r1_cor,
            r1_slope_p, r1_cor_test$p.value, r1_sum$sigma, r1_n),
  stringsAsFactors = FALSE
)
save_table(r1_stats, "R1_model_statistics.csv")
save_table(coef_table(r1, "R1"), "R1_coefficients.csv")
save_table(data.frame(label = "R1", fitted_equation = r1_eq,
                      stringsAsFactors = FALSE),
           "R1_fitted_equation.csv")

write_reg_summary(
  r1, "R1  SIMPLE LINEAR REGRESSION (Part 1 model)",
  "R1_simple_regression.txt",
  notes = c(
    "Part 1 technique: correlation interpreted as causation.",
    "The estimation itself is correct. The misleading step is the claim that",
    "the positive slope shows altitude CAUSES higher quality, from",
    "observational data with no randomisation and no controls.",
    sprintf("Altitude explains %.2f%% of the variation in cup points.",
            100 * r1_r2)
  )
)

## Scatterplot with the fitted line.
open_png("R1_misleading_scatter_with_fit.png")
op <- par(mar = c(5, 5, 5, 2))
plot(dat$altitude_km, dat$Total.Cup.Points,
     pch = 16, cex = 0.5, col = rgb(0.2, 0.2, 0.2, 0.35),
     xlab = "Altitude (km)", ylab = "Total cup points",
     main = "R1  Coffee quality increases with altitude", cex.lab = 1.1)
abline(r1, col = "firebrick", lwd = 3)
legend("bottomright",
       legend = c(r1_eq,
                  sprintf("r = %.3f,  R-squared = %.4f", r1_cor, r1_r2),
                  sprintf("p = %.3g,  n = %d", r1_slope_p, r1_n)),
       bty = "n", cex = 0.8)
par(op)
invisible(dev.off())

cat("\n----------------------------------------------------------\n")
cat("R1  Simple linear regression: Total.Cup.Points ~ altitude_km\n")
cat("----------------------------------------------------------\n")
cat("  Technique ........ correlation interpreted as causation\n")
cat(sprintf("  Intercept ..................... %.4f\n", r1_intercept))
cat(sprintf("  Altitude coefficient .......... %.4f cup points per km\n", r1_slope))
cat(sprintf("  Fitted equation ............... %s\n", r1_eq))
cat(sprintf("  R-squared ..................... %.5f\n", r1_r2))
cat(sprintf("  Correlation coefficient r ..... %.4f\n", r1_cor))
cat(sprintf("  p-value for altitude .......... %.4g\n", r1_slope_p))
cat(sprintf("  Sample size n ................. %d\n", r1_n))
cat("\n  MISLEADING INTERPRETATION (what Part 1 sells):\n")
cat(sprintf("    \"Every extra kilometre of elevation raises a coffee's score by\n"))
cat(sprintf("     %.2f cup points (p = %.3g, n = %d). Altitude causes better\n",
            r1_slope, r1_slope_p, r1_n))
cat("     coffee, and the data proves it.\"\n")
cat("  Saved: outputs/graphs/R1_misleading_scatter_with_fit.png\n")
cat("         outputs/regression_summaries/R1_simple_regression.txt\n")

## ---------------------------------------------------------------------------
## R2 -- MULTIPLE LINEAR REGRESSION
## Total.Cup.Points ~ altitude_km + Moisture + Processing.Method
## Technique: omitted-variable bias.
##
## Two processing controls are added, which makes the model look careful and
## "adjusted". Country.of.Origin is deliberately left out even though it is
## available in the same dataset, is strongly related to quality, and is
## strongly related to altitude. Part 2 adds it back on the identical rows.
##
## The model is fitted on the complete cases flagged in script 01 (cc_r2), so
## that the Part 2 corrected model can use exactly the same observations.
## ---------------------------------------------------------------------------
mdat <- dat[dat$cc_r2, ]
mdat$Processing.Method <- factor(mdat$Processing.Method)
mdat$Country.of.Origin <- factor(mdat$Country.of.Origin)

r2 <- lm(Total.Cup.Points ~ altitude_km + Moisture + Processing.Method, data = mdat)
r2_sum <- summary(r2)

r2_n       <- length(residuals(r2))
r2_r2      <- r2_sum$r.squared
r2_adj_r2  <- r2_sum$adj.r.squared
r2_slope   <- unname(coef(r2)["altitude_km"])
r2_slope_p <- r2_sum$coefficients["altitude_km", "Pr(>|t|)"]
r2_eq      <- fitted_equation(r2)

r2_stats <- data.frame(
  label = "R2",
  quantity = c("altitude_km coefficient", "p-value for altitude_km",
               "R-squared", "adjusted R-squared",
               "residual standard error", "sample size n",
               "predictors in the model (excluding intercept)",
               "complete cases available", "rows dropped for missing data"),
  value = c(r2_slope, r2_slope_p, r2_r2, r2_adj_r2, r2_sum$sigma, r2_n,
            length(coef(r2)) - 1, nrow(mdat), nrow(dat) - nrow(mdat)),
  stringsAsFactors = FALSE
)
save_table(r2_stats, "R2_model_statistics.csv")
save_table(coef_table(r2, "R2"), "R2_coefficients.csv")
save_table(data.frame(label = "R2", fitted_equation = r2_eq,
                      stringsAsFactors = FALSE),
           "R2_fitted_equation.csv")

write_reg_summary(
  r2, "R2  MULTIPLE LINEAR REGRESSION (Part 1 model, country omitted)",
  "R2_multiple_regression.txt",
  notes = c(
    "Part 1 technique: omitted-variable bias.",
    "Moisture and Processing.Method are included, which makes the model look",
    "like it controls for confounding. Country.of.Origin is left out even",
    "though it sits in the same CSV and is related both to altitude and to",
    "score, so its effect is absorbed into the altitude coefficient.",
    sprintf("Fitted on the %d complete cases flagged by cc_r2 in script 01, so",
            r2_n),
    "the Part 2 corrected model uses the identical observations."
  )
)

cat("\n----------------------------------------------------------\n")
cat("R2  Multiple regression: ~ altitude_km + Moisture + Processing.Method\n")
cat("----------------------------------------------------------\n")
cat("  Technique ........ omitted-variable bias (Country.of.Origin excluded)\n")
cat(sprintf("  Sample size n ................. %d complete cases\n", r2_n))
cat(sprintf("  Altitude coefficient .......... %.4f (p = %.4g)\n", r2_slope, r2_slope_p))
cat(sprintf("  R-squared ..................... %.5f\n", r2_r2))
cat(sprintf("  Adjusted R-squared ............ %.5f\n", r2_adj_r2))
cat(sprintf("  Fitted equation ...............\n    %s\n", r2_eq))
cat("\n  All coefficients:\n")
print(coef_table(r2, "R2")[, c("term", "estimate", "std_error", "p_value")],
      row.names = FALSE, digits = 4)
cat("\n  MISLEADING INTERPRETATION (what Part 1 sells):\n")
cat(sprintf("    \"Even after controlling for moisture content and processing\n"))
cat(sprintf("     method, altitude still adds %.2f cup points per kilometre\n", r2_slope))
cat("     (p = ", formatC(r2_slope_p, format = "g", digits = 3),
    "). The altitude effect survives adjustment.\"\n", sep = "")
cat("  Saved: outputs/regression_summaries/R2_multiple_regression.txt\n")

## Why leaving country out matters: country is related to BOTH altitude and
## score, which is the textbook condition for omitted-variable bias. These
## diagnostics are computed here and interpreted in Part 2.
country_means <- do.call(rbind, lapply(split(mdat, mdat$Country.of.Origin), function(g) {
  data.frame(country = as.character(g$Country.of.Origin[1]), n = nrow(g),
             mean_altitude_km = mean(g$altitude_km),
             mean_points = mean(g$Total.Cup.Points),
             stringsAsFactors = FALSE)
}))
country_means <- country_means[order(-country_means$n), ]
save_table(country_means, "R2_country_confounding_evidence.csv")

## Strength of the country-altitude and country-score links on the model rows.
conf_tbl <- data.frame(
  label = "R2",
  quantity = c("ANOVA R-squared, altitude_km explained by Country.of.Origin",
               "ANOVA R-squared, Total.Cup.Points explained by Country.of.Origin",
               "countries represented", "countries with only 1 observation"),
  value = c(summary(lm(altitude_km ~ Country.of.Origin, data = mdat))$r.squared,
            summary(lm(Total.Cup.Points ~ Country.of.Origin, data = mdat))$r.squared,
            nlevels(mdat$Country.of.Origin),
            sum(table(mdat$Country.of.Origin) == 1)),
  stringsAsFactors = FALSE
)
save_table(conf_tbl, "R2_omitted_variable_conditions.csv")

cat("\n  Evidence that country is a genuine confounder on these rows:\n")
print(conf_tbl[, c("quantity", "value")], row.names = FALSE, digits = 4)

## ---------------------------------------------------------------------------
## Hand key values to the report generator
## ---------------------------------------------------------------------------
saveRDS(list(
  r1 = list(n = r1_n, intercept = r1_intercept, slope = r1_slope,
            r2 = r1_r2, adj_r2 = r1_adj_r2, cor = r1_cor,
            slope_p = r1_slope_p, cor_p = r1_cor_test$p.value,
            sigma = r1_sum$sigma, equation = r1_eq,
            coefs = coef_table(r1, "R1")),
  r2 = list(n = r2_n, slope = r2_slope, slope_p = r2_slope_p,
            r2 = r2_r2, adj_r2 = r2_adj_r2, sigma = r2_sum$sigma,
            equation = r2_eq, coefs = coef_table(r2, "R2"),
            n_dropped = nrow(dat) - nrow(mdat),
            confounding = conf_tbl)
), file.path("outputs", "values", "04_part1_regressions.rds"))

cat("\n[OK] 04_part1_regressions.R complete\n")
