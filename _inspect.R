d <- read.csv("arabica_data_cleaned.csv", stringsAsFactors = FALSE)
cat("dim:", dim(d), "\n\n")
cols <- c("altitude_mean_meters", "Total.Cup.Points", "Moisture",
          "Processing.Method", "Country.of.Origin")
for (cl in cols) {
  v <- d[[cl]]
  cat(sprintf("%-22s class=%-9s NA=%-4d blank=%d\n", cl, class(v), sum(is.na(v)),
              if (is.character(v)) sum(trimws(v) == "") else 0L))
}
cat("\n--- altitude_mean_meters ---\n"); print(summary(d$altitude_mean_meters))
cat("\n--- Total.Cup.Points ---\n"); print(summary(d$Total.Cup.Points))
cat("\n--- Moisture ---\n"); print(summary(d$Moisture))
cat("\nTCP <= 0 :", sum(d$Total.Cup.Points <= 0, na.rm = TRUE), "\n")
cat("alt NA   :", sum(is.na(d$altitude_mean_meters)), "\n")
cat("alt <100 :", sum(d$altitude_mean_meters < 100, na.rm = TRUE), "\n")
cat("alt >3500:", sum(d$altitude_mean_meters > 3500, na.rm = TRUE), "\n")
keep <- !is.na(d$altitude_mean_meters) & d$altitude_mean_meters >= 100 &
        d$altitude_mean_meters <= 3500 & !is.na(d$Total.Cup.Points) &
        d$Total.Cup.Points > 0
cat("\nKEPT:", sum(keep), "\n")
cat("\n--- Processing.Method (kept) ---\n"); print(table(d$Processing.Method[keep], useNA = "ifany"))
cat("\n--- n distinct countries (kept):", length(unique(d$Country.of.Origin[keep])), "\n")
cat("\n--- countries with 1 obs (kept) ---\n")
tb <- table(d$Country.of.Origin[keep]); print(tb[tb <= 2])
cat("\nMoisture == 0 (kept):", sum(d$Moisture[keep] == 0, na.rm = TRUE), "\n")
cat("altitude max values (kept, top 8):\n")
print(sort(d$altitude_mean_meters[keep], decreasing = TRUE)[1:8])