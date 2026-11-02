## Mean and median of Total.Cup.Points and altitude_mean_meters
## on every row of the source file (no analysis filters).
##
## Run: Rscript scripts/means_all_observations.R

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
setwd(find_project_root())

raw <- read.csv("arabica_data_cleaned.csv", stringsAsFactors = FALSE)

tcp <- raw$Total.Cup.Points
alt <- raw$altitude_mean_meters

out <- data.frame(
  quantity = c(
    "n_observations",
    "n_total_cup_points_non_missing",
    "mean_total_cup_points",
    "median_total_cup_points",
    "n_altitude_mean_meters_non_missing",
    "n_altitude_mean_meters_missing",
    "mean_altitude_mean_meters",
    "median_altitude_mean_meters"
  ),
  value = c(
    nrow(raw),
    sum(!is.na(tcp)),
    mean(tcp, na.rm = TRUE),
    median(tcp, na.rm = TRUE),
    sum(!is.na(alt)),
    sum(is.na(alt)),
    mean(alt, na.rm = TRUE),
    median(alt, na.rm = TRUE)
  ),
  stringsAsFactors = FALSE
)

dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
write.csv(out, "outputs/tables/means_all_observations.csv", row.names = FALSE)

cat("Source file: arabica_data_cleaned.csv\n")
cat("All observations (no filters)\n\n")
print(out, row.names = FALSE)