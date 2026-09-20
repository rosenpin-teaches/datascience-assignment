# Assignment 2 — Stage 2: inspect the raw data before changing it.
# Run this script from the project root.

options(stringsAsFactors = FALSE)

data_dir <- file.path("data", "raw")
output_dir <- file.path("outputs", "01_data_audit")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

files <- c(Math = "student_mat.csv", Portuguese = "student_por.csv")
paths <- file.path(data_dir, files)

if (!all(file.exists(paths))) {
  stop(
    "Put student_mat.csv and student_por.csv in data/raw before running this script.",
    call. = FALSE
  )
}

read_students <- function(path) {
  read.csv(path, na.strings = c("", "NA"), check.names = FALSE)
}

range_checks <- list(
  age = c(15, 22),
  Medu = c(0, 4), Fedu = c(0, 4),
  traveltime = c(1, 4), studytime = c(1, 4), failures = c(0, 4),
  famrel = c(1, 5), freetime = c(1, 5), goout = c(1, 5),
  Dalc = c(1, 5), Walc = c(1, 5), health = c(1, 5),
  G1 = c(0, 20), G2 = c(0, 20), G3 = c(0, 20)
)

audit_one_dataset <- function(data, dataset) {
  missing_outcome <- is.na(data$Dalc) | is.na(data$Walc)
  analysis_data <- data[!missing_outcome, , drop = FALSE]

  summary_row <- data.frame(
    dataset = dataset,
    raw_rows = nrow(data),
    columns = ncol(data),
    duplicate_rows = sum(duplicated(data)),
    rows_missing_Dalc_or_Walc = sum(missing_outcome),
    rows_after_outcome_filter = nrow(analysis_data),
    stringsAsFactors = FALSE
  )

  missingness <- do.call(rbind, lapply(c("raw", "after_outcome_filter"), function(stage) {
    current <- if (stage == "raw") data else analysis_data
    data.frame(
      dataset = dataset,
      stage = stage,
      variable = names(current),
      missing_n = colSums(is.na(current)),
      missing_pct = round(100 * colMeans(is.na(current)), 1),
      stringsAsFactors = FALSE
    )
  }))

  range_results <- do.call(rbind, lapply(names(range_checks), function(variable) {
    x <- data[[variable]]
    allowed <- range_checks[[variable]]
    data.frame(
      dataset = dataset,
      variable = variable,
      allowed_range = paste(allowed, collapse = " to "),
      values_outside_range = sum(x < allowed[1] | x > allowed[2], na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))

  alcohol_distribution <- as.data.frame(table(
    alc_score = (analysis_data$Dalc + analysis_data$Walc) / 2
  ))
  names(alcohol_distribution) <- c("alc_score", "n")
  alcohol_distribution$dataset <- dataset

  list(
    summary = summary_row,
    missingness = missingness,
    range_results = range_results,
    alcohol_distribution = alcohol_distribution
  )
}

results <- mapply(
  FUN = function(path, dataset) audit_one_dataset(read_students(path), dataset),
  path = paths,
  dataset = names(files),
  SIMPLIFY = FALSE
)

summary_table <- do.call(rbind, lapply(results, `[[`, "summary"))
missingness_table <- do.call(rbind, lapply(results, `[[`, "missingness"))
range_table <- do.call(rbind, lapply(results, `[[`, "range_results"))
alcohol_table <- do.call(rbind, lapply(results, `[[`, "alcohol_distribution"))

write.csv(summary_table, file.path(output_dir, "dataset_summary.csv"), row.names = FALSE)
write.csv(missingness_table, file.path(output_dir, "missingness.csv"), row.names = FALSE)
write.csv(range_table, file.path(output_dir, "range_checks.csv"), row.names = FALSE)
write.csv(alcohol_table, file.path(output_dir, "alcohol_score_distribution.csv"), row.names = FALSE)

cat("\nDataset summary\n")
print(summary_table, row.names = FALSE)
cat("\nVariables with missing values after removing missing alcohol outcomes\n")
print(subset(missingness_table, stage == "after_outcome_filter" & missing_n > 0), row.names = FALSE)
cat("\nRange checks\n")
print(range_table, row.names = FALSE)
cat("\nSaved audit tables to:", normalizePath(output_dir), "\n")
