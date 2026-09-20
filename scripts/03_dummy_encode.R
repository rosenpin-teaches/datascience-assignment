# Assignment 2 — Stage 4: create numeric model inputs.
# The course recommends dummy variables for nominal categories.
# Run this script from the project root, after 02_mice_imputation.R.

library(fastDummies)

processed_dir <- file.path("data", "processed")
output_dir <- file.path("outputs", "03_dummy_encoding")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

model_inputs <- c(
  "sex", "age", "address",
  "famsize", "Pstatus", "Medu", "Fedu", "Mjob", "Fjob", "guardian",
  "famsup", "internet", "famrel",
  "activities", "romantic", "freetime", "goout"
)

factor_inputs <- c(
  "sex", "address", "famsize", "Pstatus", "Mjob", "Fjob", "guardian",
  "famsup", "internet", "activities", "romantic"
)

files <- c(Math = "math_mice_clean.csv", Portuguese = "portuguese_mice_clean.csv")

encode_one_dataset <- function(path, dataset) {
  clean_data <- read.csv(path, check.names = FALSE)
  model_data <- clean_data[, c(model_inputs, "alc_score")]
  model_data[factor_inputs] <- lapply(model_data[factor_inputs], factor)

  # Remove one category per factor to avoid the dummy-variable trap in regression.
  encoded_data <- dummy_cols(
    model_data,
    select_columns = factor_inputs,
    remove_first_dummy = TRUE,
    remove_selected_columns = TRUE
  )

  if (!all(vapply(encoded_data, is.numeric, logical(1)))) {
    stop("Dummy encoding did not produce a fully numeric model dataset.", call. = FALSE)
  }

  write.csv(
    encoded_data,
    file.path(processed_dir, paste0(tolower(dataset), "_model_data.csv")),
    row.names = FALSE
  )

  data.frame(
    dataset = dataset,
    rows = nrow(encoded_data),
    model_inputs = ncol(encoded_data) - 1,
    outcome = "alc_score",
    stringsAsFactors = FALSE
  )
}

summary_rows <- mapply(
  FUN = function(file, dataset) encode_one_dataset(file.path(processed_dir, file), dataset),
  file = files,
  dataset = names(files),
  SIMPLIFY = FALSE
)
summary_table <- do.call(rbind, summary_rows)

column_rows <- do.call(rbind, lapply(names(files), function(dataset) {
  data <- read.csv(
    file.path(processed_dir, paste0(tolower(dataset), "_model_data.csv")),
    check.names = FALSE
  )
  data.frame(dataset = dataset, column = names(data), stringsAsFactors = FALSE)
}))

write.csv(summary_table, file.path(output_dir, "model_data_summary.csv"), row.names = FALSE)
write.csv(column_rows, file.path(output_dir, "model_data_columns.csv"), row.names = FALSE)

cat("\nNumeric model-data summary\n")
print(summary_table, row.names = FALSE)
