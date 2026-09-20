# Data Science Assignment 2: Preprocessing
#
# Open this project in RStudio, open this file, and click Source.
# This script follows the group’s original workflow:
# 1. Load Math and Portuguese data separately
# 2. Check data quality
# 3. Remove rows missing Dalc or Walc
# 4. Prepare factors and use MICE
# 5. Create dummy variables for the model

library(dplyr)
library(mice)
library(fastDummies)

data_dir <- file.path("data", "raw")
processed_dir <- file.path("data", "processed")
output_dir <- file.path("outputs", "preprocessing")
dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Load the two course datasets
Math <- read.csv(file.path(data_dir, "student_mat.csv"),
                 na.strings = c("", "NA"), check.names = FALSE)
Portuguese <- read.csv(file.path(data_dir, "student_por.csv"),
                       na.strings = c("", "NA"), check.names = FALSE)

# These are the fields selected by the group for the research question.
model_inputs <- c(
  "sex", "age", "address",
  "famsize", "Pstatus", "Medu", "Fedu", "Mjob", "Fjob", "guardian",
  "famsup", "internet", "famrel",
  "activities", "romantic", "freetime", "goout"
)

# These categorical fields must be factors before MICE.
factor_inputs <- c(
  "sex", "address", "famsize", "Pstatus", "Mjob", "Fjob", "guardian",
  "famsup", "internet", "activities", "romantic"
)

range_checks <- list(
  age = c(15, 22), Medu = c(0, 4), Fedu = c(0, 4),
  famrel = c(1, 5), freetime = c(1, 5), goout = c(1, 5),
  Dalc = c(1, 5), Walc = c(1, 5)
)

# For each MICE version, compare imputed values with observed values.
# Lower values mean the imputed data are closer to the observed data.
score_mice_versions <- function(imp, data, dataset) {
  imputed_variables <- names(imp$imp)[vapply(imp$imp, nrow, integer(1)) > 0]
  scores <- list()

  for (version in seq_len(imp$m)) {
    completed_data <- complete(imp, version)

    for (variable in imputed_variables) {
      missing_rows <- is.na(data[[variable]])
      observed <- data[[variable]][!missing_rows]
      imputed <- completed_data[[variable]][missing_rows]

      if (is.numeric(data[[variable]])) {
        observed_sd <- sd(observed)
        difference <- if (is.na(observed_sd) || observed_sd == 0) 0 else {
          abs(mean(imputed) - mean(observed)) / observed_sd
        }
        comparison <- "standardized difference in mean"
      } else {
        levels_to_compare <- union(as.character(observed), as.character(imputed))
        observed_prop <- table(factor(observed, levels = levels_to_compare)) / length(observed)
        imputed_prop <- table(factor(imputed, levels = levels_to_compare)) / length(imputed)
        difference <- sum(abs(observed_prop - imputed_prop)) / 2
        comparison <- "difference in category proportions"
      }

      scores[[length(scores) + 1]] <- data.frame(
        dataset = dataset,
        version = version,
        variable = variable,
        comparison = comparison,
        difference = difference
      )
    }
  }

  do.call(rbind, scores)
}

prepare_dataset <- function(data, dataset_name) {
  # Basic data-quality checks from the original group code
  missing_outcome <- is.na(data$Dalc) | is.na(data$Walc)
  dataset_summary <- data.frame(
    dataset = dataset_name,
    raw_rows = nrow(data),
    columns = ncol(data),
    duplicate_rows = sum(duplicated(data)),
    rows_missing_Dalc_or_Walc = sum(missing_outcome),
    rows_after_outcome_filter = sum(!missing_outcome)
  )

  missingness <- do.call(rbind, lapply(c("raw", "after_outcome_filter"), function(stage) {
    current_data <- if (stage == "raw") data else data[!missing_outcome, ]
    data.frame(
      dataset = dataset_name,
      stage = stage,
      variable = names(current_data),
      missing_n = colSums(is.na(current_data)),
      missing_pct = round(100 * colMeans(is.na(current_data)), 1)
    )
  }))

  range_results <- do.call(rbind, lapply(names(range_checks), function(variable) {
    values <- data[[variable]]
    allowed <- range_checks[[variable]]
    data.frame(
      dataset = dataset_name,
      variable = variable,
      allowed_range = paste(allowed, collapse = " to "),
      values_outside_range = sum(values < allowed[1] | values > allowed[2], na.rm = TRUE)
    )
  }))

  # The proposal removes rows without both alcohol measures.
  data <- data %>%
    filter(!is.na(Dalc) & !is.na(Walc)) %>%
    select(all_of(c(model_inputs, "Dalc", "Walc")))

  # MICE needs categorical data to be factors.
  data <- data %>% mutate(across(all_of(factor_inputs), as.factor))

  # Course MICE workflow: 5 versions, 20 iterations, then choose the closest match.
  init <- mice(data, maxit = 0, printFlag = FALSE)
  methods <- init$method
  methods[c("Dalc", "Walc")] <- "" # alcohol outcomes are already complete

  set.seed(123)
  imp <- mice(data, method = methods, m = 5, maxit = 20,
              seed = 123, printFlag = FALSE)

  version_scores <- score_mice_versions(imp, data, dataset_name)
  mean_scores <- aggregate(difference ~ dataset + version, version_scores, mean)
  names(mean_scores)[names(mean_scores) == "difference"] <- "mean_difference"
  selected_version <- mean_scores$version[which.min(mean_scores$mean_difference)]

  # Version 2 is selected for both datasets in this run because it has the
  # smallest average difference from the observed values.
  clean_data <- complete(imp, selected_version)

  # Turn nominal categories into dummy variables for later modelling.
  model_data <- dummy_cols(
    clean_data,
    select_columns = factor_inputs,
    remove_first_dummy = TRUE,
    remove_selected_columns = TRUE
  )

  write.csv(clean_data,
            file.path(processed_dir, paste0(tolower(dataset_name), "_clean.csv")),
            row.names = FALSE)
  write.csv(model_data,
            file.path(processed_dir, paste0(tolower(dataset_name), "_model_data.csv")),
            row.names = FALSE)

  list(
    summary = dataset_summary,
    missingness = missingness,
    range_results = range_results,
    version_scores = version_scores,
    mean_scores = mean_scores,
    selected_version = selected_version,
    model_summary = data.frame(
      dataset = dataset_name,
      rows = nrow(model_data),
      model_inputs = ncol(model_data) - 2,
      outcomes = "Dalc and Walc"
    ),
    model_columns = data.frame(dataset = dataset_name, column = names(model_data))
  )
}

# Run the same preprocessing steps for both cohorts.
math_results <- prepare_dataset(Math, "Math")
portuguese_results <- prepare_dataset(Portuguese, "Portuguese")
results <- list(math_results, portuguese_results)

dataset_summary <- do.call(rbind, lapply(results, `[[`, "summary"))
missingness <- do.call(rbind, lapply(results, `[[`, "missingness"))
range_results <- do.call(rbind, lapply(results, `[[`, "range_results"))
version_scores <- do.call(rbind, lapply(results, `[[`, "version_scores"))
mean_scores <- do.call(rbind, lapply(results, `[[`, "mean_scores"))
model_summary <- do.call(rbind, lapply(results, `[[`, "model_summary"))
model_columns <- do.call(rbind, lapply(results, `[[`, "model_columns"))

write.csv(dataset_summary, file.path(output_dir, "dataset_summary.csv"), row.names = FALSE)
write.csv(missingness, file.path(output_dir, "missingness.csv"), row.names = FALSE)
write.csv(range_results, file.path(output_dir, "range_checks.csv"), row.names = FALSE)
write.csv(version_scores, file.path(output_dir, "imputation_variable_scores.csv"), row.names = FALSE)
write.csv(mean_scores, file.path(output_dir, "imputation_mean_scores.csv"), row.names = FALSE)
write.csv(model_summary, file.path(output_dir, "model_data_summary.csv"), row.names = FALSE)
write.csv(model_columns, file.path(output_dir, "model_data_columns.csv"), row.names = FALSE)

cat("\nDataset summary\n")
print(dataset_summary, row.names = FALSE)
cat("\nMICE version scores (lower is better)\n")
print(mean_scores, row.names = FALSE)
cat("\nModel-data summary\n")
print(model_summary, row.names = FALSE)
