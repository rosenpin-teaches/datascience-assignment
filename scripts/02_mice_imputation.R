# Assignment 2 — Stage 3: impute the selected model inputs with MICE.
# This keeps the original group workflow: filter missing alcohol outcomes,
# make categories factors, run MICE, choose one completed version, and continue.
# Run this script from the project root, after 01_data_audit.R.

library(mice)

options(stringsAsFactors = FALSE)

data_dir <- file.path("data", "raw")
processed_dir <- file.path("data", "processed")
output_dir <- file.path("outputs", "02_mice_imputation")
dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

files <- c(Math = "student_mat.csv", Portuguese = "student_por.csv")

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

read_students <- function(path) {
  read.csv(path, na.strings = c("", "NA"), check.names = FALSE)
}

# The lecture says to choose the version that best matches observed values.
# We use one simple rule across all included fields:
# - numeric fields: compare imputed and observed means;
# - categorical fields: compare imputed and observed category proportions.
# A smaller average difference means a closer match to the observed data.
score_imputations <- function(imp, original, dataset) {
  imputed_variables <- names(imp$imp)[vapply(imp$imp, nrow, integer(1)) > 0]
  rows <- list()

  for (i in seq_len(imp$m)) {
    completed <- complete(imp, i)

    for (variable in imputed_variables) {
      missing_rows <- is.na(original[[variable]])
      observed <- original[[variable]][!missing_rows]
      imputed <- completed[[variable]][missing_rows]

      if (is.numeric(original[[variable]])) {
        observed_sd <- sd(observed)
        difference <- if (is.na(observed_sd) || observed_sd == 0) {
          0
        } else {
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

      rows[[length(rows) + 1]] <- data.frame(
        dataset = dataset,
        imputation = i,
        variable = variable,
        comparison = comparison,
        difference = difference,
        stringsAsFactors = FALSE
      )
    }
  }

  do.call(rbind, rows)
}

impute_one_dataset <- function(path, dataset) {
  raw_data <- read_students(path)
  missing_outcome <- is.na(raw_data$Dalc) | is.na(raw_data$Walc)

  # The proposal excludes rows with a missing alcohol outcome.
  data_for_analysis <- raw_data[!missing_outcome, c(model_inputs, "Dalc", "Walc")]
  data_for_analysis[factor_inputs] <- lapply(data_for_analysis[factor_inputs], factor)

  # We use m = 5 and maxit = 20 as in the course MICE example.
  set.seed(123)
  imp <- mice(data_for_analysis, m = 5, maxit = 20, printFlag = FALSE, seed = 123)
  scores <- score_imputations(imp, data_for_analysis, dataset)

  mean_scores <- aggregate(difference ~ dataset + imputation, scores, mean)
  names(mean_scores)[names(mean_scores) == "difference"] <- "mean_difference"
  selected_imputation <- mean_scores$imputation[which.min(mean_scores$mean_difference)]

  clean_data <- complete(imp, selected_imputation)

  list(
    clean_data = clean_data,
    scores = scores,
    mean_scores = mean_scores,
    selected_imputation = selected_imputation
  )
}

results <- lapply(names(files), function(dataset) {
  impute_one_dataset(file.path(data_dir, files[[dataset]]), dataset)
})
names(results) <- names(files)

for (dataset in names(results)) {
  result <- results[[dataset]]
  write.csv(
    result$clean_data,
    file.path(processed_dir, paste0(tolower(dataset), "_mice_clean.csv")),
    row.names = FALSE
  )
  cat(sprintf(
    "%s: selected MICE version %d; %d rows; %d missing cells remain.\n",
    dataset,
    result$selected_imputation,
    nrow(result$clean_data),
    sum(is.na(result$clean_data))
  ))
}

all_scores <- do.call(rbind, lapply(results, `[[`, "scores"))
all_mean_scores <- do.call(rbind, lapply(results, `[[`, "mean_scores"))
write.csv(all_scores, file.path(output_dir, "imputation_variable_scores.csv"), row.names = FALSE)
write.csv(all_mean_scores, file.path(output_dir, "imputation_mean_scores.csv"), row.names = FALSE)

cat("\nMean difference by MICE version (lower is a closer match):\n")
print(all_mean_scores, row.names = FALSE)
