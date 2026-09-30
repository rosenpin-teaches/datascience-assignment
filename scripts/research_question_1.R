# Research question 1: Which characteristics predict alcohol consumption?
#
# Steps:
# 1. Load the training and test files made by preprocessing.R.
# 2. Compare ordinary regression, ridge, and LASSO with simple predictions.
# 3. Choose the model and penalties using five-fold cross-validation.
# 4. Save test results, coefficients, and figures.

library(glmnet)
library(ggplot2)
library(dplyr)

output_dir <- file.path("outputs", "rq1")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

input_labels <- c(
  age = "Age", Medu = "Mother's education", Fedu = "Father's education",
  famrel = "Family relationships", freetime = "Free time", goout = "Going out",
  sex_M = "Male", address_U = "Urban address", famsize_LE3 = "Family size <= 3",
  Pstatus_T = "Parents together", Mjob_health = "Mother: health",
  Mjob_other = "Mother: other job", Mjob_services = "Mother: services",
  Mjob_teacher = "Mother: teacher", Fjob_health = "Father: health",
  Fjob_other = "Father: other job", Fjob_services = "Father: services",
  Fjob_teacher = "Father: teacher", guardian_mother = "Guardian: mother",
  guardian_other = "Guardian: other", famsup_yes = "Family support",
  internet_yes = "Home internet", activities_yes = "Activities", romantic_yes = "Relationship"
)

test_scores <- function(model, actual, predicted) {
  data.frame(
    model = model, MAE = mean(abs(actual - predicted)),
    RMSE = sqrt(mean((actual - predicted)^2)),
    R2 = 1 - sum((actual - predicted)^2) / sum((actual - mean(actual))^2)
  )
}

analyse_dataset <- function(train_file, test_file, dataset_name, file_prefix) {
  if (!file.exists(train_file) || !file.exists(test_file)) {
    stop("Run scripts/preprocessing.R first.")
  }
  train <- read.csv(train_file, check.names = FALSE)
  test <- read.csv(test_file, check.names = FALSE)
  inputs <- setdiff(names(train), "alc_score")
  train_x <- as.matrix(train[, inputs])
  test_x <- as.matrix(test[, inputs])
  stopifnot(!anyNA(train), !anyNA(test), identical(names(train), names(test)))
  stopifnot(!any(c("Dalc", "Walc") %in% inputs))

  # Use the same five groups for every model comparison.
  set.seed(123)
  folds <- sample(rep(1:5, length.out = nrow(train)))
  ordinary_cv <- mean_cv <- median_cv <- rep(NA_real_, nrow(train))
  for (fold in 1:5) {
    fold_train <- train[folds != fold, ]
    fold_check <- train[folds == fold, ]
    fit <- lm(alc_score ~ ., data = fold_train)
    ordinary_cv[folds == fold] <- predict(fit, newdata = fold_check)
    mean_cv[folds == fold] <- mean(fold_train$alc_score)
    median_cv[folds == fold] <- median(fold_train$alc_score)
  }

  # Standardize inputs in each fit. Predictions use alcohol-score units.
  # Linear predictions are not restricted to the observed 1 to 5 range.
  ridge <- cv.glmnet(train_x, train$alc_score, alpha = 0,
                     foldid = folds, type.measure = "mae", standardize = TRUE)
  lasso <- cv.glmnet(train_x, train$alc_score, alpha = 1,
                     foldid = folds, type.measure = "mae", standardize = TRUE)
  validation_mae <- c(
    mean(abs(train$alc_score - mean_cv)),
    mean(abs(train$alc_score - median_cv)),
    mean(abs(train$alc_score - ordinary_cv)), min(ridge$cvm), min(lasso$cvm)
  )

  # Fit ordinary regression on all training students, then check test students.
  ordinary <- lm(alc_score ~ ., data = train)
  predictions <- list(
    "Training mean" = rep(mean(train$alc_score), nrow(test)),
    "Training median" = rep(median(train$alc_score), nrow(test)),
    "Ordinary regression" = as.numeric(predict(ordinary, newdata = test)),
    Ridge = as.numeric(predict(ridge, newx = test_x, s = "lambda.min")),
    LASSO = as.numeric(predict(lasso, newx = test_x, s = "lambda.min"))
  )
  results <- do.call(rbind, lapply(names(predictions), function(model) {
    test_scores(model, test$alc_score, predictions[[model]])
  }))
  results$CV_MAE <- validation_mae
  results$dataset <- dataset_name
  results$n_train <- nrow(train)
  results$n_test <- nrow(test)
  results$lambda <- c(NA, NA, NA, ridge$lambda.min, lasso$lambda.min)
  results$selected <- results$CV_MAE == min(results$CV_MAE)
  selected_model <- results$model[which.min(results$CV_MAE)]
  cat("\n", dataset_name, ": model selected by training CV = ", selected_model, "\n", sep = "")
  print(results, row.names = FALSE)

  # Raw coefficients describe a one-unit change or a category comparison.
  # Multiplying by the input SD puts coefficient plots on comparable scales.
  coefficients <- rbind(
    data.frame(model = "Ordinary regression", input = inputs, coefficient = coef(ordinary)[inputs]),
    data.frame(model = "Ridge", input = inputs,
               coefficient = as.matrix(coef(ridge, s = "lambda.min"))[inputs, 1]),
    data.frame(model = "LASSO", input = inputs,
               coefficient = as.matrix(coef(lasso, s = "lambda.min"))[inputs, 1])
  )
  input_sd <- sapply(train[, inputs], sd)
  coefficients$standardized_coefficient <- coefficients$coefficient * input_sd[coefficients$input]
  coefficients$label <- unname(input_labels[coefficients$input])
  coefficients$dataset <- dataset_name
  coefficients$selected <- coefficients$model == selected_model
  kept <- coefficients %>% filter(selected, coefficient != 0)
  kept <- kept %>% arrange(desc(abs(standardized_coefficient)))
  cat("\nInputs kept by selected model\n")
  print(kept[, c("input", "coefficient", "standardized_coefficient")], row.names = FALSE)

  coefficient_plot <- ggplot(
    kept, aes(x = standardized_coefficient, y = reorder(label, standardized_coefficient))
  ) +
    geom_col(fill = "#4477AA") + geom_vline(xintercept = 0, color = "grey40") +
    labs(title = paste(dataset_name, ": inputs retained by", selected_model),
         subtitle = "Coefficient per 1 SD of input; alcohol remains on its 1 to 5 scale",
         x = "Standardized coefficient", y = NULL,
         caption = "Conditional associations, not causal effects. Zero coefficients omitted.") +
    theme_minimal(base_size = 12)
  ggsave(file.path(output_dir, paste0(file_prefix, "_coefficients.png")),
         coefficient_plot, width = 8, height = 6, dpi = 300)

  cv_data <- rbind(
    data.frame(model = "Ridge", lambda = ridge$lambda, MAE = ridge$cvm, SE = ridge$cvsd),
    data.frame(model = "LASSO", lambda = lasso$lambda, MAE = lasso$cvm, SE = lasso$cvsd)
  )
  chosen <- data.frame(model = c("Ridge", "LASSO"),
                       lambda = c(ridge$lambda.min, lasso$lambda.min))
  cv_plot <- ggplot(cv_data, aes(lambda, MAE)) +
    geom_ribbon(aes(ymin = MAE - SE, ymax = MAE + SE), fill = "#4477AA", alpha = 0.15) +
    geom_line(color = "#4477AA") +
    geom_vline(data = chosen, aes(xintercept = lambda), linetype = "dashed") +
    scale_x_log10() + facet_wrap(~ model, scales = "free_x") +
    labs(title = paste(dataset_name, ": choosing the penalty"),
         subtitle = "Five-fold training cross-validation; dashed line = lowest MAE",
         x = "Penalty (lambda, logarithmic scale)", y = "Cross-validation MAE",
         caption = "Shaded band = one standard error across validation folds.") +
    theme_minimal(base_size = 12)
  ggsave(file.path(output_dir, paste0(file_prefix, "_penalties.png")),
         cv_plot, width = 9, height = 4, dpi = 300)

  prediction_data <- data.frame(
    dataset = dataset_name, observed = test$alc_score,
    predicted = predictions[[selected_model]], model = selected_model
  )
  list(metrics = results, coefficients = coefficients, predictions = prediction_data,
       models = list(ordinary = ordinary, ridge = ridge, lasso = lasso),
       selected_model = selected_model)
}

math_regression <- analyse_dataset("data/processed/Math_train.csv", "data/processed/Math_test.csv", "Math", "math")
portuguese_regression <- analyse_dataset("data/processed/Lang_train.csv", "data/processed/Lang_test.csv", "Portuguese", "portuguese")
metrics <- rbind(math_regression$metrics, portuguese_regression$metrics)
coefficients <- rbind(math_regression$coefficients, portuguese_regression$coefficients)
predictions <- rbind(math_regression$predictions, portuguese_regression$predictions)
write.csv(metrics, file.path(output_dir, "model_metrics.csv"), row.names = FALSE)
write.csv(coefficients, file.path(output_dir, "model_coefficients.csv"), row.names = FALSE)
write.csv(predictions, file.path(output_dir, "test_predictions.csv"), row.names = FALSE)

# This descriptive check shows whether larger scores are harder to predict.
# A score of 3 is a convenient comparison point, not a clinical risk threshold.
error_groups <- predictions %>%
  mutate(score_group = ifelse(observed >= 3, "3 or higher", "Below 3")) %>%
  group_by(dataset, score_group) %>%
  summarise(n = n(), MAE = mean(abs(predicted - observed)),
            mean_error = mean(predicted - observed), .groups = "drop")
write.csv(error_groups, file.path(output_dir, "error_by_score.csv"), row.names = FALSE)

# Compare the eight largest retained associations across the two datasets.
selected_coefficients <- coefficients %>% filter(selected)
ranked_inputs <- selected_coefficients %>% group_by(input) %>%
  summarise(largest = max(abs(standardized_coefficient)), .groups = "drop") %>%
  arrange(desc(largest)) %>% slice_head(n = 8)
comparison <- selected_coefficients %>% filter(input %in% ranked_inputs$input)
comparison$label <- factor(comparison$label,
                           levels = unname(input_labels[rev(ranked_inputs$input)]))
comparison_plot <- ggplot(comparison, aes(standardized_coefficient, label, fill = dataset)) +
  geom_col(position = "dodge", width = 0.75) + geom_vline(xintercept = 0, color = "grey40") +
  scale_fill_manual(values = c("Math" = "#4477AA", "Portuguese" = "#EE6677")) +
  labs(title = "Strongest retained associations with alcohol scores",
       subtitle = "LASSO coefficients per 1 SD of input; eight largest in either dataset",
       x = "Standardized coefficient", y = NULL, fill = NULL,
       caption = "An absent bar means LASSO set that coefficient to zero. These are not causal effects.") +
  theme_minimal(base_size = 12)
ggsave(file.path(output_dir, "predictor_comparison.png"), comparison_plot,
       width = 9, height = 5.5, dpi = 300)

metrics$model <- factor(metrics$model, levels = c(
  "Training median", "Training mean", "Ordinary regression", "Ridge", "LASSO"
))
accuracy_plot <- ggplot(metrics, aes(model, MAE, fill = selected)) +
  geom_col(width = 0.7) + geom_text(aes(label = sprintf("%.3f", MAE)), vjust = -0.5) +
  facet_wrap(~ dataset) + scale_fill_manual(values = c("grey65", "#4477AA"), guide = "none") +
  labs(title = "Predicting alcohol consumption in test students",
       subtitle = "Lower MAE is better; blue = model selected using training cross-validation",
       x = NULL, y = "Test MAE (alcohol-score points)") +
  scale_x_discrete(labels = c("Median", "Mean", "Ordinary", "Ridge", "LASSO")) +
  scale_y_continuous(limits = c(0, max(metrics$MAE) * 1.15)) +
  theme_minimal(base_size = 12)
ggsave(file.path(output_dir, "model_accuracy.png"), accuracy_plot, width = 10, height = 4.5, dpi = 300)

prediction_plot <- ggplot(predictions, aes(observed, predicted)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40") +
  geom_jitter(width = 0.04, height = 0, alpha = 0.45, color = "#4477AA") +
  facet_wrap(~ dataset) + coord_equal() +
  scale_x_continuous(breaks = 1:5, limits = c(0.5, 5.25)) +
  scale_y_continuous(breaks = 1:5, limits = range(c(0.5, 5.25, predictions$predicted))) +
  labs(title = "Observed and predicted alcohol scores",
       subtitle = "Selected model; perfect predictions would lie on the dashed line",
       x = "Observed alcohol score", y = "Predicted alcohol score") +
  theme_minimal(base_size = 12)
ggsave(file.path(output_dir, "prediction_check.png"), prediction_plot, width = 9, height = 5.5, dpi = 300)
