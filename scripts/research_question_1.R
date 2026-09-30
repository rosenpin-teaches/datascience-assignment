# Research question 1: Which characteristics predict alcohol consumption?
#
# Steps:
# 1. Load the training and test files made by preprocessing.R.
# 2. Compare ordinary regression, ridge, and LASSO with simple predictions.
# 3. Choose the model and penalties using five-fold cross-validation.
# 4. Save results. Run rq1_plots.R separately for figures.

library(glmnet)

output_dir <- file.path("outputs", "rq1")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

test_scores <- function(model, actual, predicted) {
  data.frame(
    model = model, MAE = mean(abs(actual - predicted)),
    RMSE = sqrt(mean((actual - predicted)^2)),
    R2 = 1 - sum((actual - predicted)^2) / sum((actual - mean(actual))^2)
  )
}

analyse_dataset <- function(train_file, test_file, dataset_name) {
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
  mean_prediction <- rep(mean(train$alc_score), nrow(test))
  median_prediction <- rep(median(train$alc_score), nrow(test))
  ordinary_prediction <- as.numeric(predict(ordinary, newdata = test))
  ridge_prediction <- as.numeric(predict(ridge, newx = test_x, s = "lambda.min"))
  lasso_prediction <- as.numeric(predict(lasso, newx = test_x, s = "lambda.min"))
  results <- rbind(
    test_scores("Training mean", test$alc_score, mean_prediction),
    test_scores("Training median", test$alc_score, median_prediction),
    test_scores("Ordinary regression", test$alc_score, ordinary_prediction),
    test_scores("Ridge", test$alc_score, ridge_prediction),
    test_scores("LASSO", test$alc_score, lasso_prediction)
  )
  results$CV_MAE <- validation_mae
  results$dataset <- dataset_name
  results$n_train <- nrow(train)
  results$n_test <- nrow(test)
  results$lambda <- c(NA, NA, NA, ridge$lambda.min, lasso$lambda.min)
  selected_model <- results$model[which.min(results$CV_MAE)]
  results$selected <- results$model == selected_model
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
  coefficients$dataset <- dataset_name
  coefficients$selected <- coefficients$model == selected_model
  kept <- coefficients[coefficients$selected & coefficients$coefficient != 0, ]
  cat("\nInputs kept by selected model\n")
  print(kept[, c("input", "coefficient", "standardized_coefficient")], row.names = FALSE)

  # Save validation scores so penalty plots do not need to refit models.
  penalties <- rbind(
    data.frame(model = "Ridge", lambda = ridge$lambda, MAE = ridge$cvm, SE = ridge$cvsd),
    data.frame(model = "LASSO", lambda = lasso$lambda, MAE = lasso$cvm, SE = lasso$cvsd)
  )
  penalties$dataset <- dataset_name
  test_predictions <- list(mean_prediction, median_prediction, ordinary_prediction,
                           ridge_prediction, lasso_prediction)
  prediction_data <- data.frame(
    dataset = dataset_name, observed = test$alc_score,
    predicted = test_predictions[[which.min(results$CV_MAE)]], model = selected_model
  )
  list(metrics = results, coefficients = coefficients, predictions = prediction_data,
       models = list(ordinary = ordinary, ridge = ridge, lasso = lasso),
       penalties = penalties, selected_model = selected_model)
}

math_regression <- analyse_dataset("data/processed/Math_train.csv", "data/processed/Math_test.csv", "Math")
portuguese_regression <- analyse_dataset("data/processed/Lang_train.csv", "data/processed/Lang_test.csv", "Portuguese")
metrics <- rbind(math_regression$metrics, portuguese_regression$metrics)
coefficients <- rbind(math_regression$coefficients, portuguese_regression$coefficients)
predictions <- rbind(math_regression$predictions, portuguese_regression$predictions)
penalties <- rbind(math_regression$penalties, portuguese_regression$penalties)
write.csv(metrics, file.path(output_dir, "model_metrics.csv"), row.names = FALSE)
write.csv(coefficients, file.path(output_dir, "model_coefficients.csv"), row.names = FALSE)
write.csv(predictions, file.path(output_dir, "test_predictions.csv"), row.names = FALSE)
write.csv(penalties, file.path(output_dir, "penalty_scores.csv"), row.names = FALSE)
