# Research question 1: Which characteristics predict alcohol consumption?
#
# Steps:
# 1. Load the separate training and test files made by preprocessing.R.
# 2. Compare simple predictions, ordinary regression, ridge, and LASSO.
# 3. Report test errors and the inputs kept by LASSO.
# 4. Save results. Run rq1_plots.R separately for figures.

library(glmnet)

# Errors are measured on the original 1 to 5 alcohol scale.
test_scores <- function(model, actual, predicted) {
  data.frame(
    model = model,
    MAE = mean(abs(actual - predicted)),
    RMSE = sqrt(mean((actual - predicted)^2)),
    R2 = 1 - sum((actual - predicted)^2) / sum((actual - mean(actual))^2)
  )
}

analyse_dataset <- function(train_file, test_file, dataset_name) {
  if (!file.exists(train_file) || !file.exists(test_file)) {
    stop("Run scripts/preprocessing.R first to create the training and test files.")
  }

  train <- read.csv(train_file, check.names = FALSE)
  test <- read.csv(test_file, check.names = FALSE)
  inputs <- setdiff(names(train), "alc_score")
  train_x <- as.matrix(train[, inputs])
  test_x <- as.matrix(test[, inputs])

  cat("\n", dataset_name, ": ", nrow(train), " training, ",
      nrow(test), " test students\n", sep = "")

  # These simple predictions give us something to compare the models with.
  mean_prediction <- rep(mean(train$alc_score), nrow(test))
  median_prediction <- rep(median(train$alc_score), nrow(test))

  ordinary <- lm(alc_score ~ ., data = train)
  ordinary_prediction <- predict(ordinary, newdata = test)

  # Five-fold cross-validation chooses the penalty using training data only.
  # glmnet standardizes inputs using the training data; alcohol stays on 1 to 5.
  set.seed(123)
  folds <- sample(rep(1:5, length.out = nrow(train)))

  # Compare ordinary regression and simple predictions using the same folds.
  ordinary_cv <- mean_cv <- median_cv <- rep(NA_real_, nrow(train))
  for (fold in 1:5) {
    fold_train <- train[folds != fold, ]
    fold_check <- train[folds == fold, ]
    fit <- lm(alc_score ~ ., data = fold_train)
    ordinary_cv[folds == fold] <- predict(fit, newdata = fold_check)
    mean_cv[folds == fold] <- mean(fold_train$alc_score)
    median_cv[folds == fold] <- median(fold_train$alc_score)
  }

  ridge <- cv.glmnet(train_x, train$alc_score, alpha = 0,
                     foldid = folds, type.measure = "mae", standardize = TRUE)
  lasso <- cv.glmnet(train_x, train$alc_score, alpha = 1,
                     foldid = folds, type.measure = "mae", standardize = TRUE)

  ridge_prediction <- as.numeric(predict(ridge, newx = test_x, s = "lambda.min"))
  lasso_prediction <- as.numeric(predict(lasso, newx = test_x, s = "lambda.min"))

  results <- rbind(
    test_scores("Training mean", test$alc_score, mean_prediction),
    test_scores("Training median", test$alc_score, median_prediction),
    test_scores("Ordinary regression", test$alc_score, ordinary_prediction),
    test_scores("Ridge", test$alc_score, ridge_prediction),
    test_scores("LASSO", test$alc_score, lasso_prediction)
  )
  results$CV_MAE <- c(
    mean(abs(train$alc_score - mean_cv)),
    mean(abs(train$alc_score - median_cv)),
    mean(abs(train$alc_score - ordinary_cv)), min(ridge$cvm), min(lasso$cvm)
  )
  results$dataset <- dataset_name
  results$n_train <- nrow(train)
  results$n_test <- nrow(test)
  results$lambda <- c(NA, NA, NA, ridge$lambda.min, lasso$lambda.min)
  selected_model <- results$model[which.min(results$CV_MAE)]
  results$selected <- results$model == selected_model

  cat("\nTest results (lower MAE and RMSE are better; higher R2 is better)\n")
  print(results, row.names = FALSE)
  cat("\nRidge lambda: ", ridge$lambda.min,
      "\nLASSO lambda: ", lasso$lambda.min, "\n", sep = "")
  cat("\nSelected by training CV: ", selected_model, "\n", sep = "")

  # LASSO sets some input coefficients exactly to zero.
  lasso_coefficients <- as.matrix(coef(lasso, s = "lambda.min"))[-1, 1]
  kept <- lasso_coefficients[lasso_coefficients != 0]
  cat("\nInputs kept by LASSO: ", length(kept), " of ", length(inputs), "\n", sep = "")
  print(sort(kept), digits = 3)

  # Save coefficients and predictions for the report figures.
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
output_dir <- file.path("outputs", "rq1")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
write.csv(metrics, file.path(output_dir, "model_metrics.csv"), row.names = FALSE)
write.csv(coefficients, file.path(output_dir, "model_coefficients.csv"), row.names = FALSE)
write.csv(predictions, file.path(output_dir, "test_predictions.csv"), row.names = FALSE)
write.csv(penalties, file.path(output_dir, "penalty_scores.csv"), row.names = FALSE)
