# Research question 1: Which characteristics predict alcohol consumption?
#
# Steps:
# 1. Load the separate training and test files made by preprocessing.R.
# 2. Compare simple predictions, ordinary regression, ridge, and LASSO.
# 3. Report test errors and the inputs kept by LASSO.

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

  cat("\nTest results (lower MAE and RMSE are better; higher R2 is better)\n")
  print(results, row.names = FALSE)
  cat("\nRidge lambda: ", ridge$lambda.min,
      "\nLASSO lambda: ", lasso$lambda.min, "\n", sep = "")

  # LASSO sets some input coefficients exactly to zero.
  lasso_coefficients <- as.matrix(coef(lasso, s = "lambda.min"))[-1, 1]
  kept <- lasso_coefficients[lasso_coefficients != 0]
  cat("\nInputs kept by LASSO: ", length(kept), " of ", length(inputs), "\n", sep = "")
  print(sort(kept), digits = 3)
}

analyse_dataset("data/processed/Math_train.csv", "data/processed/Math_test.csv", "Math")
analyse_dataset("data/processed/Lang_train.csv", "data/processed/Lang_test.csv", "Portuguese")
