# RQ1 figures and an additional error check.
# Run research_question_1.R first. This script reads results without fitting models.

library(ggplot2)
library(dplyr)

output_dir <- file.path("outputs", "rq1")
if (!file.exists(file.path(output_dir, "penalty_scores.csv"))) {
  stop("Run scripts/research_question_1.R first.")
}
metrics <- read.csv(file.path(output_dir, "model_metrics.csv"))
coefficients <- read.csv(file.path(output_dir, "model_coefficients.csv"))
predictions <- read.csv(file.path(output_dir, "test_predictions.csv"))
penalties <- read.csv(file.path(output_dir, "penalty_scores.csv"))

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
coefficients$label <- unname(input_labels[coefficients$input])

# Separate coefficient and penalty plots for each dataset.
for (dataset_name in c("Math", "Portuguese")) {
  file_prefix <- tolower(dataset_name)
  kept <- coefficients %>% filter(dataset == dataset_name, selected, coefficient != 0)
  selected_model <- metrics$model[metrics$dataset == dataset_name & metrics$selected]
  coefficient_plot <- ggplot(
    kept, aes(standardized_coefficient, reorder(label, standardized_coefficient))
  ) +
    geom_col(fill = "#4477AA") + geom_vline(xintercept = 0, color = "grey40") +
    labs(title = paste(dataset_name, ": inputs retained by", selected_model),
         subtitle = "Coefficient per 1 SD of input; alcohol remains on its 1 to 5 scale",
         x = "Standardized coefficient", y = NULL,
         caption = "Conditional associations, not causal effects. Zero coefficients omitted.") +
    theme_minimal(base_size = 12)
  ggsave(file.path(output_dir, paste0(file_prefix, "_coefficients.png")),
         coefficient_plot, width = 8, height = 6, dpi = 300)

  cv_data <- penalties %>% filter(dataset == dataset_name)
  chosen <- metrics %>% filter(dataset == dataset_name, model %in% c("Ridge", "LASSO"))
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
}

# A score of 3 is a descriptive comparison point, not a clinical risk threshold.
error_groups <- predictions %>%
  mutate(score_group = ifelse(observed >= 3, "3 or higher", "Below 3")) %>%
  group_by(dataset, score_group) %>%
  summarise(n = n(), MAE = mean(abs(predicted - observed)),
            mean_error = mean(predicted - observed), .groups = "drop")
write.csv(error_groups, file.path(output_dir, "error_by_score.csv"), row.names = FALSE)

# Compare the eight largest retained associations across the datasets.
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

set.seed(123)
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
