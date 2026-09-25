# Exploratory data analysis
#
# Steps:
# 1. Load the processed Math and Portuguese datasets.
# 2. Describe alcohol consumption and the selected predictors.
# 3. Explore initial relationships with the alcohol score.
# 4. Save figures for both datasets.

library(dplyr)
library(ggplot2)

output_dir <- file.path("outputs", "exploration")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Numeric and ordinal fields kept on their original scales.
numeric_inputs <- c(
  "age", "Medu", "Fedu", "famrel", "freetime", "goout"
)

# Same exploration steps for each dataset.
explore_data <- function(input_file, dataset_name, file_prefix) {
  dataset <- read.csv(input_file, check.names = FALSE)

  cat("\n", dataset_name, " dataset\n", sep = "")
  cat("Students: ", nrow(dataset), "\n", sep = "")
  cat("Variables: ", ncol(dataset), "\n", sep = "")
  cat("Missing values: ", sum(is.na(dataset)), "\n", sep = "")

  # Describe the three alcohol variables.
  cat("\nAlcohol summary\n")
  print(summary(dataset[, c("Dalc", "Walc", "alc_score")]))

  cat("\nAlcohol score frequencies\n")
  alcohol_frequencies <- dataset %>%
    count(alc_score) %>%
    mutate(percent = round(100 * n / sum(n), 1))
  print(alcohol_frequencies)

  cat("\nWorkday and weekend alcohol table\n")
  print(addmargins(table(dataset$Dalc, dataset$Walc)))

  cat("\nSpearman correlation between Dalc and Walc\n")
  print(cor(dataset$Dalc, dataset$Walc, method = "spearman"))

  # Describe numeric and ordinal predictors.
  predictor_summary <- data.frame(
    variable = numeric_inputs,
    mean = round(sapply(dataset[numeric_inputs], mean), 2),
    standard_deviation = round(sapply(dataset[numeric_inputs], sd), 2),
    median = sapply(dataset[numeric_inputs], median),
    IQR = sapply(dataset[numeric_inputs], IQR),
    minimum = sapply(dataset[numeric_inputs], min),
    maximum = sapply(dataset[numeric_inputs], max),
    row.names = NULL
  )

  cat("\nNumeric and ordinal predictor summary\n")
  print(predictor_summary)

  # Describe the proportion represented by each dummy variable.
  outcome_columns <- c("Dalc", "Walc", "alc_score")
  dummy_inputs <- setdiff(names(dataset), c(numeric_inputs, outcome_columns))

  dummy_summary <- data.frame(
    variable = dummy_inputs,
    count = sapply(dataset[dummy_inputs], sum),
    percent = round(100 * sapply(dataset[dummy_inputs], mean), 1),
    row.names = NULL
  )

  cat("\nDummy variable summary\n")
  print(dummy_summary)

  # Show alcohol scores by going out, age, and sex.
  relationship_data <- dataset %>%
    mutate(sex = ifelse(sex_M == 1, "Male", "Female"))

  cat("\nAlcohol score by going out with friends\n")
  print(
    relationship_data %>%
      group_by(goout) %>%
      summarise(
        students = n(),
        median_alcohol = median(alc_score),
        IQR_alcohol = IQR(alc_score),
        mean_alcohol = round(mean(alc_score), 2),
        .groups = "drop"
      )
  )

  cat("\nAlcohol score by age\n")
  print(
    relationship_data %>%
      group_by(age) %>%
      summarise(
        students = n(),
        median_alcohol = median(alc_score),
        IQR_alcohol = IQR(alc_score),
        mean_alcohol = round(mean(alc_score), 2),
        .groups = "drop"
      )
  )

  cat("\nAlcohol score by sex\n")
  print(
    relationship_data %>%
      group_by(sex) %>%
      summarise(
        students = n(),
        median_alcohol = median(alc_score),
        IQR_alcohol = IQR(alc_score),
        mean_alcohol = round(mean(alc_score), 2),
        .groups = "drop"
      )
  )

  # Calculate exploratory correlations for all model inputs.
  model_inputs <- setdiff(names(dataset), outcome_columns)
  correlations <- sapply(
    dataset[model_inputs],
    function(variable) cor(variable, dataset$alc_score, method = "spearman")
  )

  correlation_summary <- data.frame(
    variable = names(correlations),
    correlation = round(as.numeric(correlations), 3),
    row.names = NULL
  ) %>%
    arrange(desc(abs(correlation)))

  cat("\nExploratory correlations with alcohol score\n")
  print(correlation_summary)

  # Prepare the alcohol variables for one faceted distribution plot.
  alcohol_plot_data <- rbind(
    data.frame(measure = "Workday alcohol", score = dataset$Dalc),
    data.frame(measure = "Weekend alcohol", score = dataset$Walc),
    data.frame(measure = "Average alcohol score", score = dataset$alc_score)
  )

  alcohol_plot <- ggplot(alcohol_plot_data, aes(x = score)) +
    geom_bar(fill = "#377EB8", width = 0.35) +
    facet_wrap(~ measure, ncol = 1) +
    scale_x_continuous(breaks = seq(1, 5, by = 0.5)) +
    labs(
      title = paste(dataset_name, "alcohol distributions"),
      x = "Alcohol score",
      y = "Number of students"
    ) +
    theme_minimal()

  goout_plot <- ggplot(
    relationship_data,
    aes(x = factor(goout), y = alc_score)
  ) +
    geom_boxplot(fill = "#4DAF4A", alpha = 0.7, outlier.shape = NA) +
    geom_jitter(width = 0.15, height = 0.04, alpha = 0.25) +
    labs(
      title = paste(dataset_name, "alcohol score by going out with friends"),
      x = "Going out with friends (1 to 5)",
      y = "Average alcohol score"
    ) +
    theme_minimal()

  age_plot <- ggplot(
    relationship_data,
    aes(x = factor(age), y = alc_score)
  ) +
    geom_boxplot(fill = "#984EA3", alpha = 0.7, outlier.shape = NA) +
    geom_jitter(width = 0.15, height = 0.04, alpha = 0.25) +
    labs(
      title = paste(dataset_name, "alcohol score by age"),
      x = "Age",
      y = "Average alcohol score"
    ) +
    theme_minimal()

  sex_plot <- ggplot(
    relationship_data,
    aes(x = sex, y = alc_score, fill = sex)
  ) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA) +
    geom_jitter(width = 0.12, height = 0.04, alpha = 0.25) +
    scale_fill_manual(values = c("Female" = "#E41A1C", "Male" = "#377EB8")) +
    labs(
      title = paste(dataset_name, "alcohol score by sex"),
      x = "Sex",
      y = "Average alcohol score"
    ) +
    theme_minimal() +
    theme(legend.position = "none")

  correlation_plot <- ggplot(
    correlation_summary,
    aes(x = reorder(variable, correlation), y = correlation, fill = correlation)
  ) +
    geom_col() +
    coord_flip() +
    scale_fill_gradient2(
      low = "#377EB8",
      mid = "white",
      high = "#E41A1C",
      midpoint = 0
    ) +
    labs(
      title = paste(dataset_name, "exploratory correlations with alcohol score"),
      x = "Student characteristic",
      y = "Spearman correlation"
    ) +
    theme_minimal() +
    theme(legend.position = "none")

  ggsave(
    file.path(output_dir, paste0(file_prefix, "_alcohol_distributions.png")),
    alcohol_plot,
    width = 7,
    height = 8,
    dpi = 300
  )

  ggsave(
    file.path(output_dir, paste0(file_prefix, "_goout_vs_alcohol.png")),
    goout_plot,
    width = 7,
    height = 5,
    dpi = 300
  )

  ggsave(
    file.path(output_dir, paste0(file_prefix, "_age_vs_alcohol.png")),
    age_plot,
    width = 7,
    height = 5,
    dpi = 300
  )

  ggsave(
    file.path(output_dir, paste0(file_prefix, "_sex_vs_alcohol.png")),
    sex_plot,
    width = 7,
    height = 5,
    dpi = 300
  )

  ggsave(
    file.path(output_dir, paste0(file_prefix, "_predictor_correlations.png")),
    correlation_plot,
    width = 8,
    height = 7,
    dpi = 300
  )
}

# Explore the Math dataset.
explore_data(
  input_file = "data/processed/Math.csv",
  dataset_name = "Math",
  file_prefix = "math"
)

# Explore the Portuguese dataset.
explore_data(
  input_file = "data/processed/Lang.csv",
  dataset_name = "Portuguese",
  file_prefix = "portuguese"
)
