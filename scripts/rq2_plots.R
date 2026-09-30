# RQ2 figures.
# Run research_question_2.R first. This script reads results without fitting clusters.

library(ggplot2)
library(dplyr)

output_dir <- file.path("outputs", "rq2")
if (!file.exists(file.path(output_dir, "profile_tests.csv"))) {
  stop("Run scripts/research_question_2.R first.")
}
tests <- read.csv(file.path(output_dir, "profile_tests.csv"))
alcohol_summaries <- read.csv(file.path(output_dir, "profile_alcohol.csv"))
numeric_summaries <- read.csv(file.path(output_dir, "profile_numeric.csv"))
category_summaries <- read.csv(file.path(output_dir, "profile_categories.csv"))
cluster_choices <- read.csv(file.path(output_dir, "cluster_choices.csv"))

numeric_labels <- c(age = "Age", Medu = "Mother's education", Fedu = "Father's education",
                    famrel = "Family relationships", freetime = "Free time", goout = "Going out")
social_labels <- c(sex_M = "Male", activities_yes = "Activities",
                   famsup_yes = "Family support", romantic_yes = "Relationship")
profile_colors <- c("#4477AA", "#EE6677", "#228833", "#CCBB44", "#AA3377", "#66CCEE")
kmeans_students <- data.frame()
set.seed(123)

# Each row of the test table describes one dataset and clustering method.
for (i in seq_len(nrow(tests))) {
  dataset_name <- tests$dataset[i]
  method_name <- tests$method[i]
  method_prefix <- if (method_name == "K-means") "kmeans" else "pam"
  file_prefix <- paste0(tolower(dataset_name), "_", method_prefix)
  students <- read.csv(file.path(output_dir, paste0(file_prefix, "_assignments.csv")))
  if (!all(c("dimension1", "dimension2") %in% names(students))) {
    stop("Run scripts/research_question_2.R again to save plot coordinates.")
  }
  students$profile <- factor(students$profile)
  alcohol <- alcohol_summaries %>% filter(dataset == dataset_name, method == method_name)
  numeric_summary <- numeric_summaries %>% filter(dataset == dataset_name, method == method_name)
  category_summary <- category_summaries %>% filter(dataset == dataset_name, method == method_name)

  alcohol_plot <- ggplot(students, aes(profile, alc_score, fill = profile)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.7) +
    geom_jitter(width = 0.13, height = 0.02, alpha = 0.18, size = 1) +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
    scale_fill_manual(values = profile_colors, guide = "none") +
    scale_x_discrete(labels = paste0(alcohol$profile, "\n(n=", alcohol$n, ")")) +
    labs(title = paste(dataset_name, ": alcohol scores by", method_name, "profile"),
         subtitle = "Alcohol was excluded from clustering; diamond = mean",
         x = "Profile", y = "Average workday and weekend rating (1 to 5)") +
    theme_minimal(base_size = 12)
  ggsave(file.path(output_dir, paste0(file_prefix, "_alcohol.png")),
         alcohol_plot, width = 7, height = 4.5, dpi = 300)

  numeric_summary$profile <- factor(numeric_summary$profile)
  numeric_summary$label <- factor(unname(numeric_labels[numeric_summary$variable]),
                                  levels = rev(unname(numeric_labels)))
  profile_plot <- ggplot(numeric_summary, aes(profile, label, fill = standardized_mean)) +
    geom_tile(color = "white") + geom_text(aes(label = sprintf("%.2f", mean)), size = 3.5) +
    scale_fill_gradient2(low = "#4477AA", mid = "white", high = "#EE6677",
                         midpoint = 0, name = "Mean Z-score") +
    labs(title = paste(dataset_name, ": characteristics of", method_name, "profiles"),
         subtitle = "Numbers are original-scale means; color is relative to all students",
         x = "Profile", y = NULL) + theme_minimal(base_size = 12)
  ggsave(file.path(output_dir, paste0(file_prefix, "_profile_means.png")),
         profile_plot, width = 7, height = 4.5, dpi = 300)

  # Summing the 'yes' category also gives 0% when a group contains only 'no'.
  social <- category_summary %>% filter(variable %in% names(social_labels)) %>%
    group_by(profile, variable) %>%
    summarise(percent = sum(percent[category == "1"]), .groups = "drop")
  social$profile <- factor(social$profile)
  social$characteristic <- unname(social_labels[social$variable])
  social_plot <- ggplot(social, aes(profile, percent, fill = profile)) +
    geom_col() + geom_text(aes(label = paste0(round(percent), "%")), vjust = -0.4) +
    facet_wrap(~ characteristic) + scale_y_continuous(limits = c(0, 110), breaks = c(0, 50, 100)) +
    scale_fill_manual(values = profile_colors, guide = "none") +
    labs(title = paste(dataset_name, ": social characteristics of", method_name, "profiles"),
         x = "Profile", y = "Students (%)") + theme_minimal(base_size = 12)
  ggsave(file.path(output_dir, paste0(file_prefix, "_social_profiles.png")),
         social_plot, width = 8, height = 5, dpi = 300)

  map_plot <- ggplot(students, aes(dimension1, dimension2, color = profile)) +
    geom_point(alpha = 0.55, size = 1.5) + scale_color_manual(values = profile_colors) +
    labs(title = paste(dataset_name, ":", method_name, "profiles in two dimensions"),
         subtitle = "A visual summary only; clustering used all inputs",
         x = "Dimension 1", y = "Dimension 2", color = "Profile") +
    theme_minimal(base_size = 12)
  ggsave(file.path(output_dir, paste0(file_prefix, "_profile_map.png")),
         map_plot, width = 7, height = 5, dpi = 300)

  if (method_name == "K-means") {
    students$dataset <- dataset_name
    kmeans_students <- rbind(kmeans_students, students)
  }
}

# Silhouette and elbow plots show how the number of groups was chosen.
for (dataset_name in c("Math", "Portuguese")) {
  choices <- cluster_choices %>% filter(dataset == dataset_name)
  file_prefix <- tolower(dataset_name)
  choice_plot <- ggplot(choices, aes(k, silhouette)) +
    geom_line(color = "#4477AA") + geom_point(color = "#4477AA") + facet_wrap(~ method) +
    scale_x_continuous(breaks = sort(unique(choices$k))) +
    labs(title = paste(dataset_name, ": choosing the number of profiles"),
         subtitle = "Choose the largest silhouette within each method; alcohol is not used",
         x = "Number of profiles", y = "Average silhouette") + theme_minimal(base_size = 12)
  ggsave(file.path(output_dir, paste0(file_prefix, "_silhouettes.png")),
         choice_plot, width = 9, height = 4, dpi = 300)
  elbow_plot <- ggplot(choices %>% filter(method == "K-means"), aes(k, within_ss)) +
    geom_line(color = "#4477AA") + geom_point(color = "#4477AA") +
    scale_x_continuous(breaks = sort(unique(choices$k))) +
    labs(title = paste(dataset_name, ": k-means elbow plot"),
         x = "Number of profiles", y = "Within-cluster sum of squares") +
    theme_minimal(base_size = 12)
  ggsave(file.path(output_dir, paste0(file_prefix, "_elbow.png")),
         elbow_plot, width = 6, height = 4, dpi = 300)
}

kmeans_tests <- tests %>% filter(method == "K-means")
test_subtitle <- paste0(
  "Kruskal-Wallis: Math p = ", formatC(kmeans_tests$p[kmeans_tests$dataset == "Math"], format = "f", digits = 3),
  "; Portuguese p = ", formatC(kmeans_tests$p[kmeans_tests$dataset == "Portuguese"], format = "f", digits = 3)
)
alcohol_comparison <- ggplot(kmeans_students, aes(profile, alc_score, fill = profile)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  geom_jitter(width = 0.13, height = 0.02, alpha = 0.12, size = 1) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
  facet_wrap(~ dataset, scales = "free_x") + scale_fill_manual(values = profile_colors, guide = "none") +
  labs(title = "Alcohol scores by k-means profile", subtitle = test_subtitle,
       x = "Profile", y = "Average workday and weekend rating (1 to 5)",
       caption = "Diamond = mean. Profile numbers are separate labels within each dataset.") +
  theme_minimal(base_size = 12)
ggsave(file.path(output_dir, "kmeans_alcohol_comparison.png"), alcohol_comparison,
       width = 9, height = 5, dpi = 300)
