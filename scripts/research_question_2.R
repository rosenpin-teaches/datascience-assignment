# Research question 2: Are there student profiles with different alcohol scores?
#
# Steps:
# 1. Combine the training and test files for each dataset.
# 2. Cluster student inputs with k-means and compare with Gower + PAM.
# 3. Choose the number of groups using silhouette scores, without alcohol.
# 4. Describe the groups and compare alcohol scores using Kruskal-Wallis.
# 5. Save tables and figures for the report.

library(cluster)
library(ggplot2)
library(dplyr)

output_dir <- file.path("outputs", "rq2")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

numeric_vars <- c("age", "Medu", "Fedu", "famrel", "freetime", "goout")
binary_vars <- c("sex_M", "address_U", "famsize_LE3", "Pstatus_T",
                 "famsup_yes", "internet_yes", "activities_yes", "romantic_yes")
nominal_vars <- c("Mjob", "Fjob", "guardian")
numeric_labels <- c("Age", "Mother's education", "Father's education",
                    "Family relationships", "Free time", "Going out")
profile_colors <- c("#4477AA", "#EE6677", "#228833", "#CCBB44", "#AA3377", "#66CCEE")

# The dropped category applies when all its dummy columns are zero.
reconstruct_factor <- function(df, cols, reference_level) {
  levels_found <- sub(".*_", "", cols)
  out <- rep(reference_level, nrow(df))
  for (i in seq_along(cols)) {
    out[df[[cols[i]]] == 1] <- levels_found[i]
  }
  factor(out)
}

describe_profiles <- function(full, groups, distance, coordinates,
                              method, dataset_name, file_prefix) {
  full$profile <- factor(groups)
  silhouette_values <- silhouette(groups, distance)[, 3]
  alcohol <- full %>% group_by(profile) %>%
    summarise(n = n(), mean = mean(alc_score), sd = sd(alc_score),
              median = median(alc_score), IQR = IQR(alc_score), .groups = "drop")
  alcohol$dataset <- dataset_name
  alcohol$method <- method

  # Alcohol was not used to choose the groups or their number.
  kw <- kruskal.test(alc_score ~ profile, data = full)
  tests <- data.frame(
    dataset = dataset_name, method = method, n = nrow(full),
    k = nlevels(full$profile), silhouette = mean(silhouette_values),
    negative_silhouette_percent = mean(silhouette_values < 0) * 100,
    H = unname(kw$statistic), df = unname(kw$parameter), p = kw$p.value
  )
  pairwise <- data.frame(group1 = character(), group2 = character(), adjusted_p = numeric())
  if (kw$p.value < 0.05) {
    # Use approximate Wilcoxon p-values because alcohol ratings contain ties.
    pw <- pairwise.wilcox.test(full$alc_score, full$profile,
                               p.adjust.method = "bonferroni", exact = FALSE)
    pairwise <- as.data.frame(as.table(pw$p.value))
    names(pairwise) <- c("group1", "group2", "adjusted_p")
    pairwise <- pairwise[!is.na(pairwise$adjusted_p), ]
  }
  pairwise$dataset <- rep(dataset_name, nrow(pairwise))
  pairwise$method <- rep(method, nrow(pairwise))

  # Keep numeric summaries on their original scales for interpretation.
  numeric_summary <- data.frame()
  category_summary <- data.frame()
  standardized_numeric <- as.data.frame(scale(full[, numeric_vars]))
  for (profile in levels(full$profile)) {
    rows <- full$profile == profile
    values <- full[rows, numeric_vars]
    numeric_summary <- rbind(numeric_summary, data.frame(
      profile = profile, variable = numeric_vars,
      mean = sapply(values, mean), sd = sapply(values, sd),
      median = sapply(values, median), IQR = sapply(values, IQR),
      standardized_mean = sapply(standardized_numeric[rows, ], mean)
    ))
    for (variable in c(binary_vars, nominal_vars)) {
      counts <- table(full[rows, variable])
      category_summary <- rbind(category_summary, data.frame(
        profile = profile, variable = variable, category = names(counts),
        n = as.numeric(counts), percent = as.numeric(counts) / sum(rows) * 100
      ))
    }
  }
  numeric_summary$dataset <- dataset_name
  numeric_summary$method <- method
  category_summary$dataset <- dataset_name
  category_summary$method <- method

  cat("\n", dataset_name, " ", method, "\n", sep = "")
  print(tests, row.names = FALSE)
  print(alcohol, row.names = FALSE)
  if (nrow(pairwise) > 0) print(pairwise, row.names = FALSE)

  alcohol_plot <- ggplot(full, aes(profile, alc_score, fill = profile)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.7) +
    geom_jitter(width = 0.13, height = 0.02, alpha = 0.18, size = 1) +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
    scale_fill_manual(values = profile_colors, guide = "none") +
    scale_x_discrete(labels = paste0(alcohol$profile, "\n(n=", alcohol$n, ")")) +
    labs(title = paste(dataset_name, ": alcohol scores by", method, "profile"),
         subtitle = "Alcohol was excluded from clustering; diamond = mean",
         x = "Profile", y = "Average workday and weekend rating (1 to 5)") +
    theme_minimal(base_size = 12)
  ggsave(file.path(output_dir, paste0(file_prefix, "_alcohol.png")),
         alcohol_plot, width = 7, height = 4.5, dpi = 300)

  numeric_summary$label <- factor(
    numeric_labels[match(numeric_summary$variable, numeric_vars)], levels = rev(numeric_labels)
  )
  profile_plot <- ggplot(numeric_summary, aes(profile, label, fill = standardized_mean)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.2f", mean)), size = 3.5) +
    scale_fill_gradient2(low = "#4477AA", mid = "white", high = "#EE6677",
                         midpoint = 0, name = "Mean Z-score") +
    labs(title = paste(dataset_name, ": characteristics of", method, "profiles"),
         subtitle = "Numbers are original-scale means; color is relative to all students",
         x = "Profile", y = NULL) + theme_minimal(base_size = 12)
  ggsave(file.path(output_dir, paste0(file_prefix, "_profile_means.png")),
         profile_plot, width = 7, height = 4.5, dpi = 300)

  social <- full %>% group_by(profile) %>%
    summarise(across(all_of(c("sex_M", "activities_yes", "famsup_yes", "romantic_yes")), mean),
              .groups = "drop")
  social_long <- data.frame()
  social_labels <- c("Male", "Activities", "Family support", "Relationship")
  for (i in 2:ncol(social)) {
    social_long <- rbind(social_long, data.frame(
      profile = social$profile, characteristic = social_labels[i - 1],
      percent = social[[i]] * 100
    ))
  }
  social_plot <- ggplot(social_long, aes(profile, percent, fill = profile)) +
    geom_col() + geom_text(aes(label = paste0(round(percent), "%")), vjust = -0.4) +
    facet_wrap(~ characteristic) + scale_y_continuous(limits = c(0, 110), breaks = c(0, 50, 100)) +
    scale_fill_manual(values = profile_colors, guide = "none") +
    labs(title = paste(dataset_name, ": social characteristics of", method, "profiles"),
         x = "Profile", y = "Students (%)") + theme_minimal(base_size = 12)
  ggsave(file.path(output_dir, paste0(file_prefix, "_social_profiles.png")),
         social_plot, width = 8, height = 5, dpi = 300)

  map <- data.frame(dimension1 = coordinates[, 1], dimension2 = coordinates[, 2],
                     profile = full$profile)
  map_plot <- ggplot(map, aes(dimension1, dimension2, color = profile)) +
    geom_point(alpha = 0.55, size = 1.5) + scale_color_manual(values = profile_colors) +
    labs(title = paste(dataset_name, ":", method, "profiles in two dimensions"),
         subtitle = "A visual summary only; clustering used all inputs",
         x = "Dimension 1", y = "Dimension 2", color = "Profile") +
    theme_minimal(base_size = 12)
  ggsave(file.path(output_dir, paste0(file_prefix, "_profile_map.png")),
         map_plot, width = 7, height = 5, dpi = 300)

  assignments <- data.frame(prepared_row = seq_len(nrow(full)),
                             profile = full$profile, alc_score = full$alc_score)
  write.csv(assignments, file.path(output_dir, paste0(file_prefix, "_assignments.csv")),
            row.names = FALSE)
  list(alcohol = alcohol, tests = tests, pairwise = pairwise,
       numeric = numeric_summary, categories = category_summary, students = full)
}

profile_dataset <- function(train_file, test_file, dataset_name, file_prefix, k_range = 2:6) {
  full <- rbind(read.csv(train_file, check.names = FALSE),
                read.csv(test_file, check.names = FALSE))
  inputs <- setdiff(names(full), "alc_score")
  stopifnot(!anyNA(full), all(full$alc_score >= 1 & full$alc_score <= 5))
  stopifnot(!any(c("Dalc", "Walc") %in% inputs))

  # Restore all categories for clustering, then standardize inputs as in Lecture 6.
  # Unlike regression, clustering does not need a dropped reference category.
  kmeans_data <- full[, inputs]
  mother_jobs <- c("Mjob_health", "Mjob_other", "Mjob_services", "Mjob_teacher")
  father_jobs <- c("Fjob_health", "Fjob_other", "Fjob_services", "Fjob_teacher")
  guardians <- c("guardian_mother", "guardian_other")
  kmeans_data$Mjob_at_home <- 1 - rowSums(full[, mother_jobs])
  kmeans_data$Fjob_at_home <- 1 - rowSums(full[, father_jobs])
  kmeans_data$guardian_father <- 1 - rowSums(full[, guardians])
  stopifnot(all(rowSums(kmeans_data[, c(mother_jobs, "Mjob_at_home")]) == 1),
            all(rowSums(kmeans_data[, c(father_jobs, "Fjob_at_home")]) == 1),
            all(rowSums(kmeans_data[, c(guardians, "guardian_father")]) == 1))
  kmeans_data <- scale(kmeans_data)
  stopifnot(!anyNA(kmeans_data))
  euclidean <- dist(kmeans_data)
  fits <- list()
  choices <- data.frame()
  for (k in k_range) {
    set.seed(123)
    fits[[as.character(k)]] <- kmeans(kmeans_data, centers = k, nstart = 25, iter.max = 100)
    fit <- fits[[as.character(k)]]
    choices <- rbind(choices, data.frame(
      method = "K-means", k = k,
      silhouette = mean(silhouette(fit$cluster, euclidean)[, 3]),
      within_ss = fit$tot.withinss
    ))
  }
  best_k <- choices$k[which.max(choices$silhouette)]
  best_kmeans <- fits[[as.character(best_k)]]
  pca <- prcomp(kmeans_data, center = TRUE, scale. = FALSE)
  full$Mjob <- reconstruct_factor(full, mother_jobs, "at_home")
  full$Fjob <- reconstruct_factor(full, father_jobs, "at_home")
  full$guardian <- reconstruct_factor(full, guardians, "father")
  km <- describe_profiles(full, best_kmeans$cluster, euclidean, pca$x[, 1:2],
                          "K-means", dataset_name, paste0(file_prefix, "_kmeans"))

  # Gower + PAM is an additional mixed-data comparison, not the lecture method.
  # Reconstruct single nominal fields so jobs and guardian each count once.
  mixed_data <- full[, c(numeric_vars, binary_vars, nominal_vars)]
  for (variable in binary_vars) mixed_data[[variable]] <- factor(mixed_data[[variable]])
  gower <- daisy(mixed_data, metric = "gower")
  pam_choices <- data.frame()
  pam_fits <- list()
  for (k in k_range) {
    fit <- pam(gower, diss = TRUE, k = k)
    pam_fits[[as.character(k)]] <- fit
    pam_choices <- rbind(pam_choices, data.frame(
      method = "Gower + PAM", k = k, silhouette = fit$silinfo$avg.width, within_ss = NA
    ))
  }
  best_pam <- pam_fits[[as.character(pam_choices$k[which.max(pam_choices$silhouette)])]]
  pam_map <- cmdscale(gower, k = 2, add = TRUE)$points
  pam_result <- describe_profiles(full, best_pam$clustering, gower, pam_map,
                                 "Gower + PAM", dataset_name, paste0(file_prefix, "_pam"))
  write.csv(mixed_data[best_pam$medoids, ],
            file.path(output_dir, paste0(file_prefix, "_pam_medoids.csv")), row.names = FALSE)

  choices <- rbind(choices, pam_choices)
  choices$dataset <- dataset_name
  choice_plot <- ggplot(choices, aes(k, silhouette)) +
    geom_line(color = "#4477AA") + geom_point(color = "#4477AA") + facet_wrap(~ method) +
    scale_x_continuous(breaks = k_range) +
    labs(title = paste(dataset_name, ": choosing the number of profiles"),
         subtitle = "Choose the largest silhouette within each method; alcohol is not used",
         x = "Number of profiles", y = "Average silhouette") + theme_minimal(base_size = 12)
  ggsave(file.path(output_dir, paste0(file_prefix, "_silhouettes.png")),
         choice_plot, width = 9, height = 4, dpi = 300)
  elbow_plot <- ggplot(choices %>% filter(method == "K-means"), aes(k, within_ss)) +
    geom_line(color = "#4477AA") + geom_point(color = "#4477AA") +
    scale_x_continuous(breaks = k_range) +
    labs(title = paste(dataset_name, ": k-means elbow plot"),
         x = "Number of profiles", y = "Within-cluster sum of squares") +
    theme_minimal(base_size = 12)
  ggsave(file.path(output_dir, paste0(file_prefix, "_elbow.png")),
         elbow_plot, width = 6, height = 4, dpi = 300)
  list(kmeans = km, pam = pam_result, choices = choices,
       kmeans_fit = best_kmeans, pam_fit = best_pam)
}

math_profiles <- profile_dataset("data/processed/Math_train.csv", "data/processed/Math_test.csv", "Math", "math")
portuguese_profiles <- profile_dataset("data/processed/Lang_train.csv", "data/processed/Lang_test.csv", "Portuguese", "portuguese")
for (table_name in c("alcohol", "tests", "pairwise", "numeric", "categories")) {
  result <- rbind(math_profiles$kmeans[[table_name]], math_profiles$pam[[table_name]],
                  portuguese_profiles$kmeans[[table_name]], portuguese_profiles$pam[[table_name]])
  write.csv(result, file.path(output_dir, paste0("profile_", table_name, ".csv")), row.names = FALSE)
}
write.csv(rbind(math_profiles$choices, portuguese_profiles$choices),
          file.path(output_dir, "cluster_choices.csv"), row.names = FALSE)

kmeans_students <- rbind(
  math_profiles$kmeans$students %>% mutate(dataset = "Math"),
  portuguese_profiles$kmeans$students %>% mutate(dataset = "Portuguese")
)
test_subtitle <- paste0(
  "Kruskal-Wallis: Math p = ", formatC(math_profiles$kmeans$tests$p, format = "f", digits = 3),
  "; Portuguese p = ", formatC(portuguese_profiles$kmeans$tests$p, format = "f", digits = 3)
)
alcohol_comparison <- ggplot(kmeans_students, aes(profile, alc_score, fill = profile)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  geom_jitter(width = 0.13, height = 0.02, alpha = 0.12, size = 1) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
  facet_wrap(~ dataset, scales = "free_x") + scale_fill_manual(values = profile_colors, guide = "none") +
  labs(title = "Alcohol scores by k-means profile",
       subtitle = test_subtitle,
       x = "Profile", y = "Average workday and weekend rating (1 to 5)",
       caption = "Diamond = mean. Profile numbers are separate labels within each dataset.") +
  theme_minimal(base_size = 12)
ggsave(file.path(output_dir, "kmeans_alcohol_comparison.png"), alcohol_comparison,
       width = 9, height = 5, dpi = 300)
