# Research question 2 (robustness check): student profiles via Gower distance
# + PAM (k-medoids), instead of Euclidean k-means on standardized dummies.
#
# Why: research_question_2.R's k-means treats each one-hot dummy column
# (Mjob_health, Mjob_other, Mjob_services, Mjob_teacher, ...) as its own
# standardized numeric variable. Rare categories get huge standardized
# values (e.g. Mjob_health = +3.29 SD for the small group of students whose
# mother works in health), which dominates squared Euclidean distance and
# pulls the clustering toward parental job/education almost by construction.
#
# Fix: reconstruct the original categorical variables from their dummy
# columns, then use Gower distance, which compares each ORIGINAL variable
# (numeric, binary, or multi-category) on its own footing - a multi-level
# category counts once, not once per dummy column, and no standardization-
# driven inflation of rare categories.
#
# Steps:
# 1. Load train + test, reconstruct Mjob, Fjob, guardian as single factors.
# 2. Compute Gower distance (cluster::daisy) over the reconstructed variables.
# 3. Choose k via average silhouette width across candidate k (using PAM).
# 4. Cluster with PAM (k-medoids - each cluster center is an actual student,
#    which also makes profiles easier to describe/interpret).
# 5. Describe profiles and test association with alc_score exactly as before
#    (Kruskal-Wallis / ANOVA), for direct comparison with the k-means result.

library(cluster)
library(factoextra)
library(ggplot2)

# Turn a set of one-hot dummy columns back into a single factor.
# `reference_level` is the category that was dropped during dummy-coding
# (i.e. the student is that category if every dummy in `cols` is 0).
reconstruct_factor <- function(df, cols, reference_level) {
  levels_found <- sub(".*_", "", cols)  # e.g. "Mjob_health" -> "health"
  out <- rep(reference_level, nrow(df))
  for (i in seq_along(cols)) {
    out[df[[cols[i]]] == 1] <- levels_found[i]
  }
  factor(out)
}

profile_dataset_gower <- function(train_file, test_file, dataset_name, k_range = 2:6) {
  train <- read.csv(train_file, check.names = FALSE)
  test  <- read.csv(test_file, check.names = FALSE)
  full  <- rbind(train, test)
  
  # --- Reconstruct original variables from dummy columns ---
  full$Mjob <- reconstruct_factor(
    full, c("Mjob_health", "Mjob_other", "Mjob_services", "Mjob_teacher"),
    reference_level = "at_home"
  )
  full$Fjob <- reconstruct_factor(
    full, c("Fjob_health", "Fjob_other", "Fjob_services", "Fjob_teacher"),
    reference_level = "at_home"
  )
  full$guardian <- reconstruct_factor(
    full, c("guardian_mother", "guardian_other"),
    reference_level = "father"
  )
  
  # Remaining binary 0/1 variables -> factors (not left as raw numeric),
  # so Gower treats them as categorical, not as a 0-1 numeric range.
  binary_vars <- c("sex_M", "address_U", "famsize_LE3", "Pstatus_T",
                   "famsup_yes", "internet_yes", "activities_yes", "romantic_yes")
  for (v in binary_vars) full[[v]] <- factor(full[[v]])
  
  # Numeric/ordinal variables stay numeric; Gower range-normalizes them
  # internally, so no manual standardization is needed here.
  numeric_vars <- c("age", "Medu", "Fedu", "famrel", "freetime", "goout")
  
  cluster_vars <- c(numeric_vars, binary_vars, "Mjob", "Fjob", "guardian")
  cluster_data <- full[, cluster_vars]
  
  cat("\n==", dataset_name, ": ", nrow(full), " students total ==\n", sep = "")
  
  # --- Gower distance ---
  gower_dist <- daisy(cluster_data, metric = "gower")
  
  # --- Choose k via average silhouette width (PAM) ---
  sil_widths <- sapply(k_range, function(k) {
    pam(gower_dist, diss = TRUE, k = k)$silinfo$avg.width
  })
  best_k <- k_range[which.max(sil_widths)]
  cat("Silhouette width by k:\n")
  print(round(setNames(sil_widths, k_range), 3))
  cat("Chosen number of profiles: ", best_k, "\n", sep = "")
  
  # --- Cluster with PAM ---
  pam_fit <- pam(gower_dist, diss = TRUE, k = best_k)
  full$profile <- factor(pam_fit$clustering)
  
  cat("\nProfile sizes:\n")
  print(table(full$profile))
  
  # Medoids: the actual "most representative" student for each profile
  cat("\nMedoid (representative student) characteristics per profile:\n")
  print(cluster_data[pam_fit$medoids, ])
  
  # Describe profiles: means for numeric vars, proportions for categorical
  cat("\nNumeric variable means per profile:\n")
  print(aggregate(full[, numeric_vars], by = list(profile = full$profile), FUN = mean))
  
  cat("\nCategory breakdown per profile (Mjob, Fjob, guardian):\n")
  for (v in c("Mjob", "Fjob", "guardian")) {
    cat("\n", v, ":\n", sep = "")
    print(prop.table(table(full$profile, full[[v]]), margin = 1))
  }
  
  # --- Alcohol consumption by profile ---
  cat("\nAlcohol score by profile:\n")
  print(aggregate(alc_score ~ profile, data = full, FUN = function(v)
    c(mean = round(mean(v), 2), sd = round(sd(v), 2), n = length(v))))
  
  aov_fit <- aov(alc_score ~ profile, data = full)
  cat("\nANOVA (alc_score ~ profile):\n")
  print(summary(aov_fit))
  
  kw <- kruskal.test(alc_score ~ profile, data = full)
  cat("\nKruskal-Wallis test:\n")
  print(kw)
  
  if (kw$p.value < 0.05) {
    cat("\nSignificant overall difference - pairwise comparisons",
        "(Wilcoxon, Bonferroni-corrected):\n")
    print(pairwise.wilcox.test(full$alc_score, full$profile,
                               p.adjust.method = "bonferroni"))
  } else {
    cat("\nNo significant overall difference between profiles at alpha = 0.05.\n")
  }
  
  # --- Visualize ---
  # fviz_cluster can plot a PAM result directly from the dissimilarity matrix
  p1 <- fviz_cluster(pam_fit, data = gower_dist,
                     geom = "point", ellipse.type = "convex",
                     main = paste0(dataset_name, ": PAM profiles (Gower distance)"))
  print(p1)
  
  p2 <- ggplot(full, aes(x = profile, y = alc_score, fill = profile)) +
    geom_boxplot(outlier.alpha = 0.4) +
    labs(title = paste0(dataset_name, ": alcohol score by profile (Gower + PAM)"),
         x = "Profile", y = "Alcohol score (1-5)") +
    theme_minimal() +
    theme(legend.position = "none")
  print(p2)
  
  invisible(full)
}

math_profiles_gower <- profile_dataset_gower(
  "data/processed/Math_train.csv", "data/processed/Math_test.csv", "Math"
)
lang_profiles_gower <- profile_dataset_gower(
  "data/processed/Lang_train.csv", "data/processed/Lang_test.csv", "Portuguese"
)