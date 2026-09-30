# Table images for FINDINGS.md, using the saved RQ1 and RQ2 results.
# Run both modelling scripts first. No models are fitted here.

output_dir <- file.path("outputs", "report")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Draw a table with base R graphics.
save_table <- function(values, heading, filename) {
  png(filename, width = 1500, height = 100 + 75 * (nrow(values) + 1), res = 150)
  par(mar = c(0.5, 0.5, 2.5, 0.5))
  plot(NA, xlim = c(0, ncol(values)), ylim = c(0, nrow(values) + 1),
       type = "n", axes = FALSE, xlab = "", ylab = "", xaxs = "i", yaxs = "i")
  title(main = heading, line = 0.7)
  rect(0, nrow(values), ncol(values), nrow(values) + 1, col = "#E8EDF3", border = NA)
  text(seq_len(ncol(values)) - 0.5, nrow(values) + 0.5, names(values), font = 2)
  for (row in seq_len(nrow(values))) {
    y <- nrow(values) - row
    if (row %% 2 == 0) rect(0, y, ncol(values), y + 1, col = "grey96", border = NA)
    text(seq_len(ncol(values)) - 0.5, y + 0.5, unlist(values[row, ], use.names = FALSE))
  }
  dev.off()
}

metrics <- read.csv(file.path("outputs", "rq1", "model_metrics.csv"))
math <- metrics[metrics$dataset == "Math", ]
portuguese <- metrics[metrics$dataset == "Portuguese", ]
portuguese <- portuguese[match(math$model, portuguese$model), ]
regression_table <- data.frame(
  Model = math$model,
  "Math\nCV MAE" = sprintf("%.3f", math$CV_MAE),
  "Math\nTest MAE" = sprintf("%.3f", math$MAE),
  "Portuguese\nCV MAE" = sprintf("%.3f", portuguese$CV_MAE),
  "Portuguese\nTest MAE" = sprintf("%.3f", portuguese$MAE),
  check.names = FALSE
)
save_table(regression_table, "Prediction error: lower MAE is better",
           file.path(output_dir, "regression_results.png"))

tests <- read.csv(file.path("outputs", "rq2", "profile_tests.csv"))
tests <- tests[order(tests$method != "K-means", tests$dataset), ]
clustering_table <- data.frame(
  Dataset = tests$dataset,
  Method = tests$method,
  Groups = as.character(tests$k),
  Silhouette = sprintf("%.3f", tests$silhouette),
  "Kruskal-Wallis\np-value" = format.pval(tests$p, digits = 3, eps = 1e-8),
  check.names = FALSE
)
save_table(clustering_table, "Student profiles: k-means main, PAM comparison",
           file.path(output_dir, "clustering_results.png"))
