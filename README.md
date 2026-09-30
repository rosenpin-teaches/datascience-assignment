# Data Science Assignment 2

This repository contains the group’s data, code, and analysis outputs.

## Start in RStudio

1. Download or clone this repository from GitHub.
2. Open `datascience-assignment.Rproj` in RStudio.
3. Open `scripts/preprocessing.R`.
4. Click **Source** at the top-right of the script editor.

The script checks the raw data and saves full processed files for exploration, plus separate training and test files for prediction.

After preprocessing, open `scripts/exploration.R` and click **Source** to run the exploratory analysis and create the figures.

To answer the first research question, open `scripts/research_question_1.R` and click **Source**. Model comparisons and selected inputs appear in the Console, and tables are saved in `outputs/rq1/`. Run `scripts/rq1_plots.R` afterward for figures and the additional error check.

RQ1 compares ordinary regression, ridge, and LASSO with simple mean and median predictions. Ridge and LASSO choose their penalty using the training data. The main result is test MAE: the average prediction error on the 1 to 5 alcohol scale. RMSE shows larger errors more strongly, and R² shows how much of the variation in test scores the model explains. LASSO also lists the inputs it kept; these are associations, not proof of cause.

For the second question, run `scripts/research_question_2.R`, then `scripts/rq2_plots.R` for figures. K-means is the main course analysis; Gower + PAM is an additional comparison. Alcohol is excluded from clustering. Results are saved in `outputs/rq2/`.

Run `scripts/report_tables.R` to create table images. [FINDINGS.md](FINDINGS.md) gives a short summary with figures for the report.

## First time only

In RStudio, install these packages through **Tools → Install Packages**:

`dplyr`, `mice`, `fastDummies`, `ggplot2`, `glmnet`, `cluster`
