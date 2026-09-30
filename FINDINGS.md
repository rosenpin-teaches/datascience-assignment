# Alcohol analysis findings for the report

Analysis run: 30 September 2026.

RQ1: Going out and recorded sex are the strongest retained predictors in both datasets. LASSO predicts better than a simple constant score, but individual predictions remain uncertain.

RQ2: Standardized k-means finds groups mainly separated by mother's job, with no significant alcohol-score differences. Gower + PAM finds some alcohol differences, but the groups overlap substantially and differ from the k-means solution. The results do not establish clearly separated, reliable alcohol-related student types.

## Data and preparation

| Dataset | Students analysed | Training | Test | Alcohol mean (SD) | Alcohol median (IQR) |
|---|---:|---:|---:|---:|---:|
| Math | 355 | 248 | 107 | 1.897 (0.974) | 1.5 (1.5) |
| Portuguese | 579 | 405 | 174 | 1.891 (0.987) | 1.5 (1.5) |

Students missing either alcohol measure were excluded: 35 Math and 61 Portuguese records. The group also excluded students with a recorded age of 20 or older: 5 Math and 9 Portuguese records. These ages were unusual for the chosen population, but were not proven data errors.

The outcome is the equally weighted average of the workday and weekend ratings: `alc_score = (Dalc + Walc) / 2`. It ranges from 1 to 5 and describes survey ratings, not the number of drinks consumed in a week.

The 17 selected demographic, family, and social variables become 24 numerical or dummy columns for regression. Missing inputs were filled with MICE, using five versions, 20 iterations, and seed 123. Following the lecture workflow, a version was selected by comparing observed and imputed training values: version 3 for Math and version 5 for Portuguese. Test students and alcohol scores did not help fit the imputation models.

See [PREPROCESSING.md](PREPROCESSING.md) for the fields, exclusions, and full workflow. Math and Portuguese are analysed separately. Some students appear in both, so agreement between datasets is complementary evidence rather than independent replication.

## RQ1 Prediction accuracy

Five-fold cross-validation within the training group compared ordinary regression, ridge, and LASSO. Ridge and LASSO standardized inputs during fitting and selected lambda using MAE. The final model was selected using training cross-validation, before comparing test errors. LASSO had the lowest cross-validation MAE in both datasets.

| Model | Math CV MAE | Math test MAE | Portuguese CV MAE | Portuguese test MAE |
|---|---:|---:|---:|---:|
| Training mean | 0.794 | 0.789 | 0.794 | 0.793 |
| Training median | 0.756 | 0.771 | 0.756 | 0.756 |
| Ordinary regression | 0.686 | 0.670 | 0.707 | 0.704 |
| Ridge | 0.683 | 0.666 | 0.703 | 0.700 |
| LASSO | 0.676 | 0.661 | 0.700 | 0.683 |

| Selected LASSO model | Lambda | Test RMSE | Test R² | Retained input columns |
|---|---:|---:|---:|---:|
| Math | 0.02016 | 0.821 | 0.275 | 17 of 24 |
| Portuguese | 0.03329 | 0.854 | 0.238 | 14 of 24 |

MAE is the average absolute error in alcohol-score points. An MAE of 0.661 means the Math predictions miss by about 0.66 points on average. Compared with predicting the training median for everyone, LASSO reduces test MAE by 14.3% in Math and 9.6% in Portuguese. Its advantage over ridge is small, especially in Math.

R² indicates that predictions account for about 27.5% and 23.8% of the variation in the two test groups. This is modest prediction accuracy.

Higher scores are particularly hard to predict. For test students with observed scores of 3 or higher, MAE is 1.097 in Math (20 students) and 1.244 in Portuguese (31 students), with predictions generally too low. The score of 3 is a descriptive comparison point, not a clinical risk threshold.

Sources: [model metrics](outputs/rq1/model_metrics.csv), [test predictions](outputs/rq1/test_predictions.csv), and [errors by score](outputs/rq1/error_by_score.csv).

## RQ1 Student characteristics

Going out and male sex have the two largest absolute standardized LASSO coefficients in both datasets. Better reported family relationships are associated with a lower predicted alcohol score in both. The remaining retained associations are smaller or less consistent across datasets.

| Input | Math coefficient | Portuguese coefficient | Meaning of one unit |
|---|---:|---:|---|
| Going out | +0.276 | +0.248 | One higher category on the 1 to 5 going-out rating |
| Male | +0.419 | +0.426 | Male compared with female |
| Family relationships | -0.091 | -0.115 | One higher category on the 1 to 5 family-relationship rating |

These are raw coefficients in alcohol-score points, holding the other model inputs constant. They are associations in a penalized prediction model, not causal effects or significance tests.

The figures use coefficients multiplied by each training input's SD so inputs with different scales can be compared. `glmnet` itself returns coefficients on the original input scales. [Package documentation](https://search.r-project.org/CRAN/refmans/glmnet/html/glmnet.html).

The reference categories for dummy coefficients are female, rural address, family size greater than 3, parents apart, parents working at home, father as guardian, and no for yes/no inputs. A zero coefficient means LASSO omitted that input in this fit; it does not prove that the characteristic is irrelevant.

Source: [all model coefficients](outputs/rq1/model_coefficients.csv).

## RQ2 Student profiles

The clustering uses all analysed students by combining their prepared training and test inputs. Alcohol is excluded from both imputation of these inputs and clustering. Each method compares 2 to 6 groups and chooses the largest average silhouette. Alcohol differences are inspected only afterward.

K-means is the main analysis from the course plan. All inputs, including dummy columns, are converted to Z-scores, following Lecture 6's instruction to standardize clustering inputs. For parental jobs and guardian, the dropped reference category is restored because clustering does not require a regression baseline. K-means uses 25 starts and seed 123; elbow plots are also supplied.

The lectures do not specify how to weight dummy columns for mixed data. Standardizing them gives rare categories larger distances, and variables with several dummy columns can receive more weight. This is a limitation of applying the lecture method to these inputs, not evidence that parental jobs are the most important alcohol predictors.

Eliska's Gower + PAM method is retained as a comparison for mixed inputs. It reconstructs each nominal variable as one factor. Gower compares categorical matches and rescales numerical differences by their ranges; PAM groups students around representative records. [Gower documentation](https://stat.ethz.ch/R-manual/R-devel/library/cluster/html/daisy.html).

| Dataset | Method | Groups | Average silhouette | Kruskal-Wallis H | df | p |
|---|---|---:|---:|---:|---:|---:|
| Math | K-means | 6 | 0.127 | 1.857 | 5 | 0.869 |
| Portuguese | K-means | 5 | 0.122 | 3.129 | 4 | 0.537 |
| Math | Gower + PAM | 4 | 0.105 | 10.647 | 3 | 0.0138 |
| Portuguese | Gower + PAM | 2 | 0.108 | 28.349 | 1 | 1.01 × 10⁻⁷ |

Silhouette scores close to zero indicate substantial overlap between the groups. All four solutions have weak separation. Math k-means selects the largest number tested, so six is the best within 2 to 6, not an established optimum. The Math PAM choice is also uncertain: four groups only narrowly outperform two in average silhouette. The distance methods weight differences differently, so their silhouettes are not a direct contest between interchangeable models. [Silhouette documentation](https://stat.ethz.ch/R-manual/R-devel/library/cluster/html/silhouette.html).

### K means results

The groups largely reproduce mother's job categories. In Math, profiles 1 to 5 correspond almost entirely to health, services, other, teacher, and at-home jobs; profile 6 contains students with an 'other' guardian. The five Portuguese groups correspond to those same five mother's job categories. Profile numbers are arbitrary labels within each dataset. These are not clear new alcohol-related social profiles.

| Dataset | Profile | Students | Mean alcohol | Median alcohol |
|---|---:|---:|---:|---:|
| Math | 1 | 29 | 1.793 | 1.5 |
| Math | 2 | 78 | 1.917 | 1.5 |
| Math | 3 | 120 | 1.950 | 1.5 |
| Math | 4 | 55 | 1.955 | 1.5 |
| Math | 5 | 49 | 1.867 | 1.5 |
| Math | 6 | 24 | 1.625 | 1.5 |
| Portuguese | 1 | 42 | 1.810 | 1.5 |
| Portuguese | 2 | 125 | 2.024 | 2.0 |
| Portuguese | 3 | 229 | 1.847 | 1.5 |
| Portuguese | 4 | 64 | 1.984 | 1.75 |
| Portuguese | 5 | 119 | 1.815 | 1.5 |

There is no significant evidence of alcohol-score differences between these k-means groups. This does not establish that parental jobs have no association with drinking; it describes the particular groups found here.

### Gower and PAM comparison

| Dataset | Profile | Students | Mean alcohol | Median alcohol |
|---|---:|---:|---:|---:|
| Math | 1 | 101 | 1.688 | 1.5 |
| Math | 2 | 75 | 2.167 | 2.0 |
| Math | 3 | 80 | 1.825 | 1.5 |
| Math | 4 | 99 | 1.965 | 1.5 |
| Portuguese | 1 | 356 | 1.701 | 1.5 |
| Portuguese | 2 | 223 | 2.195 | 2.0 |

After significant Kruskal-Wallis tests, pairwise Wilcoxon comparisons used Bonferroni correction and approximate p-values because alcohol scores contain many ties. In Math, only profiles 1 and 2 differ after correction (adjusted p = 0.00646). Profile 1 is 4% male with 78% reporting family support; profile 2 is 80% male with 8% reporting family support.

In Portuguese, profile 2 has a higher alcohol score than profile 1 (adjusted p = 1.01 × 10⁻⁷). Profile 2 is 76% male versus 21% in profile 1, has more activity participation (76% versus 30%), and less reported family support (39% versus 76%).

These differences are exploratory and depend on the clustering method. They should not be presented as proof of stable student types or future risk. The group comparisons also do not isolate the effects of sex, activities, or family support.

Sources: [cluster choices](outputs/rq2/cluster_choices.csv), [profile tests](outputs/rq2/profile_tests.csv), [alcohol summaries](outputs/rq2/profile_alcohol.csv), [pairwise tests](outputs/rq2/profile_pairwise.csv), [numeric profiles](outputs/rq2/profile_numeric.csv), and [category profiles](outputs/rq2/profile_categories.csv).

## Limits and practical interpretation

- The study is cross-sectional. Predictors describe current associations and cannot establish causes or future drinking risk.
- Prediction was checked on one random 70/30 split. Small differences between regression methods should not be overstated.
- MICE filled training inputs before the cross-validation folds were formed. The final test students remain excluded from fitting; tuning scores do not represent a completely separate preprocessing pipeline within each fold.
- One imputed version was selected following the lecture workflow. Results do not pool uncertainty across imputations, as the grader suggested. Missingness assumptions cannot be verified from these data.
- Alcohol and several inputs are ordinal ratings. Regression and numerical distances assume meaningful equal steps between categories.
- Some students occur in both subject datasets, and age exclusions restrict the population represented.
- Generalization beyond these two Portuguese schools has not been tested.

These findings can inform discussion of social context and further research. Prediction errors and weak profile boundaries are too substantial to justify assigning individual students to drinking-risk categories. The data do not show whether any proposed prevention or support programme would reduce alcohol consumption.

## Figures for the report

For a short report, start with these three figures:

1. [Regression accuracy](outputs/rq1/model_accuracy.png). Caption: Test MAE for constant predictions, ordinary regression, ridge, and LASSO. Models were selected using five-fold training cross-validation. Lower MAE is better.
2. [Predictor comparison](outputs/rq1/predictor_comparison.png). Caption: The eight inputs with the largest absolute standardized LASSO coefficients in either dataset. Positive coefficients correspond to higher predicted alcohol ratings, conditional on the other inputs.
3. [K-means alcohol comparison](outputs/rq2/kmeans_alcohol_comparison.png). Caption: Alcohol ratings across six Math and five Portuguese k-means profiles. Diamonds indicate means. Kruskal-Wallis tests did not find significant group differences.

The [prediction check](outputs/rq1/prediction_check.png) shows the underprediction of higher alcohol scores. Separate coefficient charts and profile characteristics, silhouettes, elbow plots, and two-dimensional maps are also saved in the two output folders. PAM alcohol plots are [Math](outputs/rq2/math_pam_alcohol.png) and [Portuguese](outputs/rq2/portuguese_pam_alcohol.png). These should be clearly labelled as the alternative analysis if used.

## Code changes and reproduction

Eliska's factor reconstruction, Gower distance, PAM fitting, silhouette selection, and corrected pairwise tests were retained. The submitted file is preserved in [reference/eliska_RQ2_submitted.R](reference/eliska_RQ2_submitted.R). The updated script adds the course k-means comparison and saves report outputs. It replaces the unavailable `factoextra` plotting dependency with `ggplot2` plots and uses Kruskal-Wallis as the primary group test rather than reporting a second ANOVA p-value.

Course alignment: Lecture 5 covers ordinary regression, ridge, LASSO, MAE/MSE, and choosing penalties with cross-validation. Lecture 6 covers five- or ten-fold cross-validation, k-means, standardized inputs, repeated starts, silhouette/elbow checks, and PCA. Kruskal-Wallis and post-hoc comparisons follow the proposal and grader feedback. Gower + PAM and its two-dimensional distance map are extra methods retained from Eliska's work, not methods established in these lectures. RMSE, R-squared, baseline comparisons, coefficient rescaling, and the higher-score error check support the agreed analyses; they are not new model types.

In RStudio, open the project and Source `scripts/preprocessing.R`, then `scripts/research_question_1.R` and `scripts/research_question_2.R`. The fitted models remain available as `math_regression$models`, `portuguese_regression$models`, `math_profiles`, and `portuguese_profiles`. Outputs can be regenerated from these scripts.

The report needs the assignment's AI-use declaration, describing the actual code review, debugging, plotting, and writing assistance used by the group.
