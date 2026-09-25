# Preprocessing

## Stage 1: Choose fields

We analyse the Math and Portuguese datasets separately. We use the average of `Dalc` and `Walc` as the main alcohol outcome. These fields are not model inputs.

### Included model inputs

- **Demographic/home:** `sex`, `age`, `address`
- **Family/home context:** `famsize`, `Pstatus`, `Medu`, `Fedu`, `Mjob`, `Fjob`, `guardian`, `famsup`, `internet`, `famrel`
- **Social/lifestyle:** `activities`, `romantic`, `freetime`, `goout`

### Excluded from model inputs

`school`, `reason`, `traveltime`, `studytime`, `failures`, `schoolsup`, `paid`, `nursery`, `higher`, `health`, `absences`, `G1`, `G2`, `G3`

These are school, academic, historical-education, or health variables. They are outside our chosen focus: demographic/home, family/home, and social/lifestyle characteristics.

## Stage 2: Check the raw data

- Math: 395 rows; 35 rows are missing `Dalc` or `Walc`, leaving 360. Five students aged 20 or older are then removed, leaving 355.
- Portuguese: 649 rows; 61 rows are missing `Dalc` or `Walc`, leaving 588. Nine students aged 20 or older are then removed, leaving 579.
- There are no duplicate rows and no values outside the expected ranges.
- The remaining variables have missing values. We will handle these with MICE in the next stage.

## Stage 3: Impute missing inputs for exploration

- We changed categorical inputs into factors and ran MICE with 5 versions and 20 iterations, following the course example.
- Following the lecture workflow, we compared the imputed values with the observed values and selected one best-matching version for each dataset.
- After removing students aged 20 or older, version 1 had the smallest average difference for Math and version 4 had the smallest average difference for Portuguese.
- The selected Math and Portuguese datasets have no missing values in `Dalc`, `Walc`, or the included model inputs.

## Stage 4: Create the alcohol score and numeric model inputs

- We calculate `alc_score = (Dalc + Walc) / 2`, giving the two survey measures equal weight. This is an average of the two ratings, not an estimate of total drinks in a week.
- We keep `Dalc` and `Walc` to inspect workday and weekend drinking separately.
- When predicting `alc_score`, exclude `Dalc` and `Walc` from the inputs. When clustering, exclude all three alcohol columns.
- We changed each categorical input into 0/1 dummy variables.
- We removed one category per variable as a baseline, to avoid the dummy-variable trap.
- Each dataset has 24 numeric model inputs, the two original alcohol measures, and `alc_score`.
- Scaling happens in the analysis scripts. For clustering, scale the chosen inputs before k-means.
- The full `Math.csv` and `Lang.csv` files are for exploration.

## Stage 5: Prepare data for prediction

- After the same student exclusions, each dataset is split into 70% training and 30% test students, using seed 123. Math has 248 training and 107 test students; Portuguese has 405 training and 174 test students.
- MICE learns how to fill missing inputs from the training students. It also fills missing test inputs, but test students do not help fit MICE. `Dalc`, `Walc`, and `alc_score` are not used to fill inputs.
- Five MICE versions are compared using training students only. This selects version 3 for Math and version 5 for Portuguese. These differ from the full exploration files because the training data are smaller and alcohol is excluded from model imputation.
- Categorical inputs become dummy variables, and `alc_score` is calculated from `Dalc` and `Walc`.
- The script saves `Math_train.csv`, `Math_test.csv`, `Lang_train.csv`, and `Lang_test.csv` in `data/processed/`. The test files include the real alcohol scores so predictions can be checked. These scores are not used to prepare inputs or fit models. The RQ1 script reads these files and standardizes inputs when fitting ridge and LASSO.

## Outlier check

- The raw data contain students aged 20 to 22. These ages are unusual for secondary school, and we cannot confirm whether they are correct.
- We remove five Math records and nine Portuguese records aged 20 or older before imputation. No other selected value was flagged as outside its recorded scale.
