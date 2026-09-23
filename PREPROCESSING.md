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

- Math: 395 rows; 35 rows are missing `Dalc` or `Walc`, leaving 360.
- Portuguese: 649 rows; 61 rows are missing `Dalc` or `Walc`, leaving 588.
- There are no duplicate rows and no values outside the expected ranges.
- The remaining variables have missing values. We will handle these with MICE in the next stage.

## Stage 3: Impute missing model inputs

- We changed categorical inputs into factors and ran MICE with 5 versions and 20 iterations, following the course example.
- We compared the imputed values with the observed values. We selected the version with the smallest average difference; this was version 2 for both datasets.
- The selected Math and Portuguese datasets have no missing values in `Dalc`, `Walc`, or the included model inputs.

## Stage 4: Create the alcohol score and numeric model inputs

- We calculate `alc_score = (Dalc + Walc) / 2`, giving the two survey measures equal weight. This is an average of the two ratings, not an estimate of total drinks in a week.
- We keep `Dalc` and `Walc` to inspect workday and weekend drinking separately.
- When predicting `alc_score`, exclude `Dalc` and `Walc` from the inputs. When clustering, exclude all three alcohol columns.
- We changed each categorical input into 0/1 dummy variables.
- We removed one category per variable as a baseline, to avoid the dummy-variable trap.
- Each dataset has 24 numeric model inputs, the two original alcohol measures, and `alc_score`.
- Scaling will happen in the analysis scripts. For regression, use training data to choose the imputation and scaling steps before evaluating on test data. For clustering, scale the chosen inputs before k-means.
- These saved files are ready for exploration. The regression evaluation still needs its own train/test preparation.

## Outlier check

- The raw data contain one 22-year-old in each dataset. Age 22 is within the stated 15 to 22 range, so these rows are kept.
- All selected fields are within their stated ranges. Unusual but valid values are kept for exploration.
