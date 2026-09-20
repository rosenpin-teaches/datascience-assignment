# Preprocessing

## Stage 1 — Choose fields

We analyse the Math and Portuguese datasets separately. The outcome is overall alcohol score:

`alc_score = (Dalc + Walc) / 2`

`Dalc` and `Walc` are used only to create this outcome. They are not model inputs.

### Included model inputs

- **Demographic/home:** `sex`, `age`, `address`
- **Family/home context:** `famsize`, `Pstatus`, `Medu`, `Fedu`, `Mjob`, `Fjob`, `guardian`, `famsup`, `internet`, `famrel`
- **Social/lifestyle:** `activities`, `romantic`, `freetime`, `goout`

### Excluded from model inputs

`school`, `reason`, `traveltime`, `studytime`, `failures`, `schoolsup`, `paid`, `nursery`, `higher`, `health`, `absences`, `G1`, `G2`, `G3`

These are school, academic, historical-education, or health variables. They are outside our chosen focus: demographic/home, family/home, and social/lifestyle characteristics.

## Stage 2 — Check the raw data

- Math: 395 rows; 35 rows are missing `Dalc` or `Walc`, leaving 360.
- Portuguese: 649 rows; 61 rows are missing `Dalc` or `Walc`, leaving 588.
- There are no duplicate rows and no values outside the expected ranges.
- The remaining variables have missing values. We will handle these with MICE in the next stage.
