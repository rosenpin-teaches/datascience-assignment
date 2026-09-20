# Data Science Assignment 2

This repository contains the group’s data, code, and analysis outputs.

## Start in RStudio

1. Download or clone this repository from GitHub.
2. Open `datascience-assignment.Rproj` in RStudio.
3. Open `scripts/preprocessing.R`.
4. Click **Source** at the top-right of the script editor.

The script checks the raw data, runs MICE, and creates model-ready data for both datasets.

## First time only

In RStudio, install these packages through **Tools → Install Packages**:

`dplyr`, `mice`, `fastDummies`

## Where things are

- `data/raw/`: original course datasets. Do not edit these files.
- `scripts/preprocessing.R`: the one script to run for preprocessing.
- `data/processed/`: generated local datasets. These are recreated whenever the script runs.
- `outputs/preprocessing/`: small tables that show the preprocessing checks and MICE choice.
- `PREPROCESSING.md`: short explanation of the decisions we made.
- `reference/`: the untouched original group code.

You do not need to know Git to run the script. If you change the code, save the file and tell the group what you changed.
