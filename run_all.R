# Master script to run full replication pipeline for Assessment 2

# Load required packages
source("requirements.R")

# Step 1: Data cleaning
source("src/01_data_cleaning.R")

# Step 2: Create derived variables (e.g. binary CES-D outcome)
source("src/02_derive_vars.R")

# Step 3: Descriptive statistics (Table 1)
source("src/03_descriptive_stats.R")

# Step 4: Regression analyses (Table 2 and Table 3)
source("src/04_regression_analysis.R")

# Done
message("All steps completed. Output tables saved to /figures")
