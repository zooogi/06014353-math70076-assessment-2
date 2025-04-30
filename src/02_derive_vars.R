# 02_derive_vars.R

# --- Load required packages ---
library(tidyverse)

#  --- Read cleaned data ---
df_clean <- readRDS("data/processed/cleaned_data.rds")

#  ---Generate all derivative variables required for subsequent analysis---
df_derived <- df_clean %>%
  # Gender-specific CES-D cut-off
  mutate(
    depressed = case_when(
      sex == "Male"   & cesd >= 16 ~ 1,
      sex == "Female" & cesd >= 21 ~ 1,
      TRUE                        ~ 0
    ))

# ---Save Processed Derived Data---
saveRDS(df_derived, "data/processed/derived_data.rds")
message(" Derived Data completed and saved to data/processed/derived_data.rds")
