# ---- Load Required Packages ----
library(tidyverse)

# ---- Import Raw Data ----
raw_data <- read_csv("data/raw/Data Carrard et al. 2022 MedTeach.csv")

# ---- Step 1: Exclude non-binary gender participants ----
clean_data <- raw_data %>%
  filter(sex != 3)

# ---- Step 2: Recode variables for regression (set as factor) ----
# - Curriculum year (year)
# - Gender (sex)
clean_data <- clean_data %>%
  mutate(
    year = factor(year, levels = c(1, 2, 3, 4, 5, 6),
                  labels = c("B1", "B2", "B3", "M1", "M2", "M3")),
    sex = factor(sex, levels = c(1, 2), labels = c("Male", "Female"))
  )

# ---- Step 3: Check missing data (forced answer, so normally none) ----
missing_summary <- clean_data %>% summarise_all(~ sum(is.na(.)))
print(missing_summary)
# (Just check, no NA expected)


# ---- Step 4: Save Processed Clean Data ----
saveRDS(clean_data, file = "data/processed/cleaned_data.rds")
message("Data cleaning completed and saved to data/processed/clean_data.rds")


