# =========================================================
# 02_descriptive_stats.R
# Descriptive statistics for replication
# =========================================================

# --- Load required packages ---
library(tidyverse)
library(psych)

# --- Read data ---
df_raw <- read_csv("data/raw/Data Carrard et al. 2022 MedTeach.csv")
df_cesd<- readRDS("data/processed/derived_data.rds")

# =========================================================
# Part 1: Table 1 - Descriptive Statistics
# =========================================================

# --- Define variables ---
continuous_vars <- c("age", "stud_h", "health", 
                     "jspe", "qcae_cog", "qcae_aff", "amsp", "erec_mean", 
                     "cesd", "stai_t", "mbi_ex", "mbi_cy", "mbi_ea")

categorical_vars <- c("sex", "year", "glang", "part", "job", "psyt")

# --- Summarize continuous variables ---
continuous_summary <- df_raw %>%
  select(all_of(continuous_vars)) %>%
  psych::describe() %>%
  as_tibble(rownames = "Variable") %>%
  select(Variable, mean, sd) %>%
  mutate(
    `Mean (SD)` = paste0(round(mean, 2), " (", round(sd, 2), ")"),
    Percent = NA  # For continuous variables, no Percent
  ) %>%
  select(Variable, `Mean (SD)`, Percent)

# --- Summarize categorical variables ---
categorical_summary <- df_raw %>%
  select(all_of(categorical_vars)) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
  group_by(Variable, Value) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Variable) %>%
  mutate(
    percent = round(100 * n / sum(n), 2)
  ) %>%
  ungroup() %>%
  mutate(
    Variable = case_when(
      Variable == "sex" & Value == 1 ~ "Sex: Male",
      Variable == "sex" & Value == 2 ~ "Sex: Female",
      Variable == "sex" & Value == 3 ~ "Sex: Non-binary",
      Variable == "year" & Value == 1 ~ "Year: B1",
      Variable == "year" & Value == 2 ~ "Year: B2",
      Variable == "year" & Value == 3 ~ "Year: B3",
      Variable == "year" & Value == 4 ~ "Year: M1",
      Variable == "year" & Value == 5 ~ "Year: M2",
      Variable == "year" & Value == 6 ~ "Year: M3",
      Variable == "glang" & Value == 1 ~ "Mother tongue: French",
      Variable == "glang" & Value == 15 ~ "Mother tongue: German",
      Variable == "glang" & Value == 20 ~ "Mother tongue: English",
      Variable == "glang" & Value == 37 ~ "Mother tongue: Arab",
      Variable == "glang" & Value == 51 ~ "Mother tongue: Basque",
      Variable == "glang" & Value == 52 ~ "Mother tongue: Bulgarian",
      Variable == "part" & Value == 0 ~ "Has partner: No",
      Variable == "part" & Value == 1 ~ "Has partner: Yes",
      Variable == "job" & Value == 0 ~ "Has paid job: No",
      Variable == "job" & Value == 1 ~ "Has paid job: Yes",
      Variable == "psyt" & Value == 0 ~ "Consulted psy last year: No",
      Variable == "psyt" & Value == 1 ~ "Consulted psy last year: Yes",
      TRUE ~ paste(Variable, Value)
    )
  ) %>%
  select(Variable, percent) %>%
  mutate(
    `Mean (SD)` = NA,  # For categorical variables, no Mean (SD)
    Percent = paste0(percent, "%")
  ) %>%
  select(Variable, `Mean (SD)`, Percent)

# --- Combine and arrange ---
final_table1 <- bind_rows(
  continuous_summary,
  categorical_summary
) %>%
  arrange(Variable)

# --- Export Table 1 ---
write_csv(final_table1, "figures/table1_descriptive_stats.csv")
message("Table 1 descriptive statistics saved to 'figures/table1_descriptive_stats.csv'.")

# =========================================================
# Part 2: Flow Chart - Additional Sample Description
# =========================================================

# --- Gender chi-squared test ---
# H0:The gender distribution in the sample equals that in the population
gender_table <- tibble(
  group = c("Sample", "Population"),
  female = c(0.684, 0.6596),
  male = c(1 - 0.684, 1 - 0.6596)
)

N_assumed <- 1000  # Assume 1000 for expected count
chi_table <- matrix(
  c(
    gender_table$female[1] * N_assumed, gender_table$male[1] * N_assumed,
    gender_table$female[2] * N_assumed, gender_table$male[2] * N_assumed
  ),
  nrow = 2,
  byrow = TRUE
)

colnames(chi_table) <- c("Female", "Male")
rownames(chi_table) <- c("Sample", "Population")

# Run Chi² test
chi_result <- chisq.test(chi_table)
print(chi_result) # do not reject H0

# --- CES-D depression risk ---
depression_rate <- mean(df_cesd$depressed) * 100
cat("Percentage at risk of depression:", round(depression_rate, 3), "%\n")


