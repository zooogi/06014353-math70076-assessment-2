# 03_regression_analysis.R
# Purpose: Run 10 linear models and 25 linear models (Reproduce Table 2 and Table 3)

# ---Load packages---
library(tidyverse)
library(broom)          # for tidy(), glance()
library(parameters)     # for standardize_parameters()
library(effectsize)     # for eta_squared()
library(dplyr)
library(stringr)

# ---Load data---
df_clean <- readRDS("data/processed/cleaned_data.rds")
df_cesd <- readRDS("data/processed/derived_data.rds")

# =========================================================
# Part 1: Regressions for Table 2 (β, SE, p-values)
# =========================================================

# ---Define outcome variables for each dimension---
mental_vars <- c("cesd", "stai_t")  # Depressive symptoms, Anxiety
burnout_vars <- c("mbi_ex", "mbi_cy", "mbi_ea")  # Emotional exhaustion, Cynicism, Academic efficacy
empathy_vars <- c("jspe", "qcae_cog", "qcae_aff", "amsp", "erec_mean")  # Empathy group



# --- Unified function: Fit model once and extract tidy + eta ---
run_lm_full <- function(outcome_var, data = df_clean) {
  model <- lm(reformulate(c("year", "sex"), outcome_var), data = data)
  
  # tidy result
  tidy_result <- tidy(model) %>%
    filter(term != "(Intercept)") %>%
    select(term, estimate, std.error, p.value) %>%
    mutate(outcome = outcome_var)
  
  # eta squared
  eta_result <- effectsize::eta_squared(model, partial = TRUE) %>%
    select(Parameter, Eta2_partial) %>%
    rename(term = Parameter) %>%
    mutate(outcome = outcome_var)
  
  return(list(model = model, tidy = tidy_result, eta = eta_result))
}


# --- Helper: Run lm and return standardized beta ---
run_lm_standardized <- function(outcome_var, data = df_clean) {
  model <- lm(reformulate(c("year", "sex"), outcome_var), data = data)
  
  ##Original P and SE
  raw_stats <- tidy(model) %>%
    filter(term != "(Intercept)") %>%
    select(term, std.error, p.value)
  
  std_coef <- parameters::standardize_parameters(model, method = "basic") %>%
    as_tibble() %>%
    filter(Parameter != "(Intercept)") %>%
    select(term = Parameter, estimate_std = Std_Coefficient)
  
  #merge
  raw_stats %>%
    left_join(std_coef, by = "term") %>%
    mutate(outcome = outcome_var) %>%
    select(outcome, term, estimate_std, std.error, p.value)
}

# --- Run all models ---
results <- map(c(mental_vars, burnout_vars, empathy_vars), run_lm_full)
table2_tidy_raw <- map_dfr(results, "tidy")
table2_eta <- map_dfr(results, "eta")
table2_std <- map_dfr(c(mental_vars, burnout_vars, empathy_vars), run_lm_standardized)

# --- Join standardized beta and eta ---
# --- Format beta + significance stars ---


table2_final <- table2_std %>%
  #1) Mark predictor for later alignment with eta table
  mutate(
    predictor = case_when(
      str_starts(term, "year") ~ "year",
      str_starts(term, "sex")  ~ "sex",
      TRUE                     ~ NA_character_
    )
  ) %>%
  #2) Rename the term of the eta table to predictor, and then join
  left_join(
    table2_eta %>% rename(predictor = term),
    by = c("outcome", "predictor")
  ) %>%
  #3) Generate β and η, round them up, and add an asterisk
  mutate(
    β_val   = round(estimate_std, 2),
    p_stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE            ~ ""
    ),
    β   = paste0(β_val, p_stars),
    eta = round(Eta2_partial, 2),
    std.error = round(std.error, 2)
  ) %>%
  #4) Finally select outcome, term, β, and standard error eta
  select(outcome, term, β, std.error, eta)


# ---Export results---
write_csv(table2_final, "figures/table2_replication.csv")
write_csv(table2_tidy_raw, "figures/table2_lm_results.csv")
write_csv(table2_eta, "figures/table2_eta_summary.csv")
# ---Optional: Save model list if needed for later use---
# saveRDS(results, file = "figures/table2_models.rds")
message("Table 2 regression models computed and saved.")

# --- Sanity check ---
print(head(table2_final))

# ==========================================================================
# Additional Description: CES-D cut-off prevalence by curriculum year
# ==========================================================================

# ---CES-D cut-off prevalence
cesd_prevalence <- df_cesd %>%
  group_by(year) %>%
  summarise(
    n = n(),
    positive_n = sum(depressed),
    positive_pct = round(100*positive_n/n, 2)
  )

# ---Save---
write_csv(cesd_prevalence, "figures/cesd_prevalence_by_year.csv")
message(" CES-D cut-off prevalence saved.")

# ---quick check---
pct_vector <- cesd_prevalence$positive_pct
print(pct_vector)

# =========================================================
# Part 2: Regressions for Table 3 (β, SE, p-values)
# =========================================================


# --- Define function: Fit one model and extract required results ---
run_table3_model <- function(emp_var, outcome_var, data) {
  
  # 1) Formula: outcome ~ empathy
  fml <- reformulate(emp_var, outcome_var)
  m   <- lm(fml, data)
  
  # 2) Extract raw coefficient, standard error, and p-value
  td_raw <- tidy(m) %>% 
    filter(term == emp_var) %>% 
    select(estimate_raw = estimate, std.error, p.value)
  
  # 3) Extract standardized coefficient (basic method)
  td_std <- standardize_parameters(m, method = "basic") %>% 
    as_tibble() %>% 
    filter(Parameter == emp_var) %>% 
    select(estimate_std = Std_Coefficient)
  
  # 4) Extract partial eta squared
  td_eta <- eta_squared(m, partial = FALSE) %>% 
    as_tibble() %>% 
    filter(Parameter == emp_var) %>% 
    select(eta2 = Eta2)
  
  # 5) Extract F-statistic and R-squared
  gl <- glance(m)
  F_val  <- gl$statistic
  R2_val <- gl$r.squared
  
  # 6) Create significance stars
  stars <- case_when(
    td_raw$p.value < 0.001 ~ "***",
    td_raw$p.value < 0.01  ~ "**",
    td_raw$p.value < 0.05  ~ "*",
    TRUE                   ~ ""
  )
  
  tibble(
    empathy    = emp_var,
    outcome    = outcome_var,
    estimate   = round(td_std$estimate_std, 2),
    p_stars    = stars,
    std.error  = round(td_raw$std.error, 2),
    eta2       = round(td_eta$eta2, 2),
    F_stat     = round(F_val, 2),
    R2         = round(R2_val, 2)
  )
}

# --- Run all models across empathy × outcome combinations ---
table3_results <- map_df(empathy_vars, function(emp) {
  map_df(c(mental_vars, burnout_vars), function(outc) {
    run_table3_model(emp, outc, df_clean)
  })
})

# --- Print results for preview ---
print(table3_results)

# --- Save results to CSV ---
write_csv(table3_results, "figures/table3_replication.csv")
message("Table 3 regression models completed and saved.")



