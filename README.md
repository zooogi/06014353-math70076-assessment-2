# 06014353-math70076-assessment-2
This project replicates the key regression analyses from [Bemey et al. (2021)], which investigates how medical students' curriculum year and gender influence empathy, burnout, and mental health. The replication is based on open-access survey data and independently conducted using R.

## Project Structure
```bash
06014353-math70076-assessment-2/
│
├── README.md           
├── .gitignore          # Files/folders to ignore (e.g., .Rproj.user/)
├── renv.lock           # Lock file for R package versions (to ensure reproducibility)
├── run_all.R           # Master script to run the full workflow
├── requirements.R      # Required R Packages
│
├── data/
│   ├── raw/            # Raw data files 
│   ├── processed/      # Cleaned/processed data ready for analysis
│
├── src/
│   ├── 01_data_cleaning.R        # Script for data cleaning
│   ├── 02_derive_vars.R          # Script for derivative variables
│   ├── 03_descriptive_stats.R    # Script for descriptive statistics (Table 1 results)
│   ├── 04_regression_analysis.R  # Script for regression analyses (Table 2 and Table 3)
│
├── figures/            # output table and figures
│
├── report/
│   ├── final_report.pdf       # Final replication report
│   ├── final_report.Rmd       
│   ├── references.bib         # References
│
```
## Dataset Information
This replication uses the publicly available dataset from the first wave of the ETMED-L project, which surveys 886 medical students at the University of Lausanne Medical School (Switzerland).

> Source: [Zenodo Repository](https://zenodo.org/records/5702895#.Y8OraNJBwUE)



## Required R Packages
This project requires several R packages.
You can check whether they are installed by running:
```{r}
source("requirements.R")
```
If any packages are missing, please install them manually

## Environment Information
The project includes a `renv.lock` file for recording R package versions.
Although `renv::restore()` is not required to run this project, using consistent package versions is recommended for reproducibility.


## Quick Start
To fully reproduce the results:

1. Open the project folder in RStudio.
2. Make sure required packages are installed by running `requirements.R`.
3. Run `run_all.R` to execute the full workflow and reproduce all outputs (figures, tables, report).

No manual setup is needed — everything will be generated automatically.

## Research Result

This project successfully replicated the main findings of the original study on the relationship between curriculum year, gender, empathy, mental health, and burnout.

- All standardized regression coefficients ($\beta$) were successfully reproduced using R, with results fully matching the original Table 2 and Table 3.
- Key findings were visualized using bar plots and heatmaps for intuitive interpretation.
- CES-D risk prevalence by year was calculated and found to closely match the original report (40.98% vs. 40.18%).

All results and interpretations are documented in the `report/final_report.pdf`, with full regression outputs included in the appendix section or figures file.
