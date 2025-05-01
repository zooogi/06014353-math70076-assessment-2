# Check required packages
required_packages <- c(
  "tidyverse",
  "psych",
  "effectsize",
  "broom",
  "parameters",
  "dplyr",
  "stringr",
  "here",
  "knitr",
  "kableExtra",
  "ggplot2",
  "forcats"
  
)

new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]

if (length(new_packages)) {
  message("The following packages are required but not installed:")
  message(paste(new_packages, collapse = ", "))
  message("Please install them manually using install.packages(c(...)).")
} else {
  message("All required packages are already installed. Good to go!")
}
