## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 8,
  fig.height = 6,
  dpi = 96
)

## ----setup--------------------------------------------------------------------
library(nightowl)
library(survival)
library(dplyr)
library(ggplot2)

# Create comprehensive clinical survival dataset
set.seed(123)  # For reproducible examples

# Simulate clinical trial survival data
clinical_survival <- tibble(
  patient_id = 1:300,
  time = rexp(300, rate = 0.02),  # Exponential survival times
  event = rbinom(300, 1, 0.7),   # 70% event rate
  treatment = sample(c("Control", "Treatment A", "Treatment B"), 300, replace = TRUE),
  age = round(rnorm(300, 65, 12)),
  sex = sample(c("Male", "Female"), 300, replace = TRUE),
  stage = sample(c("I", "II", "III", "IV"), 300, replace = TRUE, prob = c(0.2, 0.3, 0.3, 0.2)),
  biomarker_high = sample(c("High", "Low"), 300, replace = TRUE),
  ecog_status = sample(0:2, 300, replace = TRUE, prob = c(0.4, 0.4, 0.2)),
  site = sample(c("Site A", "Site B", "Site C", "Site D"), 300, replace = TRUE),
  risk_group = sample(c("Low", "Intermediate", "High"), 300, replace = TRUE)
) %>%
  # Adjust survival times based on treatment effect
  mutate(
    time = ifelse(treatment == "Treatment A", time * 1.5, time),  # Treatment benefit
    time = ifelse(treatment == "Treatment B", time * 1.3, time),  # Moderate benefit
    time = pmax(time, 0.1)  # Ensure positive times
  )

# Additional simulated data for examples
simple_survival <- tibble(
  time = sample(1:100, size = 100, replace = TRUE),
  event = sample(c(0, 1), size = 100, replace = TRUE),
  treatment = sample(c("Control", "Treatment"), size = 100, replace = TRUE),
  covariate1 = sample(c("No", "Yes"), size = 100, replace = TRUE),
  covariate2 = sample(c("No", "Yes"), size = 100, replace = TRUE),
  stratum1 = sample(c("A", "B"), size = 100, replace = TRUE),
  continuous_var = rnorm(100),
  site = sample(LETTERS[1:5], size = 100, replace = TRUE)
)

