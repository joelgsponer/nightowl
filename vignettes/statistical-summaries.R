## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  dpi = 96
)

## ----setup--------------------------------------------------------------------
library(nightowl)
library(dplyr)
library(ggplot2)

# Load Palmer Penguins for consistent examples
if (requireNamespace("palmerpenguins", quietly = TRUE)) {
  data(penguins, package = "palmerpenguins")
} else {
  # Create synthetic data if palmerpenguins not available
  penguins <- data.frame(
    species = sample(c("Adelie", "Chinstrap", "Gentoo"), 300, replace = TRUE),
    island = sample(c("Biscoe", "Dream", "Torgersen"), 300, replace = TRUE),
    bill_length_mm = rnorm(300, 44, 6),
    bill_depth_mm = rnorm(300, 17, 2),
    flipper_length_mm = rnorm(300, 200, 15),
    body_mass_g = rnorm(300, 4200, 800),
    sex = sample(c("male", "female"), 300, replace = TRUE)
  )
}

# Create additional clinical-style data
clinical_data <- data.frame(
  patient_id = 1:200,
  treatment_arm = sample(c("Treatment A", "Treatment B", "Control"), 200, replace = TRUE),
  age = round(rnorm(200, 65, 12)),
  response_score = round(rnorm(200, 75, 20)),
  adverse_events = sample(c("None", "Mild", "Moderate", "Severe"), 200, replace = TRUE, prob = c(0.4, 0.3, 0.2, 0.1)),
  biomarker_level = rlnorm(200, meanlog = 3, sdlog = 0.5)
)

