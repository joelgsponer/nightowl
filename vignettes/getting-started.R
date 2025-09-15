## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  dpi = 96
)

## ----eval=FALSE---------------------------------------------------------------
# # Install from GitHub
# devtools::install_github("joelgsponer/nightowl")

## ----setup--------------------------------------------------------------------
library(nightowl)
library(dplyr)
library(ggplot2)

# Load example data
data(ChickWeight)
testdata <- ChickWeight %>%
  filter(Time < 10)

## ----basic-plot---------------------------------------------------------------
# Create a simple ggplot2 plot
p <- ggplot(testdata, aes(x = Time, y = weight)) +
  geom_point()

# Wrap in nightowl Plot class for enhanced capabilities
plot_obj <- Plot$new(plot = p, options_svg = list(height = 4))
plot_obj

