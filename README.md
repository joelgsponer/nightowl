# nightowl <img src="man/figures/logo.png" align="right" height="104" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/joelgsponer/nightowl/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/joelgsponer/nightowl/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/joelgsponer/nightowl/branch/dev/graph/badge.svg)](https://app.codecov.io/gh/joelgsponer/nightowl?branch=dev)
[![Security Scan Status](https://github.com/joelgsponer/nightowl/actions/workflows/security-scan.yaml/badge.svg)](https://github.com/joelgsponer/nightowl/actions/workflows/security-scan.yaml)
[![pkgdown](https://github.com/joelgsponer/nightowl/actions/workflows/pkgdown.yml/badge.svg)](https://github.com/joelgsponer/nightowl/actions/workflows/pkgdown.yml)
<!-- badges: end -->

**Statistical visualization and analysis toolkit for clinical research applications.**

A comprehensive R package providing declarative plotting, survival analysis, and interactive visualizations specifically designed for clinical research workflows. Built on a modern R6 architecture with three main classes:

- **`Plot`**: Declarative plotting system with SVG rendering and interactive HTML output
- **`Summary`**: Statistical summaries with embedded visualizations and reactable tables  
- **`Coxph`**: Cox proportional hazards modeling for survival analysis

## Why nightowl?

- **🏥 Clinical Research Focus**: Purpose-built for clinical research workflows and regulatory requirements
- **📊 R6 Architecture**: Modern object-oriented design with method chaining for fluent interfaces
- **⚰️ Survival Analysis**: Built-in Kaplan-Meier curves and Cox proportional hazards modeling
- **📋 Statistical Summaries**: Comprehensive `summarise_*` family of functions for clinical data
- **🎯 Interactive Elements**: SVG rendering with hover effects and reactable tables
- **🎨 YAML Styling**: 14+ predefined plot styles with customizable configuration system

## Quick Start

### Basic Statistical Summaries

```r
library(nightowl)
library(survival)

# Create statistical summary with embedded visualization
summary_table <- Summary$new(
  data = veteran, 
  column = "karno", 
  group_by = "celltype",
  method = summarise_numeric_pointrange
)$reactable()

# Categorical summary with percentages  
treatment_summary <- Summary$new(
  data = veteran,
  column = "trt", 
  group_by = "celltype",
  method = summarise_categorical_with_total
)$kable()
```

### Survival Analysis

```r
# Kaplan-Meier survival curves
km_plot <- plot_km(
  data = veteran,
  time = "time",
  event = "status", 
  treatment = "trt",
  title = "Overall Survival by Treatment"
)

# Cox proportional hazards analysis
cox_model <- Coxph$new(
  data = veteran,
  time = "time",
  event = "status",
  treatment = "trt", 
  covariates = c("karno", "age"),
  group_by = "celltype"
)

# Generate forest plot for results
cox_model$forest_plot()
```

### Interactive Visualizations

```r
# Any ggplot2 can be made interactive
library(ggplot2)
my_plot <- ggplot(veteran, aes(karno, time, color = factor(trt))) + 
  geom_point() + 
  labs(title = "Performance vs Survival Time")

# Convert to interactive SVG with hover effects
Plot$new(my_plot, options_svg = list(width = 8, height = 6))$html()
```

### Method Chaining and Fluent Interfaces

```r
# Chain multiple operations elegantly
result <- Summary$new(veteran, "karno", group_by = "celltype")$
  add_calculation(list(Missing = function(x) sum(is.na(x))))$
  reactable(
    options = list(
      searchable = TRUE,
      pagination = FALSE
    )
  )
```

## Documentation

Complete documentation is available at: **https://joelgsponer.github.io/nightowl/**

The documentation site includes:
- **Function reference** with examples and usage details
- **Getting started guide** for new users
- **Statistical summaries** and analysis workflows  
- **Survival analysis** tutorials and examples

## Development and Contributing

nightowl is actively developed with a focus on clinical research applications. Contributions are welcome!

### Getting Started with Development

```r
# Clone and setup development environment
devtools::install_dev_deps()
devtools::document()
devtools::test()
```

### Contributing Guidelines

- **Comprehensive Guide**: See [`CONTRIBUTING.md`](CONTRIBUTING.md) for detailed development workflow
- **Code Standards**: Follow tidyverse style guidelines and include tests for new functionality
- **Clinical Focus**: Ensure contributions align with clinical research requirements
- **R6 Architecture**: Maintain consistency with existing Plot, Summary, and Coxph class patterns

### Development Commands

```r
# Run all tests
devtools::test()

# Check package integrity
devtools::check()

# Build documentation site  
pkgdown::build_site()

# Security scanning
source("inst/scripts/security-check.R")
```

## Installation

```r
# Install from GitHub
remotes::install_github("joelgsponer/nightowl")

# Development version
remotes::install_github("joelgsponer/nightowl@dev")
```

## Security

This package includes comprehensive security vulnerability scanning for R dependencies:

- **Automated scanning** via GitHub Actions on every push and weekly schedules
- **Local security checks** using `source("inst/scripts/security-check.R")`
- **CI/CD integration** that fails builds on critical vulnerabilities
- **Multiple scanning tools**: oysteR, OSV Scanner, and riskmetric

See [`inst/SECURITY.md`](inst/SECURITY.md) for detailed security policy and scanning procedures.
