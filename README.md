# nightowl <img src="man/figures/logo.png" align="right" height="104" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/joelgsponer/nightowl/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/joelgsponer/nightowl/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/joelgsponer/nightowl/branch/dev/graph/badge.svg)](https://app.codecov.io/gh/joelgsponer/nightowl?branch=dev)
[![Security Scan Status](https://github.com/joelgsponer/nightowl/actions/workflows/security-scan.yaml/badge.svg)](https://github.com/joelgsponer/nightowl/actions/workflows/security-scan.yaml)
<!-- badges: end -->

**nightowl** is an R package for statistical visualization and analysis, specifically designed for clinical research applications. It provides a powerful declarative plotting system built on ggplot2 with R6 classes, comprehensive survival analysis capabilities, and interactive visualizations that make complex statistical analyses accessible and reproducible.

Whether you're analyzing patient outcomes, creating regulatory-ready figures, or exploring clinical trial data, nightowl offers a consistent, professional toolkit that bridges the gap between statistical rigor and visual clarity.

## Key Features

- **📊 Declarative Plotting System**: Build complex visualizations with a clean, chainable R6 class architecture
- **🔬 Clinical Research Focus**: Specialized functions for survival analysis, biostatistics, and regulatory reporting
- **📈 Survival Analysis**: Kaplan-Meier curves, Cox proportional hazards modeling, and forest plots
- **🎨 Customizable Styling**: 14+ predefined YAML styles with full customization options
- **⚡ Interactive Visualizations**: SVG rendering with hover effects and downloadable outputs
- **📋 Statistical Summaries**: Comprehensive summary tables with integrated statistical tests
- **🔗 Method Chaining**: Fluent interface for streamlined data analysis workflows

## Installation

```r
# Install from GitHub
remotes::install_github("joelgsponer/nightowl")
```

## Quick Start

nightowl is built around three main R6 classes that work together seamlessly:

### Basic Plotting with the Plot Class

```r
library(nightowl)
library(palmerpenguins)

# Create a declarative plot with the Plot class
plot <- Plot$new(
  ggplot2::ggplot(penguins, ggplot2::aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  ggplot2::geom_point()
)

# Render as interactive SVG
plot$svg()

# Export as HTML with custom styling
plot$html()
```

### Statistical Summaries with the Summary Class

```r
# Create comprehensive statistical summaries with method chaining
summary <- Summary$new(
  penguins, 
  column = "bill_length_mm", 
  group_by = "species",
  method = summarise_numeric_pointrange
)

# Generate interactive table
summary$reactable()

# Access underlying calculations
summary$calculations
```

### Survival Analysis with the Coxph Class

```r
# Cox proportional hazards modeling for survival data
library(survival)

cox_model <- Coxph$new(
  data = lung,
  time = "time",
  event = "status", 
  covariates = c("age", "sex")
)

# Generate forest plot
cox_model$forestplot()

# Access model summary
cox_model$summary()
```

### Method Chaining and Fluent Interface

```r
# Combine multiple operations in a single pipeline
penguins %>%
  Summary$new("body_mass_g", group_by = "species", method = summarise_numeric_pointrange) %>%
  reactable()

# Chain plot customizations
Plot$new(my_ggplot)$
  svg(width = 10, height = 6)$
  html(resize = TRUE)
```

## API Overview

nightowl's API is designed around three core principles:

1. **Declarative Configuration**: Define what you want, not how to create it
2. **Method Chaining**: Fluent interfaces for streamlined workflows  
3. **Extensible Styling**: YAML-based configuration for consistent branding

### Core Classes

| Class | Purpose | Key Methods |
|-------|---------|-------------|
| `Plot` | Main plotting engine with SVG/HTML rendering | `svg()`, `html()`, `plot` |
| `Summary` | Statistical summaries and table generation | `reactable()`, `calculations` |
| `Coxph` | Cox proportional hazards survival analysis | `forestplot()`, `summary()` |

### Summary Functions

The `summarise_*` family provides modular statistical summaries:

- `summarise_numeric_pointrange()`: Mean ± confidence intervals
- `summarise_categorical()`: Frequencies and percentages  
- `summarise_numeric_histogram()`: Distribution summaries
- `summarise_numeric_violin()`: Density-based summaries

## Styling and Customization

nightowl includes 14+ predefined styles in `inst/styles/` for consistent, publication-ready figures:

```r
# Load and apply predefined styles
my_plot <- styled_plot("Boxplot-Dotplot", data = penguins, x = "species", y = "bill_length_mm")

# Customize with YAML configuration
load_style("custom-style.yaml")
```

## Documentation

- **Function Reference**: See `?nightowl` or individual function help pages
- **Development Guide**: See [`CLAUDE.md`](CLAUDE.md) for architecture details
- **Security Policy**: See [`inst/SECURITY.md`](inst/SECURITY.md) for security procedures

## Development

nightowl welcomes contributions! To get started:

1. **Read the Contributing Guide**: See [`CONTRIBUTING.md`](CONTRIBUTING.md) for detailed development workflow
2. **Understanding the Architecture**: Review [`CLAUDE.md`](CLAUDE.md) for technical implementation details
3. **Running Tests**: Use `devtools::test()` to run the comprehensive test suite
4. **Security**: Run `source("inst/scripts/security-check.R")` for dependency vulnerability scanning

### Development Commands

```r
# Package development
devtools::test()          # Run tests
devtools::check()         # Package validation  
devtools::document()      # Update documentation
pkgdown::build_site()     # Build documentation site

# Security scanning
source("inst/scripts/security-check.R")
```

## Security

This package includes comprehensive security vulnerability scanning for R dependencies:

- **Automated scanning** via GitHub Actions on every push and weekly schedules
- **Local security checks** using `source("inst/scripts/security-check.R")`
- **CI/CD integration** that fails builds on critical vulnerabilities
- **Multiple scanning tools**: oysteR, OSV Scanner, and riskmetric

See [`inst/SECURITY.md`](inst/SECURITY.md) for detailed security policy and scanning procedures.
