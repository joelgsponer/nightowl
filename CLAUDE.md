# CLAUDE.md

ALWAYS use package::function notation, e.g. ggplot2::ggplot() not ggplot(), including the package itself e.g. nightowl::some_function instead of some_function
The package is called nightowl
The github repo has a project associated "nightowl" with columns:
Backlog
Ready
In progress
In review
Done
When working with github issues make sure to add them to the project in the correct column.

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

`nightowl` is an R package for statistical visualization and analysis, focused on clinical research applications. It provides a declarative plotting system built on ggplot2 with R6 classes, survival analysis capabilities, and interactive visualizations.

## Core Architecture

### R6 Class System
The package is built around three main R6 classes:
- **Plot**: Main plotting class in `R/plot.R` with SVG rendering, HTML output, and options management
- **Summary**: Statistical summaries in `R/Summary.r` with data aggregation and table generation  
- **Coxph**: Cox proportional hazards modeling in `R/coxph.R` for survival analysis

### Key Components
- **Plotting**: Declarative plotting system with layers and customizable styling via YAML files in `inst/styles/`
- **Statistical Analysis**: Built-in statistical tests and summary functions (`summarise_*` family in `R/summaries.R`)
- **Survival Analysis**: Kaplan-Meier curves and Cox regression with forest plots
- **Interactive Elements**: SVG rendering with hover effects and reactable tables
- **Inline Plots**: Specialized mini-plots for embedding in tables (`R/inline_plots.R`)

## Development Commands

### Testing
```r
# Run all tests
devtools::test()

# Run specific test file
devtools::test(filter = "test-plot")

# Check package
devtools::check()
```

### Building and Documentation
```r
# Install package
devtools::install()

# Update documentation
devtools::document()

# Build pkgdown site
pkgdown::build_site()
```

## File Structure

### Core R Files
- `R/plot.R` - Main Plot R6 class with SVG/HTML rendering capabilities
- `R/Summary.r` - Summary R6 class for statistical summaries and table generation
- `R/coxph.R` - Coxph R6 class for survival analysis
- `R/summaries.R` - Summary functions (`summarise`, `summarise_*` family)
- `R/inline_plots.R` - Inline plotting functions for embedding in tables
- `R/km.R` - Kaplan-Meier survival curve functions
- `R/forest.R` - Forest plot implementations
- `R/grouped_chisq.R` - Chi-square test analysis functions

### Configuration and Assets
- `inst/styles/` - YAML configuration files for plot styling and templates
- `inst/assets/` - JavaScript libraries (D3, ggiraph) and CSS for interactive elements
- `inst/testapp/` - Shiny test application

### Testing
- Tests use `testthat` framework in `tests/testthat/`
- Comprehensive test coverage including survival analysis, plotting, and statistical functions

## Dependencies

Core dependencies include:
- **Visualization**: ggplot2, ggpubr, ggdist, GGally
- **Data**: dplyr, purrr, magrittr, tibble
- **Statistics**: survival, Hmisc
- **Interactive**: reactable, ggiraph
- **Utilities**: stringr, uuid, vctrs

## Key Design Patterns

### Method Chaining
The R6 classes support method chaining for fluent interfaces:
```r
Summary$new(data, "column", method = summarise_numeric_pointrange)$reactable()
```

### YAML-Driven Styling
Plot styles are defined in YAML files in `inst/styles/`, allowing declarative configuration of plot appearance and behavior.

### Modular Summary Functions
The `summarise_*` family of functions provides modular statistical summaries that can be combined and customized.
