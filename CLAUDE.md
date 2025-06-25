# CLAUDE.md

ALWAYS use package::function notation, e.g. ggplot2::ggplot() not ggplot() 

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

The `nightowl` package is an R statistical visualization and data summary package that provides a declarative approach to creating plots and tables. It focuses on survival analysis, comprehensive data summarization, and statistical testing with extensive plotting capabilities.

## Development Commands

### Build and Installation
- `make build` - Complete build pipeline: commits changes, generates documentation, styles code, builds pkgdown site, and installs package
- `make install` - Simple build and install
- `devtools::document()` - Generate documentation from roxygen comments
- `styler::style_dir("R")` - Format R code according to tidyverse style
- `pkgdown::build_site()` - Build documentation website

### Testing
- `testthat::test_dir("tests/testthat")` - Run all tests
- `testthat::test_file("tests/testthat/test-<name>.R")` - Run specific test file
- `devtools::test()` - Run package tests via devtools

## Architecture Overview

### Core R6 Classes
The package uses R6 object-oriented programming with two main classes:
- `Plot` (`plot.R:1-200+`) - Base plotting class with layered architecture
- `DeclarativePlot` (`plot.R:200+`) - High-level declarative plotting interface
- `.NightowlOptions` (`options.R`) - Global configuration singleton initialized in `zzz.R`

### Key Modules by Function

**Plotting System:**
- `plot.R` - Core R6 plotting classes and layer system
- `add_plots.R` - Plot type implementations (barplot, violin, histogram, etc.)
- `styles.R` - YAML-based styling system with 11 built-in templates in `inst/styles/`
- `donut_plot.R`, `forest.R`, `inline_plots.R` - Specialized plot types

**Statistical Analysis:**
- `summaries.R` - Comprehensive data summarization with integrated plotting
- `survival.R`, `km.R`, `coxph.R` - Survival analysis and Kaplan-Meier functionality
- `grouped_chisq.R`, `tests.R`, `ci.R` - Statistical testing and confidence intervals

**Data Processing:**
- `data.R` - Core data manipulation (outlier detection, counting)
- `transformations.R`, `percentage.R` - Data transformation utilities
- `correlation_matrix.R` - Correlation analysis

### Styling System
The package uses a YAML-based declarative styling approach:
- Style templates located in `inst/styles/` directory
- Styles define color palettes, themes, and plot aesthetics
- Accessible via the global options system

### Testing Structure
Uses `testthat` framework with comprehensive coverage:
- 20+ test files covering all major functionality
- Tests organized by module (e.g., `test-coxph.R`, `test-donut_plot.R`)
- Integration tests for plotting and statistical functions

## Dependencies and Integration

The package heavily integrates with:
- **ggplot2 ecosystem** for core visualization
- **tidyverse** (dplyr, purrr) for data manipulation and functional programming
- **Specialized packages**: `ggdist`, `GGally`, `reactable` for enhanced visualizations
- **Statistical packages**: `Hmisc` for statistical functions

## Development Patterns

### Functional Programming
- Extensive use of `purrr` for functional operations
- Pipeline-based data transformations using `%>%`
- Template-based summarization system

### Declarative Design
- YAML configuration for styling
- Mapping-driven data visualization
- Layer-based plot construction similar to ggplot2

### Modular Architecture
- Separation of concerns across focused modules
- Plugin-style layer system for extending plots
- Extensible styling framework via YAML templates

