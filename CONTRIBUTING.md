# Contributing to nightowl

Welcome to the nightowl R package! We're excited that you're interested in contributing to our statistical visualization and analysis toolkit for clinical research applications.

nightowl provides a declarative plotting system built on ggplot2 with R6 classes, survival analysis capabilities, and interactive visualizations. This guide will help you get started with contributing to the project.

## Table of Contents

- [Development Setup](#development-setup)
- [Package Architecture](#package-architecture)
- [Development Workflow](#development-workflow)
- [Testing](#testing)
- [Pull Request Process](#pull-request-process)
- [Code Standards](#code-standards)
- [Reporting Issues](#reporting-issues)
- [Resources](#resources)

## Development Setup

### Prerequisites

- **R version**: 4.0.0 or higher
- **RStudio**: Recommended for development (optional but helpful)
- **Git**: For version control
- **Rtools**: Required on Windows for package compilation

### Installation

1. **Clone the repository:**
   ```bash
   git clone https://github.com/joelgsponer/nightowl.git
   cd nightowl
   ```

2. **Install development dependencies:**
   ```r
   # Install devtools if not already installed
   install.packages("devtools")
   
   # Install all package dependencies
   devtools::install_deps(dependencies = TRUE)
   ```

3. **Load the package for development:**
   ```r
   devtools::load_all()
   ```

4. **Verify installation:**
   ```r
   # Run basic tests
   devtools::test()
   
   # Check package structure
   devtools::check()
   ```

### Development Environment Setup

We recommend using RStudio with the following settings:
- Enable "Use devtools package functions if available" in Global Options > Packages
- Set up roxygen2 for documentation generation
- Configure Git integration for version control

## Package Architecture

nightowl is built around a modular R6 class system designed for clinical research visualization:

### Core R6 Classes

- **Plot** (`R/plot.R`): Main plotting class with SVG rendering, HTML output, and options management
- **Summary** (`R/Summary.r`): Statistical summaries with data aggregation and table generation
- **Coxph** (`R/coxph.R`): Cox proportional hazards modeling for survival analysis

### Key Components

1. **Declarative Plotting System**: Layers and customizable styling via YAML files
2. **Statistical Analysis**: Built-in statistical tests and summary functions (`summarise_*` family)
3. **Survival Analysis**: Kaplan-Meier curves and Cox regression with forest plots
4. **Interactive Elements**: SVG rendering with hover effects and reactable tables
5. **Inline Plots**: Specialized mini-plots for embedding in tables

### Directory Structure

- `R/`: Core R source files
- `inst/styles/`: YAML configuration files for plot styling (14 predefined templates)
- `inst/assets/`: JavaScript libraries (D3, ggiraph) and CSS for interactive elements
- `tests/testthat/`: Test suite with comprehensive coverage
- `man/`: Generated documentation files

### Design Patterns

**Method Chaining**: R6 classes support fluent interfaces
```r
Summary$new(data, "column", method = summarise_numeric_pointrange)$reactable()
```

**YAML-Driven Styling**: Plot styles defined declaratively
```r
load_style("Boxplot-Dotplot.yaml")
```

**Modular Summary Functions**: Composable statistical summaries
```r
summarise_numeric_pointrange()
summarise_categorical_barplot()
```

## Development Workflow

### Branch Strategy

1. **Create a feature branch** from `dev`:
   ```bash
   git checkout dev
   git pull origin dev
   git checkout -b feature/your-feature-name
   ```

2. **Make your changes** in logical, atomic commits

3. **Test frequently** during development:
   ```r
   devtools::test()
   devtools::check()
   ```

### Commit Guidelines

- Use clear, descriptive commit messages
- Follow conventional commit format when applicable:
  - `feat:` for new features
  - `fix:` for bug fixes
  - `docs:` for documentation changes
  - `test:` for test additions/modifications
  - `refactor:` for code refactoring

Example:
```
feat: add support for custom color palettes in YAML styles

- Extend YAML parser to handle custom color definitions
- Add validation for hex color codes
- Update documentation with color palette examples
```

## Testing

nightowl uses the `testthat` framework for comprehensive testing.

### Running Tests

```r
# Run all tests
devtools::test()

# Run specific test file
devtools::test(filter = "test-plot")

# Run tests with coverage
covr::package_coverage()
```

### Test Organization

Tests are organized in `tests/testthat/` with files following the `test-*.R` pattern:
- `test-plot.R`: Plot class functionality
- `test-summary.R`: Summary class and statistical functions
- `test-survival.R`: Survival analysis features
- `test-inline-plots.R`: Inline plotting capabilities

### Writing Tests

When adding new functionality:

1. **Create corresponding tests** in the appropriate test file
2. **Test both success and failure cases**
3. **Include edge cases and boundary conditions**
4. **Verify backward compatibility**

Example test structure:
```r
test_that("summarise_numeric_pointrange handles missing values", {
  data_with_na <- data.frame(x = c(1, 2, NA, 4))
  result <- summarise_numeric_pointrange(data_with_na, "x")
  expect_true(!is.na(result$mean))
  expect_true(result$n == 3)
})
```

### Package Validation

Before submitting changes, ensure all checks pass:

```r
# Full package check (CRAN standards)
devtools::check()

# Update documentation
devtools::document()

# Install package locally
devtools::install()
```

## Pull Request Process

### Before Submitting

1. **Ensure all tests pass** locally
2. **Update documentation** if needed
3. **Add news entry** if appropriate
4. **Check code style** follows package conventions

### PR Requirements

1. **Clear description** of changes and motivation
2. **Reference related issues** using `Closes #issue-number`
3. **Include test coverage** for new functionality
4. **Update documentation** for user-facing changes
5. **Maintain backward compatibility** or provide migration guide

### PR Template

```markdown
## Summary
Brief description of changes

## Changes Made
- List of specific changes
- Include any breaking changes

## Testing
- [ ] All existing tests pass
- [ ] New tests added for new functionality
- [ ] Manual testing completed

## Documentation
- [ ] Code documentation updated
- [ ] User-facing documentation updated (if applicable)
```

### Review Process

1. **Automated checks** must pass (tests, R CMD check)
2. **Code review** by maintainers
3. **Testing** in different environments if needed
4. **Approval** before merging

## Code Standards

### R Code Style

Follow the [tidyverse style guide](https://style.tidyverse.org/) with these specific conventions:

- **Function names**: Use `snake_case`
- **Variable names**: Use `snake_case`
- **Class names**: Use `PascalCase` for R6 classes
- **Constants**: Use `UPPER_SNAKE_CASE`

### Documentation

- **All exported functions** must have roxygen2 documentation
- **Include examples** that demonstrate usage
- **Document parameters** and return values clearly
- **Use `@inheritParams`** when appropriate

Example documentation:
```r
#' Create numeric point range summary
#'
#' Generates a point range visualization for numeric data with mean and 
#' confidence intervals.
#'
#' @param data A data frame containing the data to summarize
#' @param column Character string naming the column to summarize
#' @param conf.level Confidence level for intervals (default: 0.95)
#' @return A Summary object with point range visualization
#' @examples
#' library(nightowl)
#' data(mtcars)
#' summarise_numeric_pointrange(mtcars, "mpg")
#' @export
```

### YAML Style Files

When contributing new styling options:

- **Follow existing structure** in `inst/styles/`
- **Use descriptive names** (e.g., `Boxplot-SummaryMean.yaml`)
- **Include comments** for complex configurations
- **Test with multiple datasets** before submitting

### Package Dependencies

- **Minimize new dependencies** - justify any additions
- **Use `Imports`** for essential dependencies
- **Use `Suggests`** for optional features
- **Document version requirements** when necessary

## Reporting Issues

### Bug Reports

When reporting bugs, please include:

1. **Reproducible example** with minimal code
2. **Expected vs actual behavior**
3. **Session information**: `sessionInfo()`
4. **Package version**: `packageVersion("nightowl")`

Use this template:
```markdown
## Bug Description
Brief description of the issue

## Reproducible Example
```r
# Minimal code that reproduces the issue
library(nightowl)
# ...
```

## Expected Behavior
What you expected to happen

## Actual Behavior
What actually happened

## Session Info
```r
sessionInfo()
```
```

### Feature Requests

For new features, provide:

1. **Clear use case** and motivation
2. **Proposed API** or interface
3. **Examples** of how it would be used
4. **Alternatives considered**

### Performance Issues

Include:
1. **Benchmarking code** and results
2. **Data characteristics** (size, structure)
3. **System specifications**
4. **Performance expectations**

## Resources

### R Package Development

- [R Packages book](https://r-pkgs.org/) by Hadley Wickham and Jenny Bryan
- [Writing R Extensions](https://cran.r-project.org/doc/manuals/r-release/R-exts.html) - Official CRAN guide
- [Advanced R](https://adv-r.hadley.nz/) for advanced R programming concepts

### nightowl-Specific Resources

- [CLAUDE.md](./CLAUDE.md) - Detailed technical architecture guide
- [README.md](./README.md) - Basic package overview and installation
- Package documentation: `help(package = "nightowl")`

### Testing and Quality

- [testthat documentation](https://testthat.r-lib.org/)
- [R CMD check](https://r-pkgs.org/r-cmd-check.html) guide
- [Code coverage with covr](https://covr.r-lib.org/)

### Statistical Methods

- [Survival Analysis in R](https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html)
- [ggplot2 extensions](https://exts.ggplot2.tidyverse.org/)
- [Clinical data visualization best practices](https://www.fda.gov/regulatory-information/search-fda-guidance-documents/statistical-principles-clinical-trials)

## Getting Help

- **GitHub Issues**: For bug reports and feature requests
- **GitHub Discussions**: For questions and general discussion
- **Code Review**: Maintainers will provide feedback on pull requests

Thank you for contributing to nightowl! Your contributions help make clinical research visualization more accessible and effective for the R community.