# CODE_STYLE.md

This document outlines the coding conventions and style guidelines for the nightowl R package, based on existing patterns in the codebase. These standards ensure consistency, maintainability, and readability across the package.

## Table of Contents

1. [File Organization](#file-organization)
2. [Naming Conventions](#naming-conventions)  
3. [R6 Class Design Patterns](#r6-class-design-patterns)
4. [Function Structure](#function-structure)
5. [Documentation Standards](#documentation-standards)
6. [Testing Guidelines](#testing-guidelines)
7. [YAML Configuration Patterns](#yaml-configuration-patterns)
8. [Import and Dependency Management](#import-and-dependency-management)
9. [Code Formatting](#code-formatting)
10. [Error Handling](#error-handling)

## File Organization

### File Naming
- **R files**: Use descriptive, lowercase names with `.R` extension (preferred) or `.r` (legacy support)
- **Exception**: Some legacy files use `.r` extension (`Summary.r`, `formula.r`, `svg.r`, `utils.r`)
- **Test files**: Follow pattern `test-{module}.R` in `tests/testthat/`
- **YAML files**: Use PascalCase with descriptive names in `inst/styles/` (e.g., `Boxplot-Dotplot.yaml`)

### File Structure Patterns
```
R/
├── {class}.R          # Main R6 classes (plot.R, coxph.R, Summary.r)
├── {function-family}.R # Related functions (summaries.R, inline_plots.R)
├── utils-{type}.R     # Utility functions (utils-pipe.R)
├── imports.R          # Package imports
├── options.R          # Package options/configuration
└── zzz.R             # Package hooks (.onLoad, etc.)
```

### Section Separators
Use consistent comment separators for file sections:
```r
# =================================================
# Main section header
# =================================================

# Subsection header ----------------------------------------------------------------

#' roxygen2 documentation
```

## Naming Conventions

### Variables and Arguments
- **snake_case** for all variables and function arguments
- **Descriptive names**: `group_by`, `options_svg`, `add_caption`
- **Boolean variables**: Prefix with action verbs: `add_test`, `keep_y`, `wrap_header`

### Functions
- **snake_case** for all function names
- **Action verbs**: `set_data()`, `get_variables()`, `check_formula()`
- **Family naming**: Related functions share prefixes
  - `summarise_*()`: `summarise_numeric()`, `summarise_categorical()`
  - `calc_*()`: `calc_test()`, `calc_percentage()`
  - `render_*()`: `render_kable()`, `render_svg()`
  - `format_*()`: `format_frequencies()`, `format_p_value()`

### R6 Classes
- **PascalCase** for class names: `Plot`, `Summary`, `Coxph`, `DeclarativePlot`
- **Method naming**: snake_case for all methods
- **Private methods**: Use same naming as public, distinguished by `private` list

### Constants and Options
- **Package-level options**: Use descriptive names in options objects
- **Field names**: Use descriptive snake_case: `options_svg`, `options_reactables`

## R6 Class Design Patterns

### Class Structure Template
```r
#' R6 Class Description
#' @export
ClassName <- R6::R6Class("ClassName",
  public = list(
    # Fields (documented with @field if needed)
    field_name = NULL,
    
    # Constructor
    initialize = function(...) {
      # Parameter handling with purrr::imap
      purrr::imap(list(...), function(.x, .y) {
        if (.y %in% names(self)) {
          self[[.y]] <- .x
        }
      })
      # Initialization chain
      self$set_data()
      self$set_method()
      invisible(self)
    },
    
    # Setters (return self for chaining)
    set_something = function(value) {
      self$something <- value
      invisible(self)
    },
    
    # Getters
    get_something = function() {
      return(self$something)
    },
    
    # Validators  
    check_something = function() {
      if (condition) {
        rlang::abort("Error message")
      }
    },
    
    # Main methods
    main_method = function() {
      # Implementation
    }
  ),
  private = list(
    # Private fields and methods
    private_field = NULL,
    private_method = function() {
      # Implementation
    }
  )
)
```

### Common Patterns

#### Parameter Handling in Constructor
```r
initialize = function(..., debug = FALSE) {
  if (debug) browser()
  purrr::imap(list(...), function(.x, .y) {
    if (.y %in% names(self)) {
      self[[.y]] <- .x
    }
  })
  # Continue initialization...
}
```

#### Method Chaining Support
```r
# Setters return self invisibly
set_data = function(data) {
  self$data <- data
  invisible(self)
}
```

#### Memoization for Expensive Operations
```r
set_plot = function() {
  self$plot <- memoise::memoise(function() {
    # Expensive plotting computation
  })()
}
```

## Function Structure

### Function Template
```r
#' @title Function Title
#' @description Brief description
#' @param param_name Parameter description
#' @return Return value description
#' @export
function_name <- function(required_param, 
                         optional_param = default_value,
                         ...) {
  # Input validation
  stopifnot(is.list(required_param))
  
  # Main logic
  result <- process_data(required_param)
  
  # Return
  return(result)
}
```

### Parameter Patterns
- **Required parameters first**
- **Optional parameters with defaults**
- **`...` for flexible argument passing**
- **Self-referencing in R6 methods**: Many functions accept `self` as first parameter

### Return Patterns
- **Explicit returns**: Use `return()` for clarity
- **R6 methods**: Return `invisible(self)` for chaining or computed values
- **List returns**: For complex outputs, return named lists

## Documentation Standards

### Roxygen2 Patterns

#### Complete Documentation Template
```r
#' @title Function Title
#' @description Detailed description of what the function does
#' @details Additional details if needed
#' @param param_name Description of parameter
#' @param .data Data frame input (common pattern)
#' @param ... Additional arguments passed to other functions
#' @return Description of return value
#' @examples
#' \dontrun{
#'   example_code()
#' }
#' @export
```

#### R6 Class Documentation
```r
#' R6 Class for [Purpose]
#' @description Detailed class description
#' @field field_name Description of public field
#' @section Methods:
#' \describe{
#'   \item{method_name()}{Method description}
#' }
#' @export
```

#### Incomplete Documentation Pattern
Many functions use placeholder documentation:
```r
#' @title
#' MISSING_TITLE
#' @export
```
**Note**: This should be replaced with proper documentation in new code.

### Import Documentation
```r
#' @importFrom package function1 function2
#' @export
```

## Testing Guidelines

### Test File Structure
- **File naming**: `test-{module}.R`
- **Test organization**: Group related tests in single files
- **Test naming**: Use descriptive test names

### Test Patterns
```r
test_that("descriptive test name", {
  # Setup
  testdata <- setup_test_data()
  
  # Test execution with expect_* functions
  result <- function_to_test(testdata)
  
  # Assertions
  expect_true(condition)
  expect_equal(result$field, expected_value)
  
  # Test R6 objects
  obj <- ClassName$new(data = testdata)
  expect_s3_class(obj, "R6")
  expect_true(inherits(obj, "ClassName"))
})
```

### Common Test Utilities
- Use `require(magrittr)` for pipe operations in tests
- Test data often uses `ChickWeight`, `palmerpenguins::penguins`, or `mtcars`
- Test both positive and negative cases
- Test method chaining in R6 classes

## YAML Configuration Patterns

### Style Configuration Structure
```yaml
name: Style Name - Description
description: Brief description
transform:                    # Data transformations
  x: factor
  fill: factor
layers:                      # Plot layers
- type: layer_type
  mapping:
    group: ~
    color: ~
  parameter: value
svg:                         # SVG options
  height: 8
  width: 8
  scaling: 1
```

### Naming Conventions
- **File names**: PascalCase with descriptive names
- **YAML fields**: lowercase with underscores
- **Layer types**: lowercase names matching function names

### Common Patterns
- Use `~` for null/empty mappings in YAML
- Consistent indentation (2 spaces)
- SVG dimensions typically 8x8 with scaling = 1

## Import and Dependency Management

### Package Imports (`imports.R`)
```r
#' @importFrom package function1 function2
NULL
```

### NAMESPACE Patterns
- **S3 methods**: Explicit exports for custom S3 methods
- **R6 classes**: Direct export of class constructors
- **Function families**: Export all related functions
- **Pipe operator**: `@importFrom magrittr %>%` and `@export`

### Import Strategies
- **Selective imports**: Import specific functions rather than entire packages
- **Common imports**: `dplyr`, `purrr`, `ggplot2`, `stringr` functions
- **Specialized imports**: `survival::strata`, `vctrs::vec_*` functions

## Code Formatting

### Indentation and Spacing
- **2 spaces** for indentation (no tabs)
- **Space around operators**: `x <- y + z`
- **Space after commas**: `function(a, b, c)`
- **No trailing whitespace**

### Line Length
- **Prefer shorter lines** when possible
- **Break long function calls** across multiple lines:
```r
long_function_call(
  parameter1 = value1,
  parameter2 = value2,
  parameter3 = value3
)
```

### Alignment Patterns
```r
# Align similar assignments
self$field1    <- value1
self$field2    <- value2
self$longer_field <- value3

# Align function parameters  
function_name(param1 = value1,
              param2 = value2,
              param3 = value3)
```

### Pipe Usage
```r
# Use pipes for data transformation chains
data %>%
  dplyr::filter(condition) %>%
  dplyr::mutate(new_var = transformation) %>%
  dplyr::select(relevant_columns)
```

## Error Handling

### Error Messages
- **Use `rlang::abort()`** for clear error messages
- **Descriptive messages**: Include variable names and context
```r
if (!all(vars %in% names(self$data))) {
  missing <- vars[!vars %in% names(self$data)]
  msg <- glue::glue("`{missing}` not present in data")
  rlang::abort(msg)
}
```

### Validation Patterns
```r
# Check required fields
check_data = function() {
  if (is.null(self$data)) {
    rlang::abort("No data provided - use `set_data` method to update")
  }
}

# Use stopifnot for simple assertions
stopifnot(is.list(calculations))
```

### Safe Operations
```r
# Use purrr::safely for operations that might fail
safe_operation <- purrr::safely(risky_function)
result <- safe_operation(input)
if (!is.null(result$error)) {
  # Handle error
}
```

### Try-Catch Patterns
```r
tryCatch({
  # Risky operation
  result <- operation()
}, error = function(e) {
  # Error handling
  default_value
})
```

---

## Complexity Score: 8/10

**Reasoning**: The nightowl package exhibits high complexity due to:

1. **Advanced R6 OOP patterns** with inheritance and complex method chaining
2. **Multiple domain expertise** required (statistics, visualization, survival analysis)
3. **Complex data transformations** and statistical calculations
4. **YAML-driven configuration** system requiring domain knowledge
5. **Interactive visualization** with SVG/HTML rendering
6. **Extensive dependency management** across multiple statistical packages
7. **Meta-programming** with rlang for dynamic evaluation
8. **Package infrastructure** including custom S3 methods and vctrs integration

The codebase requires deep R knowledge and understanding of statistical concepts, making it accessible primarily to experienced R developers with domain expertise in biostatistics and clinical research.