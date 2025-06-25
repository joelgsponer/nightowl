# Security Guidelines for nightowl Package

## Overview

This document outlines the security considerations and best practices implemented in the nightowl package to prevent code injection and other security vulnerabilities.

## Security Fixes Implemented

### 1. Formula Construction Security

The `create_Surv_formula()` function has been rewritten to use safe formula construction methods:

**Previous vulnerability:**
```r
# UNSAFE - Direct string interpolation
base <- glue::glue("survival::Surv({time}, {event}) ~ {treatment}")
```

**Current secure implementation:**
```r
# SAFE - Programmatic formula construction with validation
lhs <- rlang::call2("Surv", rlang::sym(time), rlang::sym(event), .ns = "survival")
rhs <- rlang::sym(treatment)
formula_obj <- rlang::new_formula(lhs, rhs)
```

**Security features:**
- Input validation for all variable names
- Rejection of variables with special characters
- Verification that variables exist in the dataset
- No string interpolation that could allow code injection

### 2. Function Evaluation Security

The `add_geom()` function now uses a whitelist approach for cut functions:

**Previous vulnerability:**
```r
# UNSAFE - Arbitrary code execution
if (is.character(cut_f)) cut_f <- eval(parse(text = cut_f))
```

**Current secure implementation:**
```r
# SAFE - Whitelist-based function lookup
allowed_cut_functions <- c(
  "cut_interval" = ggplot2::cut_interval,
  "cut_number" = ggplot2::cut_number,
  "cut_width" = ggplot2::cut_width
)
```

### 3. Parameter Evaluation Security

The `summarise()` function no longer evaluates arbitrary expressions:

**Previous vulnerability:**
```r
# UNSAFE - Direct evaluation of expressions
if (rlang::is_expression(parameters)) {
  parameters <- eval(parameters)
}
```

**Current secure implementation:**
```r
# SAFE - Only accept lists
if (!is.list(parameters)) {
  stop("parameters must be a list, not an expression")
}
```

## Security Testing

Comprehensive security tests have been implemented in `tests/testthat/test-security-formula-construction.R`:

- Tests for code injection through variable names
- Tests for malicious function references
- Tests for expression-based attacks
- Tests for edge cases and boundary conditions

## Best Practices for Contributors

### 1. Never Use String-Based Code Construction

❌ **Avoid:**
```r
formula_str <- paste("y ~", paste(vars, collapse = " + "))
formula <- as.formula(formula_str)
```

✅ **Use instead:**
```r
lhs <- rlang::sym("y")
rhs <- purrr::reduce(purrr::map(vars, rlang::sym), ~ rlang::call2("+", .x, .y))
formula <- rlang::new_formula(lhs, rhs)
```

### 2. Always Validate User Inputs

❌ **Avoid:**
```r
function(data, column) {
  data[[column]]  # No validation
}
```

✅ **Use instead:**
```r
function(data, column) {
  if (!column %in% names(data)) {
    stop("Column not found in data")
  }
  if (!grepl("^[a-zA-Z][a-zA-Z0-9._]*$", column)) {
    stop("Invalid column name")
  }
  data[[column]]
}
```

### 3. Use Whitelists for Dynamic Function Calls

❌ **Avoid:**
```r
fun <- get(func_name)  # Can call any function
```

✅ **Use instead:**
```r
allowed_functions <- list(
  "mean" = mean,
  "median" = median
)
if (!func_name %in% names(allowed_functions)) {
  stop("Function not allowed")
}
fun <- allowed_functions[[func_name]]
```

### 4. Never Use eval() on User Input

❌ **Avoid:**
```r
result <- eval(parse(text = user_expression))
```

✅ **Use instead:**
```r
# Parse and validate the expression structure
expr <- rlang::parse_expr(user_expression)
# Only allow specific, safe operations
if (!is_safe_expression(expr)) {
  stop("Expression not allowed")
}
result <- rlang::eval_tidy(expr, data = safe_environment)
```

## Reporting Security Issues

If you discover a security vulnerability in the nightowl package, please report it privately by:

1. Creating a private security advisory on GitHub
2. Emailing the maintainers directly
3. Not disclosing the vulnerability publicly until it has been addressed

## Security Testing

Run security tests with:
```r
devtools::test(filter = "security")
```

All security tests must pass before any code changes are merged.