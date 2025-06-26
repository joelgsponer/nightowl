# Coding Conventions for nightowl R Package

This document outlines the coding conventions used in the nightowl R package to ensure consistency, maintainability, and readability across the codebase.

## 1. Naming Conventions

### Functions
- **Use snake_case** for all function names
- Choose descriptive names that clearly indicate purpose
- Examples: `format_frequencies()`, `summarise_numeric()`, `validate_data_frame()`

### Variables and Parameters
- **Use snake_case** for all variable and parameter names
- Use descriptive names that indicate content/purpose
- Examples: `group_by`, `min_value`, `max_value`, `data_frame`

### Classes (R6)
- **Use PascalCase** for class names
- Examples: `Plot`, `Summary`, `Coxph`, `DeclarativePlot`

### Methods (within R6 classes)
- **Use snake_case** for method names
- Examples: `set_plot()`, `get_width()`, `html()`

### Temporary/Local Variables
- **Use dot notation** for intermediate processing variables
- Examples: `.data`, `.reference`, `.group`, `.calculations`, `.p`

## 2. Code Formatting

### Indentation
- Use **2 spaces** for indentation (no tabs)
- Align function parameters across multiple lines
- Align ggplot2 operations with `+` operators

### Function Definitions
```r
function_name <- function(param1,
                         param2 = default_value,
                         param3 = NULL) {
  # Function body with 2-space indentation
}
```

### R6 Class Structure
```r
ClassName <- R6::R6Class("ClassName",
  public = list(
    # Fields first
    field1 = NULL,
    field2 = NULL,
    
    # Initialize method
    initialize = function(...) {
      # Initialization code
    },
    
    # Other methods grouped by functionality
    method_name = function() {
      # Method implementation
    }
  ),
  
  private = list(
    # Private fields and methods
    private_field = NULL,
    private_method = function() {
      # Private method implementation
    }
  )
)
```

## 3. Documentation Standards

### roxygen2 Documentation
All exported functions must include comprehensive roxygen2 documentation:

```r
#' @title Clear, descriptive title
#' @description
#' Detailed description of function purpose and behavior.
#' Include usage examples and important notes.
#' @param param_name Type. Description of parameter including valid values
#' @param param_name2 Type. Description with default value. Default TRUE.
#' @return Type. Clear description of return value and structure
#' @export
#' @examples
#' # Example usage
#' result <- function_name(param1 = "value")
```

### Documentation Requirements
- Always include `@title`, `@description`, `@param`, `@return`
- Include type information in parameter descriptions
- Mention default values where applicable
- Use `@export` for public functions, `@keywords internal` for internal ones
- Include `@examples` for exported functions

## 4. File Organization

### File Naming
- Use lowercase with dashes for multi-word files: `plot-utilities.R`
- Use `.R` extension (preferred) or `.r` (acceptable but be consistent)
- Group related functionality in single files

### File Structure
- Core R6 classes in separate files: `plot.R`, `coxph.R`
- Related functions grouped logically: `summaries.R`, `validation.R`
- Utilities and helpers in `utils.R`
- Theme-related functions in `theming.R`

### Within-File Organization
1. Package imports and dependencies
2. Global variables and constants
3. Main functions/classes
4. Helper functions
5. Validation functions
6. Utility functions

## 5. R6 Class Conventions

### Method Organization
Organize methods within R6 classes in this order:
1. **Fields** - Public fields first
2. **initialize** - Constructor method
3. **Data handling** - Methods for data manipulation
4. **Validation** - Input validation methods
5. **Computation** - Core logic methods
6. **Output/rendering** - Methods that produce output
7. **Utilities** - Helper methods

### Field and Method Naming
- Use descriptive names for fields: `options_svg`, `add_caption`
- Use action verbs for methods: `set_plot()`, `get_width()`, `validate_data()`
- Group related functionality together

## 6. Dependencies and Imports

### Package Usage
- Use explicit package prefixing: `ggplot2::ggplot()`, `dplyr::mutate()`
- Import only what's needed via NAMESPACE
- Document all dependencies in DESCRIPTION

### Core Dependencies
- **Data manipulation**: dplyr, purrr, tidyr, magrittr
- **Visualization**: ggplot2, ggpubr, patchwork
- **Classes**: R6
- **Performance**: memoise, furrr
- **Utilities**: stringr, glue, rlang

## 7. Error Handling and Validation

### Error Messages
- Use `rlang::abort()` for errors with informative messages
- Include context and suggested solutions in error messages
- Use consistent error message formatting

### Input Validation
- Validate all user inputs at function entry points
- Use dedicated validation functions from `validation.R`
- Include type checking and range validation where appropriate

### Security Considerations
- Prevent path traversal attacks in file operations
- Validate formula inputs to prevent code injection
- Use safe evaluation practices for user-provided code

## 8. Performance Conventions

### Memoization
- Use `memoise::memoise()` for expensive computations
- Implement cache management for memory efficiency
- Track cache hits/misses for optimization

### Memory Management
- Use memory-efficient data handling practices
- Avoid unnecessary data copying
- Clear temporary objects when appropriate

## 9. Testing Standards

### Test Organization
- Use `testthat` framework
- Name test files with `test-` prefix: `test-plot.R`
- Use descriptive test names that explain what's being tested

### Test Coverage
- Include unit tests for all exported functions
- Test error conditions and edge cases
- Include integration tests for complex workflows
- Add security tests for sensitive functionality

## 10. Comments and Documentation

### Code Comments
- Use section headers for major code sections:
  ```r
  # Main plotting functionality =============================================
  ```
- Include inline comments for complex logic
- Use TODO/FIXME markers for future improvements

### Comment Style
- Use `#` for single-line comments
- Use `#'` only for roxygen2 documentation
- Keep comments up-to-date with code changes

## 11. Version Control

### Commit Messages
- Use conventional commit format: `feat:`, `fix:`, `docs:`, `refactor:`
- Include issue references: `Refs #40`
- Write clear, descriptive commit messages

### Branch Naming
- Use feature branches: `issue/40-add-coding-conventions`
- Use descriptive branch names that indicate purpose
- Keep branches focused on single features/fixes

## 12. Code Review Guidelines

### Before Submitting
- Run `devtools::check()` to ensure package validity
- Run `devtools::test()` to ensure all tests pass
- Update documentation with `devtools::document()`
- Check code style consistency

### Review Checklist
- [ ] Follows naming conventions
- [ ] Includes appropriate documentation
- [ ] Has adequate test coverage
- [ ] Follows security best practices
- [ ] Maintains backward compatibility
- [ ] Updates relevant documentation

## 13. Backward Compatibility

### Breaking Changes
- Avoid breaking changes when possible
- When necessary, implement deprecation warnings
- Provide clear migration guidance
- Update documentation with migration examples

### Deprecation Process
1. Add deprecation warning to old functionality
2. Document new approach in function documentation
3. Update all internal usage to new approach
4. Plan removal for future major version

---

## Enforcement

These conventions should be followed for all new code and when modifying existing code. Consistency with existing patterns is important for maintainability and developer experience.

For questions about these conventions or suggestions for improvements, please open an issue in the repository.