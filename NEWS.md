# nightowl (development version)

## New Features

### Documentation Enhancements
* Added comprehensive vignettes covering all major package functionality:
  - `getting-started`: Complete introduction to nightowl with R6 classes and declarative plotting
  - `statistical-summaries`: Deep dive into Summary R6 class and `summarise_*` function family
  - `survival-analysis`: Comprehensive survival analysis with Coxph R6 class and Kaplan-Meier functions
* Implemented automated vignette code validation in test suite
* Added pkgdown configuration for enhanced documentation website
* Created automated documentation deployment via GitHub Actions

### Testing Infrastructure
* Added `test-vignettes.R` for comprehensive vignette code validation
* Created `helper-vignette-data.R` with standardized test datasets
* Implemented code extraction and execution testing for all vignette examples
* Added YAML header validation and coding standards checks for vignettes

### Configuration Updates
* Added `VignetteBuilder: knitr` to DESCRIPTION for proper vignette building
* Created comprehensive `_pkgdown.yml` with organized reference sections
* Configured GitHub Actions workflow for automated pkgdown deployment

## Developer Experience
* Enhanced CI/CD pipeline with vignette validation
* Comprehensive testing framework ensures all documentation examples execute correctly
* Automated quality assurance for vignette content and structure

---

*This release significantly enhances the package documentation and user onboarding experience while maintaining backward compatibility.*