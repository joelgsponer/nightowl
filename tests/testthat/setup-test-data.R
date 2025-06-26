# Standardized Test Data Fixtures
# Centralized test data creation for consistent testing across all test files

# Standard Test Data Generators ====

#' Create small test dataset for basic functionality testing
#' @param n Number of rows (default: 30)
#' @param seed Random seed for reproducibility (default: 42)
#' @return tibble with standardized columns
create_small_test_data <- function(n = 30, seed = 42) {
  set.seed(seed)
  tibble::tibble(
    id = 1:n,
    group = sample(c("Group_A", "Group_B", "Group_C"), n, replace = TRUE),
    category = sample(c("Cat1", "Cat2", "Cat3", "Cat4"), n, replace = TRUE),
    numeric_var = rnorm(n, 50, 15),
    numeric_var2 = rnorm(n, 100, 25),
    logical_var = sample(c(TRUE, FALSE), n, replace = TRUE),
    factor_var = factor(sample(letters[1:5], n, replace = TRUE)),
    character_var = sample(paste0("text_", 1:10), n, replace = TRUE)
  )
}

#' Create medium test dataset for standard testing scenarios
#' @param n Number of rows (default: 100)
#' @param seed Random seed for reproducibility (default: 123)
#' @return tibble with comprehensive column types
create_medium_test_data <- function(n = 100, seed = 123) {
  set.seed(seed)
  tibble::tibble(
    id = 1:n,
    group = sample(c("Control", "Treatment_A", "Treatment_B"), n, replace = TRUE),
    subgroup = sample(c("Low", "Medium", "High"), n, replace = TRUE),
    category = sample(c("Alpha", "Beta", "Gamma", "Delta", "Epsilon"), n, replace = TRUE),
    numeric_var = rnorm(n, 75, 20),
    numeric_var2 = rnorm(n, 0, 1),
    numeric_var3 = runif(n, 0, 100),
    binary_outcome = sample(c(0, 1), n, replace = TRUE),
    continuous_outcome = rnorm(n, 10, 3),
    time_to_event = sample(1:365, n, replace = TRUE),
    event_occurred = sample(c(0, 1), n, replace = TRUE),
    weight = abs(rnorm(n, 70, 15)),
    age = sample(18:85, n, replace = TRUE),
    factor_ordered = factor(
      sample(c("Mild", "Moderate", "Severe"), n, replace = TRUE),
      levels = c("Mild", "Moderate", "Severe"), 
      ordered = TRUE
    )
  )
}

#' Create large test dataset for performance testing
#' @param n Number of rows (default: 1000)
#' @param n_groups Number of distinct groups (default: 5)
#' @param n_categories Number of distinct categories (default: 8)
#' @param seed Random seed for reproducibility (default: 456)
#' @return tibble optimized for performance testing
create_large_test_data <- function(n = 1000, n_groups = 5, n_categories = 8, seed = 456) {
  set.seed(seed)
  tibble::tibble(
    id = 1:n,
    group = sample(paste0("Group_", 1:n_groups), n, replace = TRUE),
    category = sample(paste0("Category_", 1:n_categories), n, replace = TRUE),
    subcategory = sample(paste0("Sub_", 1:3), n, replace = TRUE),
    numeric_var1 = rnorm(n, 50, 15),
    numeric_var2 = rnorm(n, 100, 30),
    numeric_var3 = runif(n, 0, 1),
    numeric_var4 = rexp(n, rate = 0.1),
    binary_var1 = sample(c(0, 1), n, replace = TRUE),
    binary_var2 = sample(c(TRUE, FALSE), n, replace = TRUE),
    ordinal_var = sample(1:7, n, replace = TRUE),
    time_var = sample(1:1000, n, replace = TRUE),
    event_var = sample(c(0, 1), n, replace = TRUE),
    weight_var = abs(rnorm(n, 65, 12))
  )
}

# Edge Case Test Data Generators ====

#' Create test data with missing values
#' @param n Number of rows (default: 50)
#' @param missing_prop Proportion of missing values (default: 0.2)
#' @param seed Random seed for reproducibility (default: 789)
#' @return tibble with systematic missing data patterns
create_missing_data_test <- function(n = 50, missing_prop = 0.2, seed = 789) {
  set.seed(seed)
  base_data <- create_medium_test_data(n, seed)
  
  # Introduce missing values systematically
  n_missing <- round(n * missing_prop)
  
  # Random missing in numeric variables
  base_data$numeric_var[sample(1:n, n_missing)] <- NA_real_
  base_data$numeric_var2[sample(1:n, n_missing)] <- NA_real_
  
  # Random missing in categorical variables
  base_data$group[sample(1:n, round(n_missing/2))] <- NA_character_
  base_data$category[sample(1:n, round(n_missing/2))] <- NA_character_
  
  # Complete missing for some rows
  complete_missing_rows <- sample(1:n, round(n_missing/4))
  base_data$numeric_var3[complete_missing_rows] <- NA_real_
  base_data$continuous_outcome[complete_missing_rows] <- NA_real_
  
  base_data
}

#' Create test data with extreme values
#' @param n Number of rows (default: 40)
#' @param seed Random seed for reproducibility (default: 321)
#' @return tibble with extreme values and outliers
create_extreme_values_test <- function(n = 40, seed = 321) {
  set.seed(seed)
  base_data <- create_small_test_data(n, seed)
  
  # Add extreme values
  base_data$numeric_var[1:3] <- c(Inf, -Inf, 1e10)
  base_data$numeric_var2[4:6] <- c(-1e10, 1e-10, -1e-10)
  
  # Add very large categorical levels
  base_data$category[7:10] <- paste0("Very_Long_Category_Name_", 1:4)
  
  # Add single observation categories
  base_data$group[n] <- "Singleton_Group"
  
  base_data
}

#' Create survival analysis test data
#' @param n Number of rows (default: 80)
#' @param seed Random seed for reproducibility (default: 654)
#' @return tibble optimized for survival analysis testing
create_survival_test_data <- function(n = 80, seed = 654) {
  set.seed(seed)
  tibble::tibble(
    id = 1:n,
    time = sample(1:365, n, replace = TRUE),
    event = sample(c(0, 1), n, replace = TRUE, prob = c(0.3, 0.7)),
    treatment = sample(c("Control", "Treatment"), n, replace = TRUE),
    age_group = sample(c("Young", "Middle", "Old"), n, replace = TRUE),
    risk_factor = sample(c("Low", "Medium", "High"), n, replace = TRUE),
    biomarker1 = rnorm(n, 0, 1),
    biomarker2 = rnorm(n, 5, 2),
    stage = factor(sample(c("I", "II", "III", "IV"), n, replace = TRUE)),
    performance_status = sample(0:2, n, replace = TRUE)
  )
}

#' Create test data with special character challenges
#' @param n Number of rows (default: 25)
#' @param seed Random seed for reproducibility (default: 987)
#' @return tibble with challenging column names and values
create_special_chars_test <- function(n = 25, seed = 987) {
  set.seed(seed)
  data <- tibble::tibble(
    id = 1:n,
    `var with spaces` = rnorm(n),
    `var.with.dots` = rnorm(n),
    `var-with-dashes` = sample(c("A", "B"), n, replace = TRUE),
    var_with_underscores = sample(c("X", "Y", "Z"), n, replace = TRUE),
    `123numeric_start` = rnorm(n),
    normal_var = sample(1:10, n, replace = TRUE)
  )
  
  # Add special characters in data values
  data$special_chars <- sample(
    c("value_with_underscore", "value-with-dash", "value with space", "value.with.dot"),
    n, replace = TRUE
  )
  
  data
}

# Test Data Validation Functions ====

#' Validate test data structure
#' @param data Test data to validate
#' @param expected_cols Expected column names
#' @param min_rows Minimum number of rows required
#' @return TRUE if valid, throws error otherwise
validate_test_data <- function(data, expected_cols = NULL, min_rows = 1) {
  # Check basic structure
  if (!is.data.frame(data)) {
    stop("Test data must be a data frame")
  }
  
  if (nrow(data) < min_rows) {
    stop(paste("Test data must have at least", min_rows, "rows"))
  }
  
  # Check expected columns if specified
  if (!is.null(expected_cols)) {
    missing_cols <- setdiff(expected_cols, names(data))
    if (length(missing_cols) > 0) {
      stop(paste("Missing expected columns:", paste(missing_cols, collapse = ", ")))
    }
  }
  
  TRUE
}

#' Check for data quality issues
#' @param data Test data to check
#' @return list of data quality metrics
check_data_quality <- function(data) {
  list(
    n_rows = nrow(data),
    n_cols = ncol(data),
    missing_values = sum(is.na(data)),
    complete_cases = sum(complete.cases(data)),
    numeric_cols = sum(sapply(data, is.numeric)),
    character_cols = sum(sapply(data, is.character)),
    factor_cols = sum(sapply(data, is.factor)),
    logical_cols = sum(sapply(data, is.logical))
  )
}

# Commonly Used Test Datasets ====

# Pre-generate commonly used datasets to avoid repeated computation
SMALL_TEST_DATA <- create_small_test_data()
MEDIUM_TEST_DATA <- create_medium_test_data()
SURVIVAL_TEST_DATA <- create_survival_test_data()
MISSING_DATA_TEST <- create_missing_data_test()
EXTREME_VALUES_TEST <- create_extreme_values_test()
SPECIAL_CHARS_TEST <- create_special_chars_test()

# Export datasets for use in tests
list(
  small = SMALL_TEST_DATA,
  medium = MEDIUM_TEST_DATA,
  survival = SURVIVAL_TEST_DATA,
  missing = MISSING_DATA_TEST,
  extreme = EXTREME_VALUES_TEST,
  special_chars = SPECIAL_CHARS_TEST
)