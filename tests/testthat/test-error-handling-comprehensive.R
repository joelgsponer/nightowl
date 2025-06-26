# Comprehensive Error Handling and Edge Case Tests
# Extends security tests with broader input validation and error scenarios

library(testthat)

# Test Data Setup ====
create_test_data <- function(n = 50) {
  tibble::tibble(
    id = 1:n,
    numeric_var = rnorm(n),
    categorical_var = sample(c("A", "B", "C"), n, replace = TRUE),
    time_var = sample(1:100, n, replace = TRUE),
    event_var = sample(c(0, 1), n, replace = TRUE),
    missing_data = c(rep(NA, n/2), rnorm(n/2)),
    extreme_values = c(rep(Inf, 2), rep(-Inf, 2), rnorm(n-4)),
    special_chars = paste0("var_", sample(letters, n, replace = TRUE))
  )
}

# Data Input Validation Tests ====
test_that("functions handle invalid data types gracefully", {
  # NULL data
  expect_error(
    nightowl::Summary$new(data = NULL, column = "var", group_by = "group"),
    ".*data.*"
  )
  
  # Non-data.frame input
  expect_error(
    nightowl::Summary$new(data = list(a = 1, b = 2), column = "a", group_by = "b"),
    ".*data.*"
  )
  
  # Empty data frame
  empty_data <- data.frame()
  expect_error(
    nightowl::Summary$new(data = empty_data, column = "var", group_by = "group"),
    ".*empty.*|.*column.*"
  )
  
  # Single row data frame
  single_row <- data.frame(x = 1, y = "A")
  expect_warning(
    nightowl::Summary$new(data = single_row, column = "x", group_by = "y"),
    ".*sample size.*|.*row.*"
  )
})

test_that("functions validate column names and handle non-existent columns", {
  test_data <- create_test_data(20)
  
  # Non-existent column names
  expect_error(
    nightowl::Summary$new(data = test_data, column = "nonexistent", group_by = "categorical_var"),
    ".*column.*|.*variable.*"
  )
  
  expect_error(
    nightowl::Summary$new(data = test_data, column = "numeric_var", group_by = "nonexistent"),
    ".*column.*|.*variable.*"
  )
  
  # Empty string column names
  expect_error(
    nightowl::Summary$new(data = test_data, column = "", group_by = "categorical_var"),
    ".*column.*|.*name.*"
  )
  
  # NULL column names
  expect_error(
    nightowl::Summary$new(data = test_data, column = NULL, group_by = "categorical_var"),
    ".*column.*|.*name.*"
  )
})

test_that("functions handle special column names correctly", {
  test_data <- create_test_data(20)
  
  # Column names with spaces, dots, special characters
  special_data <- test_data
  names(special_data)[1:4] <- c("var with spaces", "var.with.dots", "var-with-dashes", "var_with_underscores")
  
  # These should work or give informative errors
  expect_no_error(
    nightowl::Summary$new(
      data = special_data, 
      column = "var_with_underscores", 
      group_by = "categorical_var"
    )
  )
  
  # Test backtick-quoted names if supported
  if ("var with spaces" %in% names(special_data)) {
    expect_no_error(
      nightowl::Summary$new(
        data = special_data, 
        column = "`var with spaces`", 
        group_by = "categorical_var"
      )
    )
  }
})

# Missing Data and Edge Cases ====
test_that("functions handle missing data appropriately", {
  test_data <- create_test_data(30)
  
  # All missing data in x variable
  all_missing_data <- test_data
  all_missing_data$numeric_var <- NA_real_
  
  expect_warning(
    nightowl::Summary$new(
      data = all_missing_data, 
      column = "numeric_var", 
      group_by = "categorical_var"
    ),
    ".*missing.*|.*NA.*"
  )
  
  # All missing data in by variable
  all_missing_by <- test_data
  all_missing_by$categorical_var <- NA_character_
  
  expect_warning(
    nightowl::Summary$new(
      data = all_missing_by, 
      column = "numeric_var", 
      group_by = "categorical_var"
    ),
    ".*missing.*|.*NA.*"
  )
  
  # Mixed missing data - should handle gracefully
  mixed_missing <- test_data
  mixed_missing$numeric_var[1:10] <- NA
  mixed_missing$categorical_var[11:15] <- NA
  
  expect_no_error(
    nightowl::Summary$new(
      data = mixed_missing, 
      column = "numeric_var", 
      group_by = "categorical_var"
    )
  )
})

test_that("functions handle extreme numeric values correctly", {
  test_data <- create_test_data(30)
  
  # Infinite values
  inf_data <- test_data
  inf_data$extreme_values[1:5] <- Inf
  inf_data$extreme_values[6:10] <- -Inf
  
  expect_warning(
    nightowl::Summary$new(
      data = inf_data, 
      column = "extreme_values", 
      group_by = "categorical_var"
    ),
    ".*infinite.*|.*Inf.*"
  )
  
  # Very large numbers
  large_data <- test_data
  large_data$numeric_var <- c(1e10, -1e10, rnorm(28))
  
  expect_no_error(
    nightowl::Summary$new(
      data = large_data, 
      column = "numeric_var", 
      group_by = "categorical_var"
    )
  )
  
  # Very small numbers (near zero)
  small_data <- test_data
  small_data$numeric_var <- c(1e-10, -1e-10, rnorm(28))
  
  expect_no_error(
    nightowl::Summary$new(
      data = small_data, 
      column = "numeric_var", 
      group_by = "categorical_var"
    )
  )
})

# Type Mismatch and Coercion Tests ====
test_that("functions handle type mismatches appropriately", {
  test_data <- create_test_data(25)
  
  # Numeric variable treated as categorical
  expect_no_error(
    nightowl::Summary$new(
      data = test_data, 
      column = "numeric_var", 
      group_by = "categorical_var",
      method = nightowl::summarise_categorical
    )
  )
  
  # Character variable in numeric context
  char_as_numeric <- test_data
  char_as_numeric$supposed_numeric <- c("1", "2", "not_a_number", "4", rep("5", 21))
  
  expect_error(
    nightowl::Summary$new(
      data = char_as_numeric, 
      column = "supposed_numeric", 
      group_by = "categorical_var",
      method = nightowl::summarise_numeric_pointrange
    ),
    ".*numeric.*|.*type.*"
  )
  
  # Factor with many levels
  many_levels <- test_data
  many_levels$many_factor <- factor(sample(letters[1:20], 25, replace = TRUE))
  
  expect_warning(
    nightowl::Summary$new(
      data = many_levels, 
      column = "many_factor", 
      group_by = "categorical_var"
    ),
    ".*levels.*|.*factor.*"
  )
})

# Plot Generation Error Handling ====
test_that("plot functions handle invalid plot specifications", {
  test_data <- create_test_data(20)
  
  # Invalid geom specification
  expect_error(
    nightowl::plot(
      data = test_data,
      mapping = list(x = "numeric_var", y = "categorical_var"),
      layers = list(list(type = "generic", geom = "nonexistent::geom_invalid"))
    ),
    ".*geom.*|.*function.*"
  )
  
  # Conflicting aesthetic mappings
  expect_error(
    nightowl::plot(
      data = test_data,
      mapping = list(x = "numeric_var", y = "numeric_var", x = "categorical_var"), # duplicate x
      layers = list(list(type = "generic", geom = "ggplot2::geom_point"))
    ),
    ".*mapping.*|.*aesthetic.*"
  )
  
  # Invalid layer type
  expect_error(
    nightowl::plot(
      data = test_data,
      mapping = list(x = "numeric_var", y = "categorical_var"),
      layers = list(list(type = "invalid_type", geom = "ggplot2::geom_point"))
    ),
    ".*layer.*|.*type.*"
  )
})

test_that("SVG and HTML generation handles edge cases", {
  # Very large plot dimensions
  p <- ggplot2::ggplot(create_test_data(10), ggplot2::aes(x = numeric_var, y = categorical_var)) +
    ggplot2::geom_point()
  
  expect_warning(
    nightowl::Plot$new(plot = p, options_svg = list(width = 1000, height = 1000)),
    ".*size.*|.*dimension.*"
  )
  
  # Zero or negative dimensions
  expect_error(
    nightowl::Plot$new(plot = p, options_svg = list(width = 0, height = 5)),
    ".*width.*|.*positive.*"
  )
  
  expect_error(
    nightowl::Plot$new(plot = p, options_svg = list(width = 5, height = -1)),
    ".*height.*|.*positive.*"
  )
  
  # Invalid scaling values
  expect_error(
    nightowl::Plot$new(plot = p, options_svg = list(scaling = 0)),
    ".*scaling.*|.*positive.*"
  )
  
  expect_error(
    nightowl::Plot$new(plot = p, options_svg = list(scaling = "invalid")),
    ".*scaling.*|.*numeric.*"
  )
})

# Survival Analysis Edge Cases ====
test_that("survival functions handle edge cases in data", {
  # Time values with zeros
  survival_data_zeros <- tibble::tibble(
    time = c(0, 0, 1:8),
    event = sample(c(0, 1), 10, replace = TRUE),
    treatment = sample(c("A", "B"), 10, replace = TRUE)
  )
  
  expect_warning(
    nightowl::fit_coxph(
      data = survival_data_zeros,
      time = "time",
      event = "event",
      treatment = "treatment"
    ),
    ".*time.*|.*zero.*"
  )
  
  # Negative time values
  survival_data_negative <- tibble::tibble(
    time = c(-1, -2, 1:8),
    event = sample(c(0, 1), 10, replace = TRUE),
    treatment = sample(c("A", "B"), 10, replace = TRUE)
  )
  
  expect_error(
    nightowl::fit_coxph(
      data = survival_data_negative,
      time = "time",
      event = "event",
      treatment = "treatment"
    ),
    ".*time.*|.*negative.*"
  )
  
  # All events censored (no events)
  survival_no_events <- tibble::tibble(
    time = 1:20,
    event = rep(0, 20), # all censored
    treatment = sample(c("A", "B"), 20, replace = TRUE)
  )
  
  expect_warning(
    nightowl::fit_coxph(
      data = survival_no_events,
      time = "time",
      event = "event",
      treatment = "treatment"
    ),
    ".*event.*|.*censored.*"
  )
  
  # Single group in treatment
  survival_single_group <- tibble::tibble(
    time = 1:20,
    event = sample(c(0, 1), 20, replace = TRUE),
    treatment = rep("A", 20) # only one treatment group
  )
  
  expect_warning(
    nightowl::fit_coxph(
      data = survival_single_group,
      time = "time",
      event = "event",
      treatment = "treatment"
    ),
    ".*group.*|.*variation.*"
  )
})

# Options and Configuration Error Handling ====
test_that("configuration functions validate settings appropriately", {
  options <- get_nightowl_options()
  
  # Invalid color specifications
  expect_error(
    options$set_colors(c("not_a_color", "also_not_a_color")),
    ".*color.*|.*valid.*"
  )
  
  expect_error(
    options$set_colors(c("#GGGGGG")), # invalid hex
    ".*color.*|.*hex.*"
  )
  
  # Out of range values
  expect_error(
    options$set_header_width(-5),
    ".*width.*|.*positive.*"
  )
  
  expect_error(
    options$set_header_width(1000),
    ".*width.*|.*reasonable.*"
  )
})

# Memory and Performance Edge Cases ====
test_that("functions handle large datasets appropriately", {
  # Skip this test if in limited memory environment
  skip_if(Sys.getenv("SKIP_MEMORY_TESTS") == "true")
  
  # Large dataset
  large_data <- create_test_data(10000)
  
  # Should complete but may issue warnings about performance
  start_time <- Sys.time()
  result <- nightowl::Summary$new(
    data = large_data, 
    column = "numeric_var", 
    group_by = "categorical_var"
  )
  end_time <- Sys.time()
  
  execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Should complete within reasonable time (30 seconds)
  expect_true(execution_time < 30)
  expect_true(inherits(result, "Summary"))
})

test_that("functions handle datasets with many columns gracefully", {
  # Wide dataset
  wide_data <- create_test_data(50)
  
  # Add many additional columns
  for (i in 1:100) {
    wide_data[[paste0("extra_col_", i)]] <- rnorm(50)
  }
  
  # Should handle wide data without issues
  expect_no_error(
    nightowl::Summary$new(
      data = wide_data, 
      column = "numeric_var", 
      group_by = "categorical_var"
    )
  )
})

# Integration Error Handling ====
test_that("multiple function calls handle error propagation correctly", {
  test_data <- create_test_data(30)
  
  # Chain of operations that should fail gracefully
  expect_error({
    summary_obj <- nightowl::Summary$new(
      data = test_data, 
      column = "nonexistent_var", 
      group_by = "categorical_var"
    )
    summary_obj$raw()
    summary_obj$kable()
  }, ".*column.*|.*variable.*")
  
  # Partial failure scenarios
  summary_obj <- nightowl::Summary$new(
    data = test_data, 
    column = "numeric_var", 
    group_by = "categorical_var"
  )
  
  # Adding invalid calculation should not break existing functionality
  expect_error(
    summary_obj$add_calculation(list("invalid" = "not_a_function")),
    ".*function.*|.*calculation.*"
  )
  
  # But existing methods should still work
  expect_no_error(summary_obj$raw())
})