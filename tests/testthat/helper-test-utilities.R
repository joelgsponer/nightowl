# Test Utility Functions
# Helper functions for common testing patterns and assertions

library(testthat)

# R6 Class Testing Utilities ====

#' Test R6 class constructor with various inputs
#' @param class_constructor R6 class constructor function
#' @param valid_args List of valid arguments for constructor
#' @param invalid_args List of invalid arguments that should cause errors
#' @param class_name Expected class name for inheritance checking
test_r6_constructor <- function(class_constructor, valid_args, invalid_args = NULL, class_name = NULL) {
  # Test valid construction
  obj <- do.call(class_constructor, valid_args)
  expect_true(inherits(obj, "R6"))
  
  if (!is.null(class_name)) {
    expect_true(inherits(obj, class_name))
  }
  
  # Test invalid constructions
  if (!is.null(invalid_args)) {
    for (invalid_arg_set in invalid_args) {
      expect_error(
        do.call(class_constructor, invalid_arg_set),
        info = paste("Invalid args:", deparse(substitute(invalid_arg_set)))
      )
    }
  }
  
  obj
}

#' Test R6 method with input validation
#' @param obj R6 object instance
#' @param method_name Name of method to test
#' @param valid_inputs List of valid inputs for the method
#' @param invalid_inputs List of invalid inputs that should cause errors
#' @param expected_output_class Expected class of output (optional)
test_r6_method <- function(obj, method_name, valid_inputs = list(), invalid_inputs = NULL, expected_output_class = NULL) {
  # Test valid method calls
  for (valid_input in valid_inputs) {
    result <- do.call(obj[[method_name]], valid_input)
    
    if (!is.null(expected_output_class)) {
      expect_true(inherits(result, expected_output_class))
    }
  }
  
  # Test invalid method calls
  if (!is.null(invalid_inputs)) {
    for (invalid_input in invalid_inputs) {
      expect_error(
        do.call(obj[[method_name]], invalid_input),
        info = paste("Invalid input:", deparse(substitute(invalid_input)))
      )
    }
  }
}

# Plot Testing Utilities ====

#' Assert that a ggplot object is valid and renderable
#' @param plot_obj ggplot object to validate
#' @param min_layers Minimum number of layers expected (default: 1)
#' @param expected_aesthetics Expected aesthetic mappings (optional)
assert_valid_ggplot <- function(plot_obj, min_layers = 1, expected_aesthetics = NULL) {
  expect_true(inherits(plot_obj, "ggplot"))
  expect_true(length(plot_obj$layers) >= min_layers)
  
  if (!is.null(expected_aesthetics)) {
    plot_aesthetics <- names(plot_obj$mapping)
    missing_aesthetics <- setdiff(expected_aesthetics, plot_aesthetics)
    expect_true(
      length(missing_aesthetics) == 0,
      info = paste("Missing aesthetics:", paste(missing_aesthetics, collapse = ", "))
    )
  }
  
  # Test that plot can be built without errors
  expect_no_error(ggplot2::ggplot_build(plot_obj))
}

#' Test plot output consistency across different data scenarios
#' @param plot_function Function that creates plots
#' @param test_datasets List of datasets to test with
#' @param plot_args Additional arguments for plot function
test_plot_consistency <- function(plot_function, test_datasets, plot_args = list()) {
  plots <- list()
  
  for (i in seq_along(test_datasets)) {
    dataset_name <- names(test_datasets)[i]
    dataset <- test_datasets[[i]]
    
    args <- c(list(data = dataset), plot_args)
    plot_obj <- do.call(plot_function, args)
    
    assert_valid_ggplot(plot_obj)
    plots[[dataset_name]] <- plot_obj
  }
  
  plots
}

# Data Validation Utilities ====

#' Assert data frame has expected structure
#' @param df Data frame to validate
#' @param expected_cols Expected column names
#' @param min_rows Minimum number of rows
#' @param col_types Expected column types (named list)
assert_data_structure <- function(df, expected_cols = NULL, min_rows = 1, col_types = NULL) {
  expect_true(is.data.frame(df))
  expect_true(nrow(df) >= min_rows)
  
  if (!is.null(expected_cols)) {
    expect_true(all(expected_cols %in% names(df)))
  }
  
  if (!is.null(col_types)) {
    for (col_name in names(col_types)) {
      expected_type <- col_types[[col_name]]
      actual_type <- class(df[[col_name]])[1]
      
      expect_true(
        actual_type == expected_type || inherits(df[[col_name]], expected_type),
        info = paste("Column", col_name, "expected type", expected_type, "but got", actual_type)
      )
    }
  }
}

#' Check for appropriate handling of missing data
#' @param result_with_missing Result when function processes data with missing values
#' @param result_without_missing Result when function processes complete data
#' @param tolerance_factor How much worse performance is acceptable with missing data
assert_missing_data_handling <- function(result_with_missing, result_without_missing, tolerance_factor = 2) {
  # Both results should be valid
  expect_true(!is.null(result_with_missing))
  expect_true(!is.null(result_without_missing))
  
  # Missing data handling should not completely break functionality
  expect_true(inherits(result_with_missing, class(result_without_missing)))
  
  # If results are data frames, check structure is similar
  if (is.data.frame(result_with_missing) && is.data.frame(result_without_missing)) {
    expect_true(ncol(result_with_missing) == ncol(result_without_missing))
    expect_true(names(result_with_missing) == names(result_without_missing))
  }
}

# Performance Testing Utilities ====

#' Measure execution time of a function
#' @param expr Expression to time
#' @param n_reps Number of repetitions for averaging (default: 3)
#' @return Average execution time in seconds
time_execution <- function(expr, n_reps = 3) {
  times <- numeric(n_reps)
  
  for (i in seq_len(n_reps)) {
    start_time <- Sys.time()
    eval(expr)
    end_time <- Sys.time()
    times[i] <- as.numeric(difftime(end_time, start_time, units = "secs"))
  }
  
  mean(times)
}

#' Assert execution time is within acceptable bounds
#' @param expr Expression to evaluate
#' @param max_time Maximum acceptable time in seconds
#' @param n_reps Number of repetitions for averaging
assert_execution_time <- function(expr, max_time, n_reps = 3) {
  avg_time <- time_execution(substitute(expr), n_reps)
  expect_true(
    avg_time <= max_time,
    info = paste("Execution took", round(avg_time, 3), "seconds, expected <=", max_time)
  )
  avg_time
}

#' Compare performance between two approaches
#' @param expr1 First expression to compare
#' @param expr2 Second expression to compare
#' @param improvement_factor Expected improvement factor (expr1 should be this much faster)
#' @param n_reps Number of repetitions for averaging
assert_performance_improvement <- function(expr1, expr2, improvement_factor, n_reps = 3) {
  time1 <- time_execution(substitute(expr1), n_reps)
  time2 <- time_execution(substitute(expr2), n_reps)
  
  actual_improvement <- time2 / time1
  
  expect_true(
    actual_improvement >= improvement_factor,
    info = paste("Expected", improvement_factor, "x improvement, got", round(actual_improvement, 2), "x")
  )
  
  list(time1 = time1, time2 = time2, improvement = actual_improvement)
}

# Error Testing Utilities ====

#' Test that a function properly validates its inputs
#' @param func Function to test
#' @param valid_args List of valid argument sets
#' @param invalid_args List of invalid argument sets with expected error patterns
test_input_validation <- function(func, valid_args, invalid_args) {
  # Test valid inputs work
  for (valid_arg_set in valid_args) {
    expect_no_error(do.call(func, valid_arg_set))
  }
  
  # Test invalid inputs fail appropriately
  for (invalid_case in invalid_args) {
    args <- invalid_case$args
    expected_error <- invalid_case$error_pattern
    
    expect_error(
      do.call(func, args),
      regexp = expected_error,
      info = paste("Args:", deparse(args))
    )
  }
}

#' Assert that warnings are produced when expected
#' @param expr Expression that should produce warnings
#' @param warning_pattern Regex pattern for expected warning message
assert_expected_warning <- function(expr, warning_pattern = NULL) {
  if (is.null(warning_pattern)) {
    expect_warning(expr)
  } else {
    expect_warning(expr, regexp = warning_pattern)
  }
}

# Visual Testing Utilities ====

#' Skip visual tests if vdiffr is not available
skip_if_no_vdiffr <- function() {
  skip_if_not_installed("vdiffr")
  skip_on_cran()
}

#' Test visual consistency with automatic plot naming
#' @param plot_obj ggplot object to test
#' @param test_name Base name for the test
#' @param ... Additional arguments passed to vdiffr::expect_doppelganger
test_visual_consistency <- function(plot_obj, test_name, ...) {
  skip_if_no_vdiffr()
  
  # Generate a unique test name based on calling context
  call_info <- sys.call(-1)
  if (!is.null(call_info)) {
    context_name <- deparse(call_info[[1]])
    full_test_name <- paste(context_name, test_name, sep = "_")
  } else {
    full_test_name <- test_name
  }
  
  vdiffr::expect_doppelganger(full_test_name, plot_obj, ...)
}

# Memory Testing Utilities ====

#' Monitor memory usage during expression execution
#' @param expr Expression to monitor
#' @return List with initial memory, peak memory, and memory increase
monitor_memory_usage <- function(expr) {
  skip_if_not_installed("pryr")
  
  initial_memory <- pryr::mem_used()
  eval(expr)
  final_memory <- pryr::mem_used()
  
  list(
    initial = initial_memory,
    final = final_memory,
    increase = final_memory - initial_memory
  )
}

#' Assert memory usage is within acceptable bounds
#' @param expr Expression to evaluate
#' @param max_memory_mb Maximum acceptable memory increase in MB
assert_memory_usage <- function(expr, max_memory_mb) {
  memory_info <- monitor_memory_usage(substitute(expr))
  memory_increase_mb <- as.numeric(memory_info$increase) / (1024^2)
  
  expect_true(
    memory_increase_mb <= max_memory_mb,
    info = paste("Memory increased by", round(memory_increase_mb, 2), "MB, expected <=", max_memory_mb, "MB")
  )
  
  memory_increase_mb
}

# Test Suite Organization Utilities ====

#' Run a test suite with common setup and teardown
#' @param suite_name Name of the test suite
#' @param setup_func Function to run before tests (optional)
#' @param teardown_func Function to run after tests (optional)
#' @param test_func Function containing the actual tests
run_test_suite <- function(suite_name, test_func, setup_func = NULL, teardown_func = NULL) {
  context(suite_name)
  
  if (!is.null(setup_func)) {
    setup_func()
  }
  
  test_func()
  
  if (!is.null(teardown_func)) {
    teardown_func()
  }
}

#' Create test context with standardized data
#' @param context_name Name for the test context
#' @param data_type Type of test data to use ("small", "medium", "large", etc.)
#' @return Test data for the context
create_test_context <- function(context_name, data_type = "medium") {
  context(context_name)
  
  # Load appropriate test data
  test_data <- switch(data_type,
    "small" = create_small_test_data(),
    "medium" = create_medium_test_data(), 
    "large" = create_large_test_data(),
    "survival" = create_survival_test_data(),
    "missing" = create_missing_data_test(),
    "extreme" = create_extreme_values_test(),
    "special_chars" = create_special_chars_test(),
    stop("Unknown data type: ", data_type)
  )
  
  # Validate test data
  validate_test_data(test_data, min_rows = 5)
  
  test_data
}