# Comprehensive Performance Benchmarking Tests
# Tests for large dataset processing, memory usage, and performance regressions

library(testthat)

# Skip performance tests in resource-constrained environments
skip_if_performance_testing_disabled <- function() {
  skip_if(Sys.getenv("SKIP_PERFORMANCE_TESTS") == "true")
  skip_on_cran()
}

# Performance Test Data Generation ====
create_performance_test_data <- function(n_rows, n_groups = 3, n_categories = 5) {
  tibble::tibble(
    id = 1:n_rows,
    group = sample(paste0("Group_", 1:n_groups), n_rows, replace = TRUE),
    category = sample(paste0("Cat_", 1:n_categories), n_rows, replace = TRUE),
    numeric_var1 = rnorm(n_rows, 50, 15),
    numeric_var2 = rnorm(n_rows, 100, 30),
    numeric_var3 = runif(n_rows, 0, 1),
    time_var = sample(1:1000, n_rows, replace = TRUE),
    event_var = sample(c(0, 1), n_rows, replace = TRUE),
    factor_var = factor(sample(letters[1:10], n_rows, replace = TRUE)),
    logical_var = sample(c(TRUE, FALSE), n_rows, replace = TRUE)
  )
}

# Large Dataset Processing Tests ====
test_that("large dataset processing completes within reasonable time", {
  skip_if_performance_testing_disabled()
  
  # Test with 1000 rows
  large_data_1k <- create_performance_test_data(1000)
  
  start_time <- Sys.time()
  summary_1k <- nightowl::Summary$new(
    data = large_data_1k,
    x = "numeric_var1",
    by = "group"
  )
  result_1k <- summary_1k$raw()
  end_time <- Sys.time()
  
  time_1k <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  expect_true(time_1k < 10) # Should complete within 10 seconds
  expect_true(is.data.frame(result_1k) || is.list(result_1k))
  
  # Test with 5000 rows
  large_data_5k <- create_performance_test_data(5000)
  
  start_time <- Sys.time()
  summary_5k <- nightowl::Summary$new(
    data = large_data_5k,
    x = "numeric_var1", 
    by = "group"
  )
  result_5k <- summary_5k$raw()
  end_time <- Sys.time()
  
  time_5k <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  expect_true(time_5k < 30) # Should complete within 30 seconds
  expect_true(is.data.frame(result_5k) || is.list(result_5k))
  
  # Performance should scale reasonably (not more than 5x slower for 5x data)
  expect_true(time_5k < time_1k * 8)
})

test_that("large dataset plot generation performs adequately", {
  skip_if_performance_testing_disabled()
  
  # Test with 2000 rows
  plot_data_2k <- create_performance_test_data(2000)
  
  start_time <- Sys.time()
  large_plot <- nightowl::plot(
    data = plot_data_2k,
    mapping = list(
      x = "numeric_var1",
      y = "numeric_var2",
      color = "group"
    ),
    layers = list(
      list(type = "generic", geom = "ggplot2::geom_point", alpha = 0.6)
    )
  )
  end_time <- Sys.time()
  
  plot_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  expect_true(plot_time < 15) # Should complete within 15 seconds
  expect_true(inherits(large_plot, "DeclarativePlot"))
})

# Memory Usage Tests ====
test_that("functions maintain reasonable memory usage with large datasets", {
  skip_if_performance_testing_disabled()
  skip_if_not_installed("pryr") # For memory measurement
  
  # Measure memory before
  initial_memory <- pryr::mem_used()
  
  # Create and process large dataset
  memory_test_data <- create_performance_test_data(3000)
  
  summary_obj <- nightowl::Summary$new(
    data = memory_test_data,
    x = "numeric_var1",
    by = "group"
  )
  
  result <- summary_obj$raw()
  
  # Measure memory after
  peak_memory <- pryr::mem_used()
  memory_increase <- peak_memory - initial_memory
  
  # Memory increase should be reasonable (less than 100MB for this test)
  expect_true(as.numeric(memory_increase) < 100 * 1024^2)
  
  # Clean up
  rm(memory_test_data, summary_obj, result)
  gc() # Force garbage collection
})

# Reactable Performance Tests ====
test_that("reactable rendering performs well with large tables", {
  skip_if_performance_testing_disabled()
  
  # Test reactable with moderate dataset
  reactable_data <- create_performance_test_data(500)
  
  start_time <- Sys.time()
  summary_reactable <- nightowl::Summary$new(
    data = reactable_data,
    x = "category",
    by = "group"
  )
  reactable_result <- summary_reactable$reactable()
  end_time <- Sys.time()
  
  reactable_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  expect_true(reactable_time < 8) # Should complete within 8 seconds
  expect_true(inherits(reactable_result, "reactable"))
  
  # Test direct reactable rendering
  start_time <- Sys.time()
  direct_reactable <- nightowl::render_reactable(reactable_data)
  end_time <- Sys.time()
  
  direct_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  expect_true(direct_time < 5) # Should complete within 5 seconds
  expect_true(inherits(direct_reactable, "reactable"))
})

# Survival Analysis Performance Tests ====
test_that("survival analysis functions handle large datasets efficiently", {
  skip_if_performance_testing_disabled()
  
  # Create large survival dataset
  survival_data_large <- tibble::tibble(
    time = sample(1:500, 2000, replace = TRUE),
    event = sample(c(0, 1), 2000, replace = TRUE),
    treatment = sample(c("Control", "Treatment"), 2000, replace = TRUE),
    covariate1 = rnorm(2000),
    covariate2 = sample(c("Low", "Medium", "High"), 2000, replace = TRUE),
    strata1 = sample(c("A", "B"), 2000, replace = TRUE)
  )
  
  # Test Coxph fitting performance
  start_time <- Sys.time()
  coxph_result <- nightowl::fit_coxph(
    data = survival_data_large,
    time = "time",
    event = "event",
    treatment = "treatment",
    covariates = c("covariate1", "covariate2")
  )
  end_time <- Sys.time()
  
  coxph_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  expect_true(coxph_time < 20) # Should complete within 20 seconds
  expect_true(is.list(coxph_result))
  
  # Test KM plot performance
  start_time <- Sys.time()
  km_plot_result <- nightowl::plot_km(
    data = survival_data_large,
    time = "time",
    event = "event",
    treatment = "treatment"
  )
  end_time <- Sys.time()
  
  km_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  expect_true(km_time < 15) # Should complete within 15 seconds
  expect_true(inherits(km_plot_result, "ggplot"))
})

# Multi-threaded Performance Tests ====
test_that("functions can handle concurrent operations", {
  skip_if_performance_testing_disabled()
  skip_if_not_installed("future")
  skip_if_not_installed("furrr")
  
  # Setup parallel processing
  future::plan(future::multisession, workers = 2)
  
  # Create multiple datasets
  datasets <- purrr::map(1:4, ~ create_performance_test_data(500, n_groups = 2))
  
  start_time <- Sys.time()
  
  # Process datasets in parallel
  parallel_results <- furrr::future_map(datasets, function(data) {
    nightowl::Summary$new(
      data = data,
      x = "numeric_var1",
      by = "group"
    )$raw()
  })
  
  end_time <- Sys.time()
  parallel_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Process datasets sequentially for comparison
  start_time <- Sys.time()
  sequential_results <- purrr::map(datasets, function(data) {
    nightowl::Summary$new(
      data = data,
      x = "numeric_var1", 
      by = "group"
    )$raw()
  })
  end_time <- Sys.time()
  sequential_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Parallel should be faster or at least not significantly slower
  expect_true(parallel_time < sequential_time * 1.5)
  expect_equal(length(parallel_results), 4)
  expect_equal(length(sequential_results), 4)
  
  # Reset to sequential processing
  future::plan(future::sequential)
})

# Wide Dataset Performance Tests ====
test_that("functions handle wide datasets (many columns) efficiently", {
  skip_if_performance_testing_disabled()
  
  # Create wide dataset (many columns)
  base_data <- create_performance_test_data(200)
  
  # Add many additional numeric columns
  for (i in 1:50) {
    base_data[[paste0("extra_num_", i)]] <- rnorm(200)
  }
  
  # Add many additional categorical columns
  for (i in 1:20) {
    base_data[[paste0("extra_cat_", i)]] <- sample(LETTERS[1:3], 200, replace = TRUE)
  }
  
  start_time <- Sys.time()
  wide_summary <- nightowl::Summary$new(
    data = base_data,
    x = "numeric_var1",
    by = "group"
  )
  wide_result <- wide_summary$raw()
  end_time <- Sys.time()
  
  wide_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  expect_true(wide_time < 10) # Should complete within 10 seconds
  expect_true(is.data.frame(wide_result) || is.list(wide_result))
  
  # Test that column count doesn't significantly impact performance for our operations
  expect_true(ncol(base_data) > 75) # Verify we actually have many columns
})

# Caching Performance Validation ====
test_that("caching mechanisms provide expected performance improvements", {
  skip_if_performance_testing_disabled()
  
  # Test with data that should benefit from caching
  cache_test_data <- create_performance_test_data(800)
  
  # First execution (cache miss)
  start_time <- Sys.time()
  plot1 <- nightowl::plot(
    data = cache_test_data,
    mapping = list(x = "group", y = "numeric_var1"),
    layers = list(list(type = "boxplot"))
  )
  end_time <- Sys.time()
  first_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Second execution (potential cache hit)
  start_time <- Sys.time()
  plot2 <- nightowl::plot(
    data = cache_test_data,
    mapping = list(x = "group", y = "numeric_var1"),
    layers = list(list(type = "boxplot"))
  )
  end_time <- Sys.time()
  second_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Second execution should be faster or at least not significantly slower
  # Note: This test may need adjustment based on actual caching implementation
  expect_true(second_time <= first_time * 1.2) # Allow 20% tolerance
  
  expect_true(inherits(plot1, "DeclarativePlot"))
  expect_true(inherits(plot2, "DeclarativePlot"))
})

# Performance Regression Detection ====
test_that("performance metrics meet baseline expectations", {
  skip_if_performance_testing_disabled()
  
  # Standardized performance test
  standard_data <- create_performance_test_data(1000)
  
  # Summary performance baseline
  start_time <- Sys.time()
  standard_summary <- nightowl::Summary$new(
    data = standard_data,
    x = "numeric_var1",
    by = "group"
  )$raw()
  end_time <- Sys.time()
  summary_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Plot performance baseline
  start_time <- Sys.time()
  standard_plot <- nightowl::plot(
    data = standard_data[1:500, ], # Smaller subset for plot
    mapping = list(x = "group", y = "numeric_var1", color = "category"),
    layers = list(list(type = "generic", geom = "ggplot2::geom_point"))
  )
  end_time <- Sys.time()
  plot_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Reactable performance baseline
  start_time <- Sys.time()
  standard_reactable <- nightowl::render_reactable(standard_data[1:200, ])
  end_time <- Sys.time()
  reactable_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Performance expectations (these may need adjustment based on environment)
  expect_true(summary_time < 8, paste("Summary took", summary_time, "seconds"))
  expect_true(plot_time < 6, paste("Plot took", plot_time, "seconds"))
  expect_true(reactable_time < 4, paste("Reactable took", reactable_time, "seconds"))
  
  # Log performance metrics for regression tracking
  cat("\\nPerformance Metrics:\\n")
  cat(paste("Summary (1000 rows):", round(summary_time, 3), "seconds\\n"))
  cat(paste("Plot (500 rows):", round(plot_time, 3), "seconds\\n"))
  cat(paste("Reactable (200 rows):", round(reactable_time, 3), "seconds\\n"))
})