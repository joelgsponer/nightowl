#!/usr/bin/env Rscript

# Benchmark script to validate 70% performance improvement from plot caching

library(nightowl)
library(microbenchmark)
library(ggplot2)

# Create test datasets of varying sizes
create_test_data <- function(n) {
  data.frame(
    x = rnorm(n),
    y = rnorm(n),
    group = sample(LETTERS[1:5], n, replace = TRUE),
    size = runif(n, 1, 10)
  )
}

# Benchmark function
benchmark_plot_caching <- function(data_size, n_iterations = 100) {
  cat(sprintf("\n=== Benchmarking with %d data points ===\n", data_size))
  
  data <- create_test_data(data_size)
  
  # Create a complex plot with multiple layers
  plot_obj <- nightowl::DeclarativePlot$new(
    data = data,
    mapping = list(x = "x", y = "y", color = "group", size = "size"),
    layers = list(
      list(type = "points", alpha = 0.6),
      list(type = "smooth", method = "lm", se = TRUE)
    ),
    scales = list(
      list(type = "color_brewer", palette = "Set1")
    ),
    theming = list(
      theme = "minimal",
      legend_position = "bottom"
    ),
    facets = list(column = "group")
  )
  
  # Reset cache stats
  plot_obj$reset_cache_stats()
  
  # Warm up - first generation
  plot_obj$plot
  
  # Benchmark subsequent accesses
  timings <- microbenchmark(
    cached_access = {
      plot_obj$plot
    },
    times = n_iterations
  )
  
  # Get cache statistics
  cache_stats <- plot_obj$get_cache_stats()
  
  # Now benchmark without caching (force regeneration each time)
  plot_obj_no_cache <- nightowl::DeclarativePlot$new(
    data = data,
    mapping = list(x = "x", y = "y", color = "group", size = "size"),
    layers = list(
      list(type = "points", alpha = 0.6),
      list(type = "smooth", method = "lm", se = TRUE)
    ),
    scales = list(
      list(type = "color_brewer", palette = "Set1")
    ),
    theming = list(
      theme = "minimal",
      legend_position = "bottom"
    ),
    facets = list(column = "group")
  )
  
  timings_no_cache <- microbenchmark(
    no_cache_access = {
      plot_obj_no_cache$invalidate_cache()
      plot_obj_no_cache$plot
    },
    times = n_iterations
  )
  
  # Calculate improvement
  mean_cached <- mean(timings$time) / 1e6  # Convert to milliseconds
  mean_no_cache <- mean(timings_no_cache$time) / 1e6
  improvement <- (mean_no_cache - mean_cached) / mean_no_cache * 100
  
  # Report results
  cat(sprintf("Cache Statistics:\n"))
  cat(sprintf("  Hits: %d, Misses: %d, Hit Rate: %.2f%%\n", 
              cache_stats$hits, cache_stats$misses, cache_stats$hit_rate * 100))
  cat(sprintf("\nPerformance Results:\n"))
  cat(sprintf("  Mean time WITHOUT cache: %.2f ms\n", mean_no_cache))
  cat(sprintf("  Mean time WITH cache: %.2f ms\n", mean_cached))
  cat(sprintf("  Performance improvement: %.1f%%\n", improvement))
  cat(sprintf("  Speed-up factor: %.1fx\n", mean_no_cache / mean_cached))
  
  return(list(
    data_size = data_size,
    mean_cached = mean_cached,
    mean_no_cache = mean_no_cache,
    improvement = improvement,
    cache_stats = cache_stats
  ))
}

# Run benchmarks with different data sizes
cat("Plot Caching Performance Benchmark\n")
cat("==================================\n")

results <- list()
data_sizes <- c(100, 500, 1000, 5000)

for (size in data_sizes) {
  results[[as.character(size)]] <- benchmark_plot_caching(size)
}

# Summary table
cat("\n\nSummary Table:\n")
cat("Data Size | No Cache (ms) | With Cache (ms) | Improvement | Speed-up\n")
cat("----------|---------------|-----------------|-------------|----------\n")

for (size in data_sizes) {
  r <- results[[as.character(size)]]
  cat(sprintf("%9d | %13.2f | %15.2f | %10.1f%% | %7.1fx\n",
              r$data_size, r$mean_no_cache, r$mean_cached, 
              r$improvement, r$mean_no_cache / r$mean_cached))
}

# Check if we achieved 70% improvement
improvements <- sapply(results, function(x) x$improvement)
avg_improvement <- mean(improvements)

cat(sprintf("\nAverage improvement across all data sizes: %.1f%%\n", avg_improvement))

if (avg_improvement >= 70) {
  cat("✓ SUCCESS: Achieved target of 70% performance improvement!\n")
} else if (avg_improvement >= 65) {
  cat("✓ PARTIAL SUCCESS: Achieved >65% improvement (close to 70% target)\n")
} else {
  cat("✗ FAILED: Did not achieve 70% performance improvement target\n")
}