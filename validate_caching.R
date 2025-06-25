#!/usr/bin/env Rscript

# Simple validation script for plot caching improvement

# Load required libraries
library(R6)
library(memoise)

# Source other required files
if (file.exists("R/imports.R")) source("R/imports.R")
if (file.exists("R/generics.R")) source("R/generics.R")

# Define nightowl namespace functions if not available
if (!exists("nightowl")) {
  nightowl <- list()
  nightowl$Plot <- R6::R6Class("Plot")  # Dummy base class
}

# Source the plot.R file directly
source("R/plot.R")

# Mock required functions that might not be available
if (!exists("render_svg")) {
  render_svg <- function(g, ...) {
    # Simple mock that returns a string representation
    return(paste("<svg>", as.character(g), "</svg>"))
  }
}

# Create a simple test
cat("Testing Plot Caching Implementation\n")
cat("===================================\n\n")

# Test 1: Basic caching functionality
cat("Test 1: Basic caching functionality\n")

# Create a test plot
test_data <- data.frame(x = 1:100, y = rnorm(100))

plot_obj <- DeclarativePlot$new()
plot_obj$data <- test_data
plot_obj$mapping <- list(x = "x", y = "y")

# First access (should be cache miss)
plot_obj$reset_cache_stats()
start_time <- Sys.time()
plot1 <- plot_obj$set_plot()
end_time <- Sys.time()
time_first <- as.numeric(end_time - start_time, units = "secs")

stats1 <- plot_obj$get_cache_stats()
cat(sprintf("  First access - Hits: %d, Misses: %d\n", stats1$hits, stats1$misses))

# Second access (should be cache hit)
start_time <- Sys.time()
plot2 <- plot_obj$set_plot()
end_time <- Sys.time()
time_second <- as.numeric(end_time - start_time, units = "secs")

stats2 <- plot_obj$get_cache_stats()
cat(sprintf("  Second access - Hits: %d, Misses: %d\n", stats2$hits, stats2$misses))

# Calculate improvement
if (time_first > 0) {
  improvement <- (time_first - time_second) / time_first * 100
  cat(sprintf("  Time improvement: %.1f%%\n", improvement))
} else {
  cat("  Time too short to measure\n")
}

# Test 2: Cache invalidation
cat("\nTest 2: Cache invalidation\n")

# Change data and check if cache is invalidated
plot_obj$set_data(data.frame(x = 1:50, y = rnorm(50)))
plot_obj$set_plot()
stats3 <- plot_obj$get_cache_stats()
cat(sprintf("  After data change - Hits: %d, Misses: %d\n", stats3$hits, stats3$misses))

# Test 3: Performance with larger dataset
cat("\nTest 3: Performance with larger dataset\n")

large_data <- data.frame(
  x = rnorm(10000),
  y = rnorm(10000),
  group = sample(LETTERS[1:5], 10000, replace = TRUE)
)

plot_obj_large <- DeclarativePlot$new()
plot_obj_large$data <- large_data
plot_obj_large$mapping <- list(x = "x", y = "y", color = "group")
plot_obj_large$reset_cache_stats()

# Time multiple accesses
times <- numeric(5)
for (i in 1:5) {
  start_time <- Sys.time()
  plot_obj_large$set_plot()
  end_time <- Sys.time()
  times[i] <- as.numeric(end_time - start_time, units = "secs")
}

cat(sprintf("  Access times (seconds): %s\n", paste(round(times, 4), collapse = ", ")))
cat(sprintf("  First access: %.4f s\n", times[1]))
cat(sprintf("  Average cached access: %.4f s\n", mean(times[2:5])))

if (times[1] > 0) {
  cache_improvement <- (times[1] - mean(times[2:5])) / times[1] * 100
  cat(sprintf("  Cache improvement: %.1f%%\n", cache_improvement))
}

final_stats <- plot_obj_large$get_cache_stats()
cat(sprintf("  Final stats - Hits: %d, Misses: %d, Hit rate: %.1f%%\n", 
            final_stats$hits, final_stats$misses, final_stats$hit_rate * 100))

cat("\nValidation complete!\n")