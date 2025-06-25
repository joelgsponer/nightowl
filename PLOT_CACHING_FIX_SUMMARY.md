# Plot Caching Fix Summary

## Issue #3: Fix plot caching for 70% performance improvement

### Problem
The R6 Plot class was regenerating plots on every access due to improper memoization scope, causing 70% performance loss.

### Changes Made

#### 1. Added memoise dependency
- Added `memoise` to the DESCRIPTION file's Imports section

#### 2. Fixed memoization implementation in DeclarativePlot class
- Changed from immediately calling the memoized function to storing the memoized function itself
- Created a proper cache key from plot parameters (data, mapping, layers, scales, etc.)
- Added a private `generate_plot` function that is memoized
- Modified `set_plot` to use the memoized function with cache keys

#### 3. Implemented cache invalidation
- Added `invalidate_cache()` method to clear the memoized function
- Added setter methods (`set_data`, `set_mapping`, `set_layers`, `set_scales`) that automatically invalidate the cache when properties change
- This ensures plots are regenerated when their parameters change

#### 4. Added cache metrics
- Added private counters for cache hits and misses
- Added `get_cache_stats()` method to retrieve cache statistics (hits, misses, hit rate)
- Added `reset_cache_stats()` method to reset counters
- Cache hit/miss tracking is automatic during plot generation

#### 5. Created performance tests
- Added comprehensive test file: `tests/testthat/test-plot-caching-performance.R`
- Tests verify:
  - 70% performance improvement from caching
  - Cache invalidation works correctly
  - Multiple identical accesses use cache
  - Cache statistics are tracked correctly

#### 6. Created benchmark script
- Added benchmark script: `inst/benchmarks/plot-caching-benchmark.R`
- Benchmarks performance with varying data sizes
- Compares cached vs non-cached performance
- Validates the 70% improvement target

### Expected Benefits
- Plots that are accessed multiple times without changes will be served from cache
- 70% reduction in execution time for repeated plot access
- Automatic cache invalidation ensures correctness when plot parameters change
- Cache metrics provide visibility into caching effectiveness

### Usage Example
```r
# Create a plot
plot_obj <- DeclarativePlot$new(
  data = my_data,
  mapping = list(x = "x", y = "y"),
  layers = list(list(type = "points"))
)

# First access generates the plot
plot1 <- plot_obj$plot  # Cache miss

# Subsequent accesses use cache
plot2 <- plot_obj$plot  # Cache hit (70% faster)

# Check cache statistics
stats <- plot_obj$get_cache_stats()
# Returns: list(hits = 1, misses = 1, hit_rate = 0.5)

# Changing data invalidates cache
plot_obj$set_data(new_data)
plot3 <- plot_obj$plot  # Cache miss (regenerates)
```