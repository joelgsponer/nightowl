test_that("plot caching provides 70% performance improvement", {
  # Create a sample dataset
  data <- data.frame(
    x = rnorm(1000),
    y = rnorm(1000),
    group = sample(c("A", "B", "C"), 1000, replace = TRUE)
  )
  
  # Create a DeclarativePlot with multiple layers
  plot_obj <- DeclarativePlot$new(
    data = data,
    mapping = list(x = "x", y = "y", color = "group"),
    layers = list(
      list(type = "points"),
      list(type = "smooth", method = "lm")
    ),
    theming = list(theme = "minimal")
  )
  
  # Reset cache statistics
  plot_obj$reset_cache_stats()
  
  # First plot generation (cache miss)
  time1_start <- Sys.time()
  plot1 <- plot_obj$plot
  time1_end <- Sys.time()
  time_first_generation <- as.numeric(time1_end - time1_start, units = "secs")
  
  # Check cache miss
  stats_after_first <- plot_obj$get_cache_stats()
  expect_equal(stats_after_first$hits, 0)
  expect_equal(stats_after_first$misses, 1)
  
  # Second plot access (should be cache hit)
  time2_start <- Sys.time()
  plot2 <- plot_obj$plot
  time2_end <- Sys.time()
  time_cached_access <- as.numeric(time2_end - time2_start, units = "secs")
  
  # Check cache hit
  stats_after_second <- plot_obj$get_cache_stats()
  expect_equal(stats_after_second$hits, 1)
  expect_equal(stats_after_second$misses, 1)
  expect_equal(stats_after_second$hit_rate, 0.5)
  
  # Verify plots are identical
  expect_identical(plot1, plot2)
  
  # Calculate performance improvement
  improvement_ratio <- (time_first_generation - time_cached_access) / time_first_generation
  
  # Expect at least 70% improvement (allowing some variance)
  expect_true(improvement_ratio >= 0.65, 
              info = paste("Expected at least 65% improvement, got", 
                          round(improvement_ratio * 100, 2), "%"))
  
  # Test cache invalidation
  plot_obj$set_data(data[1:500, ])
  
  # Access plot again (should be cache miss)
  plot3 <- plot_obj$plot
  stats_after_invalidation <- plot_obj$get_cache_stats()
  expect_equal(stats_after_invalidation$misses, 2)
})

test_that("cache invalidation works correctly for all setters", {
  data <- data.frame(x = 1:10, y = 1:10)
  
  plot_obj <- DeclarativePlot$new(
    data = data,
    mapping = list(x = "x", y = "y")
  )
  
  # Initial plot generation
  plot_obj$reset_cache_stats()
  plot1 <- plot_obj$plot
  expect_equal(plot_obj$get_cache_stats()$misses, 1)
  
  # Test data setter invalidation
  plot_obj$set_data(data.frame(x = 1:20, y = 1:20))
  plot2 <- plot_obj$plot
  expect_equal(plot_obj$get_cache_stats()$misses, 2)
  
  # Test mapping setter invalidation
  plot_obj$set_mapping(list(x = "y", y = "x"))
  plot3 <- plot_obj$plot
  expect_equal(plot_obj$get_cache_stats()$misses, 3)
  
  # Test layers setter invalidation
  plot_obj$set_layers(list(list(type = "points")))
  plot4 <- plot_obj$plot
  expect_equal(plot_obj$get_cache_stats()$misses, 4)
  
  # Test scales setter invalidation
  plot_obj$set_scales(list(list(type = "x_continuous", limits = c(0, 30))))
  plot5 <- plot_obj$plot
  expect_equal(plot_obj$get_cache_stats()$misses, 5)
})

test_that("multiple identical plot accesses use cache", {
  data <- data.frame(x = runif(100), y = runif(100))
  
  plot_obj <- DeclarativePlot$new(
    data = data,
    mapping = list(x = "x", y = "y"),
    layers = list(list(type = "points"))
  )
  
  plot_obj$reset_cache_stats()
  
  # Generate plot multiple times
  plots <- list()
  for (i in 1:10) {
    plots[[i]] <- plot_obj$plot
  }
  
  # Check cache statistics
  stats <- plot_obj$get_cache_stats()
  expect_equal(stats$misses, 1)  # Only first access should miss
  expect_equal(stats$hits, 9)     # Remaining 9 should hit
  expect_equal(stats$hit_rate, 0.9)
  
  # All plots should be identical
  for (i in 2:10) {
    expect_identical(plots[[1]], plots[[i]])
  }
})