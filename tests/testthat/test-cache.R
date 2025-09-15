# Tests for caching functionality
# Related to GitHub Issue #53: Implement dependency management and caching strategy

test_that("cache helper functions work correctly", {
  # Test cache key generation
  cache_key <- source(".github/scripts/cache-helper.R")$value$generate_cache_key("test")
  
  expect_type(cache_key, "character")
  expect_gt(nchar(cache_key), 10)
  expect_false(grepl("[^a-zA-Z0-9.-]", cache_key))
})

test_that("dependency validation works", {
  # Mock DESCRIPTION file validation
  expect_true(file.exists("DESCRIPTION"))
  
  # Source the cache helper functions
  source(".github/scripts/cache-helper.R")
  
  # Test that validation function exists and can be called
  expect_type(validate_dependencies, "closure")
  
  # Test with actual DESCRIPTION file
  result <- validate_dependencies("DESCRIPTION")
  expect_type(result, "logical")
})

test_that("cache performance analysis functions exist", {
  source(".github/scripts/cache-helper.R")
  
  # Test that key functions are defined
  expect_type(analyze_cache_performance, "closure")
  expect_type(handle_internal_packages, "closure")
  expect_type(generate_cache_key, "closure")
})

test_that("internal package handling works", {
  source(".github/scripts/cache-helper.R")
  
  # Test with known packages
  result <- handle_internal_packages(c("ggplot2", "dplyr"))
  
  expect_type(result, "list")
  expect_true(all(c("ggplot2", "dplyr") %in% names(result)))
  
  # Each package should have required fields
  for (pkg_info in result) {
    expect_true(all(c("package", "installed", "available_on_cran", "needs_special_handling") %in% names(pkg_info)))
  }
})

test_that("cache key generation is deterministic", {
  source(".github/scripts/cache-helper.R")
  
  # Generate same key multiple times
  key1 <- generate_cache_key("test", "4.3.0", "abc123")
  key2 <- generate_cache_key("test", "4.3.0", "abc123")
  
  expect_equal(key1, key2)
  
  # Different inputs should produce different keys
  key3 <- generate_cache_key("test", "4.2.0", "abc123")
  expect_false(key1 == key3)
})

test_that("cache key sanitization works", {
  source(".github/scripts/cache-helper.R")
  
  # Test with special characters
  key <- generate_cache_key("test/with:special*chars")
  
  # Should only contain allowed characters
  expect_true(grepl("^[a-zA-Z0-9.-]+$", key))
  expect_false(grepl("[/:*]", key))
})

test_that("dependency analysis handles missing DESCRIPTION", {
  source(".github/scripts/cache-helper.R")
  
  # Test with non-existent file
  result <- validate_dependencies("non-existent-file.txt")
  expect_false(result)
})

test_that("cache analysis handles missing baseline", {
  source(".github/scripts/cache-helper.R")
  
  # Test with non-existent baseline file
  result <- analyze_cache_performance("non-existent-baseline.txt")
  
  expect_type(result, "list")
  expect_false(result$baseline_available)
  expect_true(result$cache_enabled)
})