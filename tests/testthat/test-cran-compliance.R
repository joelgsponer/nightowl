test_that("package does not pollute global environment", {
  # Check that package doesn't create objects in globalenv
  before_objects <- ls(envir = globalenv())
  
  # Load package functions (simulate package loading)
  library(nightowl)
  
  after_objects <- ls(envir = globalenv())
  
  # Check no new objects were added to global environment
  new_objects <- setdiff(after_objects, before_objects)
  
  # Filter out objects that might be legitimately added by test framework
  new_objects <- new_objects[!grepl("^test_", new_objects)]
  
  expect_length(new_objects, 0)
})

test_that("nightowl options are properly encapsulated", {
  # Check that options are not in global environment
  expect_false(exists("NightowlOptions", envir = globalenv()))
  
  # Check that we can access options through proper interface
  options <- get_nightowl_options()
  expect_true(inherits(options, "NightowlOptions"))
  
  # Check that setting options works correctly
  original_colors <- options$get_colors()
  test_colors <- c("red", "blue", "green")
  options$set_colors(test_colors)
  expect_equal(options$get_colors(), test_colors)
  
  # Reset to original colors
  options$set_colors(original_colors)
})

test_that("package state is properly managed", {
  # Test that we can create and use separate options instances
  options1 <- get_nightowl_options()
  options2 <- .NightowlOptions$new()
  
  # Modify options2 and check options1 is unaffected initially
  original_colors <- options1$get_colors()
  test_colors <- c("red", "blue")
  options2$set_colors(test_colors)
  
  # options1 should still have original colors since they're different instances
  expect_equal(options1$get_colors(), original_colors)
  expect_equal(options2$get_colors(), test_colors)
  
  # Test that set_nightowl_options properly updates the package instance
  set_nightowl_options(options2)
  updated_options <- get_nightowl_options()
  expect_equal(updated_options$get_colors(), test_colors)
})