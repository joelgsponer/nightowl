test_that("nightowl options work correctly", {
  options <- get_nightowl_options()
  expect_true(inherits(options, "NightowlOptions"))
  expect_true(is.character(options$get_colors()))
  expect_true(is.character(options$get_missing_color()))
  expect_true(is.numeric(options$get_header_width()))
})

test_that("NightowlOptions class maintains state consistency", {
  options <- get_nightowl_options()
  
  # Store original values
  original_colors <- options$get_colors()
  original_missing <- options$get_missing_color()
  original_width <- options$get_header_width()
  
  # Test color setting and persistence
  new_colors <- c("#FF0000", "#00FF00", "#0000FF")
  options$set_colors(new_colors)
  expect_identical(options$get_colors(), new_colors)
  
  # Test width setting and persistence
  new_width <- 25
  options$set_header_width(new_width)
  expect_equal(options$get_header_width(), new_width)
  
  # Test that other values remain unchanged
  expect_identical(options$get_missing_color(), original_missing)
  
  # Reset to original state
  options$set_colors(original_colors)
  options$set_header_width(original_width)
  
  # Verify reset worked
  expect_identical(options$get_colors(), original_colors)
  expect_equal(options$get_header_width(), original_width)
})

test_that("NightowlOptions validates input parameters strictly", {
  options <- get_nightowl_options()
  
  # Invalid color inputs
  expect_error(options$set_colors("single_string"), ".*color.*")
  expect_error(options$set_colors(123), ".*color.*")
  expect_error(options$set_colors(list("red", "blue")), ".*color.*")
  expect_error(options$set_colors(c("#GGGGGG")), ".*color.*")
  
  # Invalid width inputs  
  expect_error(options$set_header_width("not_numeric"), ".*width.*")
  expect_error(options$set_header_width(-1), ".*width.*")
  expect_error(options$set_header_width(0), ".*width.*")
  expect_error(options$set_header_width(c(1, 2)), ".*width.*")
})
