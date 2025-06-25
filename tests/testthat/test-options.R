test_that("nightowl options work correctly", {
  options <- get_nightowl_options()
  expect_true(inherits(options, "NightowlOptions"))
  expect_true(is.character(options$get_colors()))
  expect_true(is.character(options$get_missing_color()))
  expect_true(is.numeric(options$get_header_width()))
})
