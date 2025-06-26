test_that("Path validation prevents traversal attacks", {
  expect_true(is_safe_path("valid_file.yaml"))
  expect_true(is_safe_path("subdir/file.yaml"))
  expect_false(is_safe_path("../../../etc/passwd"))
  expect_false(is_safe_path("..\\windows\\system32"))
  expect_false(is_safe_path("~/.ssh/id_rsa"))
  expect_false(is_safe_path("/etc/passwd", allow_absolute = FALSE))
  expect_true(is_safe_path("/etc/passwd", allow_absolute = TRUE))
  expect_false(is_safe_path("file_null.txt"))
  expect_false(is_safe_path("file<script>.txt"))
})

test_that("File path validation works correctly", {
  # Create a temporary file for testing
  temp_file <- tempfile(fileext = ".yaml")
  writeLines("test: content", temp_file)
  
  expect_equal(validate_file_path(basename(temp_file)), basename(temp_file))
  expect_error(validate_file_path("../dangerous/path"), "unsafe path")
  expect_error(validate_file_path("nonexistent.yaml", must_exist = TRUE), "does not exist")
  expect_error(validate_file_path("file.txt", allowed_extensions = "yaml"), "must have one of these extensions")
  
  # Clean up
  unlink(temp_file)
})

test_that("Numeric validation works correctly", {
  expect_equal(validate_numeric(c(1, 2, 3)), c(1, 2, 3))
  expect_error(validate_numeric("not_numeric"), "must be numeric")
  expect_error(validate_numeric(c(1, NA, 3), allow_na = FALSE), "cannot contain NA")
  expect_equal(validate_numeric(c(1, NA, 3), allow_na = TRUE), c(1, NA, 3))
  expect_error(validate_numeric(0, min_value = 1), "must be >= 1")
  expect_error(validate_numeric(10, max_value = 5), "must be <= 5")
})

test_that("Data frame validation works correctly", {
  df <- data.frame(x = 1:3, y = 4:6)
  expect_equal(validate_data_frame(df), df)
  expect_error(validate_data_frame("not_df"), "must be a data frame")
  expect_error(validate_data_frame(data.frame()), "cannot be empty")
  expect_error(validate_data_frame(df, required_cols = "z"), "must contain columns: z")
  expect_equal(validate_data_frame(df, required_cols = c("x", "y")), df)
})

test_that("Column name validation prevents injection", {
  expect_equal(validate_column_names(c("valid_name", "also.valid", "name123")), 
               c("valid_name", "also.valid", "name123"))
  expect_error(validate_column_names("123invalid"), "Invalid")
  expect_error(validate_column_names("name with spaces"), "Invalid")
  expect_error(validate_column_names("name-with-dashes"), "Invalid")
  expect_error(validate_column_names("name$with$symbols"), "Invalid")
})

test_that("Character validation works correctly", {
  expect_equal(validate_character("test"), "test")
  expect_error(validate_character(123), "must be character")
  expect_error(validate_character("a", min_length = 5), "must be at least 5 characters")
  expect_error(validate_character("very_long_string", max_length = 5), "must be at most 5 characters")
  expect_equal(validate_character("valid", allowed_values = c("valid", "also_valid")), "valid")
  expect_error(validate_character("invalid", allowed_values = c("valid", "also_valid")), "Invalid values")
})

test_that("Logical validation works correctly", {
  expect_equal(validate_logical(TRUE), TRUE)
  expect_equal(validate_logical(c(TRUE, FALSE)), c(TRUE, FALSE))
  expect_error(validate_logical("not_logical"), "must be logical")
  expect_error(validate_logical(c(TRUE, NA)), "cannot contain NA")
})

test_that("Custom error classes work correctly", {
  expect_s3_class(validation_error("test", "param", "value"), "validation_error")
  expect_s3_class(security_error("test", "operation"), "security_error")
  expect_s3_class(data_error("test", "info"), "data_error")
  
  expect_error(throw_validation_error("test error"), class = "validation_error")
  expect_error(throw_security_error("security issue"), class = "security_error")
  expect_error(throw_data_error("data problem"), class = "data_error")
})

test_that("styled_plot security fixes work", {
  # Test data
  test_data <- data.frame(x = 1:3, y = 1:3, group = letters[1:3])
  
  # Should work with valid style name
  expect_no_error(styled_plot(test_data, "default"))
  
  # Should reject dangerous file paths
  expect_error(styled_plot(test_data, "../../../etc/passwd"), "unsafe path")
  expect_error(styled_plot(test_data, "~/.ssh/id_rsa"), "unsafe path")
  
  # Should reject invalid data
  expect_error(styled_plot("not_a_dataframe", "default"), "must be a data frame")
  expect_error(styled_plot(data.frame(), "default"), "cannot be empty")
})

test_that("render_svg security fixes work", {
  # Should reject call objects
  test_call <- quote(system("rm -rf /"))
  expect_error(render_svg(test_call), class = "security_error")
  expect_error(render_svg(test_call), "Call objects are not supported")
})

test_that("forestplot validation works", {
  # Valid inputs should work
  expect_no_error(forestplot(x = 1, xmin = 0.5, xmax = 1.5))
  
  # Invalid inputs should fail
  expect_error(forestplot(x = "not_numeric", xmin = 0.5, xmax = 1.5), "must be numeric")
  expect_error(forestplot(x = 1, xmin = 0.5, xmax = 1.5, height = -1), "must be >= 0")
  expect_error(forestplot(x = 1, xmin = 0.5, xmax = 1.5, xlim = c(0, 1, 2)), "must be a numeric vector of length 2")
})

test_that("data function validation works", {
  test_data <- c(1, 2, 3, 100, 4, 5)
  
  # Valid inputs should work
  expect_no_error(detect_outliers(test_data, fold = 15))
  expect_no_error(count_outliers(test_data, fold = 15))
  
  # Invalid inputs should fail
  expect_error(detect_outliers("not_numeric"), "must be numeric")
  expect_error(detect_outliers(test_data, fold = -1), "must be >= 0")
  expect_error(count_outliers("not_numeric"), "must be numeric")
  expect_error(count_outliers(test_data, fold = -1), "must be >= 0")
})

test_that("Validation framework handles edge cases", {
  # Empty inputs
  expect_error(validate_numeric(numeric(0)), "cannot be empty")
  expect_error(validate_character(character(0)), "cannot be empty")
  
  # Very large inputs
  large_vector <- rep(1, 10000)
  expect_equal(length(validate_numeric(large_vector)), 10000)
  
  # Unicode and special characters
  expect_true(is_safe_path("file_with_ñ.yaml"))
  expect_false(is_safe_path("file\ttab.yaml"))
})

test_that("Error messages are informative", {
  expect_error(validate_numeric("test", param_name = "my_param"), 
               "Parameter 'my_param' must be numeric")
  expect_error(validate_file_path("../bad", param_name = "file_path"), 
               "Parameter 'file_path' contains unsafe path")
  expect_error(validate_data_frame(list(), required_cols = "x", param_name = "input_data"),
               "Parameter 'input_data' must be a data frame")
})