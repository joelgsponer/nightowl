context("Security: Formula Construction")

test_that("create_Surv_formula rejects code injection attempts", {
  # Create test data
  test_data <- data.frame(
    time = c(1, 2, 3, 4, 5),
    event = c(0, 1, 0, 1, 1),
    treatment = c("A", "B", "A", "B", "A"),
    covariate1 = c(1, 2, 3, 4, 5),
    stringsAsFactors = FALSE
  )
  
  # Test 1: Reject variable names with special characters
  expect_error(
    create_Surv_formula(
      data = test_data,
      time = "time",
      event = "event",
      treatment = "treatment); system('echo pwned'",
      covariates = NULL
    ),
    "Variable names must start with a letter"
  )
  
  # Test 2: Reject variable names with semicolons
  expect_error(
    create_Surv_formula(
      data = test_data,
      time = "time",
      event = "event",
      treatment = "treatment",
      covariates = c("covariate1; malicious_code()")
    ),
    "Variable names must start with a letter"
  )
  
  # Test 3: Reject variable names with parentheses
  expect_error(
    create_Surv_formula(
      data = test_data,
      time = "time",
      event = "event()",
      treatment = "treatment"
    ),
    "Variable names must start with a letter"
  )
  
  # Test 4: Reject non-existent variables
  expect_error(
    create_Surv_formula(
      data = test_data,
      time = "time",
      event = "event",
      treatment = "non_existent_var"
    ),
    "The following variables are not present in the data"
  )
  
  # Test 5: Accept valid variable names
  formula_obj <- create_Surv_formula(
    data = test_data,
    time = "time",
    event = "event",
    treatment = "treatment",
    covariates = "covariate1"
  )
  expect_true(inherits(formula_obj, "formula"))
  
  # Test 6: Verify formula doesn't contain injected code
  formula_str <- deparse(formula_obj)
  expect_false(grepl("system", formula_str))
  expect_false(grepl("eval", formula_str))
  expect_false(grepl("source", formula_str))
})

test_that("create_Surv_formula handles edge cases safely", {
  test_data <- data.frame(
    time_var = c(1, 2, 3),
    event_var = c(0, 1, 0),
    trt = c("A", "B", "A"),
    cov.1 = c(1, 2, 3),
    cov_2 = c(4, 5, 6)
  )
  
  # Test with dots and underscores in names (valid)
  formula_obj <- create_Surv_formula(
    data = test_data,
    time = "time_var",
    event = "event_var",
    treatment = "trt",
    covariates = c("cov.1", "cov_2")
  )
  expect_true(inherits(formula_obj, "formula"))
  
  # Test empty covariates
  formula_obj <- create_Surv_formula(
    data = test_data,
    time = "time_var",
    event = "event_var",
    treatment = "trt",
    covariates = NULL
  )
  expect_true(inherits(formula_obj, "formula"))
})

test_that("add_geom rejects malicious cut functions", {
  # Create test plot
  g <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = wt))
  
  # Test 1: Reject arbitrary function names
  expect_error(
    add_geom(
      geom = ggplot2::geom_point,
      g = g,
      cut_f = "system"
    ),
    "Invalid cut function"
  )
  
  # Test 2: Reject function with code injection
  expect_error(
    add_geom(
      geom = ggplot2::geom_point,
      g = g,
      cut_f = "cut_interval'); system('echo pwned"
    ),
    "Invalid cut function"
  )
  
  # Test 3: Accept valid cut functions
  result <- add_geom(
    geom = ggplot2::geom_point,
    g = g,
    cut_f = "cut_interval"
  )
  expect_true(inherits(result, "gg"))
  
  # Test 4: Accept cut functions with namespace
  result <- add_geom(
    geom = ggplot2::geom_point,
    g = g,
    cut_f = "ggplot2::cut_number"
  )
  expect_true(inherits(result, "gg"))
})

test_that("summarise rejects expression parameters", {
  test_data <- data.frame(
    x = c(1, 2, 3, 4, 5),
    group = c("A", "B", "A", "B", "A")
  )
  
  # Test: Reject expression parameters
  expect_error(
    summarise(
      data = test_data,
      column = "x",
      calculations = list(mean = mean),
      parameters = expression(list(malicious = system("echo pwned")))
    ),
    "parameters must be a list"
  )
  
  # Test: Accept list parameters
  result <- summarise(
    data = test_data,
    column = "x",
    calculations = list(mean = mean),
    parameters = list(mean = list(na.rm = TRUE))
  )
  expect_true(inherits(result, "data.frame"))
})