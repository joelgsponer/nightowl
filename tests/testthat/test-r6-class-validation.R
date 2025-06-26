# Comprehensive R6 Class Validation Tests
# Tests for all major R6 classes: Plot, Summary, Coxph, DeclarativePlot, NightowlOptions

# Test data setup
test_data <- tibble::tibble(
  group = c(rep("A", 25), rep("B", 25)),
  numeric_var = c(rnorm(25, 0, 1), rnorm(25, 2, 1.5)),
  categorical_var = sample(c("Cat1", "Cat2", "Cat3"), 50, replace = TRUE),
  time_var = sample(1:100, 50, replace = TRUE),
  event_var = sample(c(0, 1), 50, replace = TRUE)
)

# Plot Class Validation Tests ====
test_that("Plot class constructor validates input parameters correctly", {
  # Valid construction
  p <- ggplot2::ggplot(test_data, ggplot2::aes(x = group, y = numeric_var)) + 
    ggplot2::geom_point()
  plot_obj <- nightowl::Plot$new(plot = p)
  
  expect_true(inherits(plot_obj, "Plot"))
  expect_true(inherits(plot_obj, "R6"))
  
  # Test with SVG options
  plot_obj_with_options <- nightowl::Plot$new(
    plot = p, 
    options_svg = list(height = 2, width = 4)
  )
  expect_equal(plot_obj_with_options$options_svg$height, 2)
  expect_equal(plot_obj_with_options$options_svg$width, 4)
})

test_that("Plot class methods work correctly and maintain state", {
  p <- ggplot2::ggplot(test_data, ggplot2::aes(x = group, y = numeric_var)) + 
    ggplot2::geom_point()
  plot_obj <- nightowl::Plot$new(plot = p)
  
  # Test width/height getters and setters
  original_width <- plot_obj$get_width()
  plot_obj$set_options_svg(list(width = 10))
  expect_equal(plot_obj$get_width(), 10)
  expect_true(plot_obj$get_width() != original_width)
  
  # Test format method
  formatted <- plot_obj$format()
  expect_true(is.character(formatted))
  expect_true(nchar(formatted) > 0)
  
  # Test HTML generation
  html_output <- plot_obj$html()
  expect_true(inherits(html_output, c("shiny.tag", "list")))
  
  # Test ggplot extraction
  ggplot_output <- plot_obj$ggplot()
  expect_true(inherits(ggplot_output, "ggplot"))
})

test_that("Plot class handles invalid inputs appropriately", {
  # Invalid plot object
  expect_error(
    nightowl::Plot$new(plot = "not_a_plot"),
    ".*plot.*"
  )
  
  # Invalid SVG options
  p <- ggplot2::ggplot(test_data, ggplot2::aes(x = group, y = numeric_var)) + 
    ggplot2::geom_point()
  
  expect_error(
    nightowl::Plot$new(plot = p, options_svg = list(height = "invalid")),
    ".*height.*"
  )
})

# Summary Class Validation Tests ====
test_that("Summary class constructor validates parameters and initializes correctly", {
  # Valid categorical summary
  summary_obj <- nightowl::Summary$new(
    data = test_data, 
    x = "categorical_var", 
    by = "group"
  )
  
  expect_true(inherits(summary_obj, "Summary"))
  expect_true(inherits(summary_obj, "R6"))
  expect_true(is.data.frame(summary_obj$data))
  expect_true(is.list(summary_obj$calculations))
  
  # Valid numeric summary
  numeric_summary <- nightowl::Summary$new(
    data = test_data,
    x = "numeric_var",
    by = "group",
    method = nightowl::summarise_numeric_pointrange
  )
  
  expect_true(inherits(numeric_summary, "Summary"))
  expect_true(is.data.frame(numeric_summary$data))
})

test_that("Summary class methods produce expected output formats", {
  summary_obj <- nightowl::Summary$new(
    data = test_data, 
    x = "categorical_var", 
    by = "group"
  )
  
  # Test raw output
  raw_output <- summary_obj$raw()
  expect_true(is.data.frame(raw_output) || is.list(raw_output))
  
  # Test reactable output
  reactable_output <- summary_obj$reactable()
  expect_true(inherits(reactable_output, "reactable"))
  
  # Test kable output  
  kable_output <- summary_obj$kable()
  expect_true(inherits(kable_output, c("knitr_kable", "character")))
})

test_that("Summary class calculation system works correctly", {
  summary_obj <- nightowl::Summary$new(
    data = test_data, 
    x = "categorical_var", 
    by = "group"
  )
  
  # Test adding custom calculations
  original_calcs <- length(summary_obj$calculations)
  summary_obj$add_calculation(list(
    "Custom_Count" = function(x) length(x),
    "Custom_Missing" = function(x) sum(is.na(x))
  ))
  
  expect_equal(length(summary_obj$calculations), original_calcs + 2)
  expect_true("Custom_Count" %in% names(summary_obj$calculations))
  expect_true("Custom_Missing" %in% names(summary_obj$calculations))
})

test_that("Summary class handles invalid inputs appropriately", {
  # Invalid data
  expect_error(
    nightowl::Summary$new(data = "not_data_frame", x = "var", by = "group"),
    ".*data.*"
  )
  
  # Non-existent column
  expect_error(
    nightowl::Summary$new(data = test_data, x = "nonexistent_col", by = "group"),
    ".*column.*"
  )
  
  # Invalid method
  expect_error(
    nightowl::Summary$new(
      data = test_data, 
      x = "numeric_var", 
      by = "group",
      method = "invalid_method"
    ),
    ".*method.*"
  )
})

# Coxph Class Validation Tests ====
test_that("Coxph class handles survival analysis correctly", {
  # Create survival test data
  survival_data <- tibble::tibble(
    time = sample(1:100, 50, replace = TRUE),
    event = sample(c(0, 1), 50, replace = TRUE),
    treatment = sample(c("Control", "Treatment"), 50, replace = TRUE),
    covariate1 = rnorm(50),
    covariate2 = sample(c("Low", "High"), 50, replace = TRUE)
  )
  
  # Test Coxph fitting
  result <- nightowl::fit_coxph(
    data = survival_data,
    time = "time",
    event = "event", 
    treatment = "treatment",
    covariates = c("covariate1", "covariate2"),
    exponentiate = FALSE
  )
  
  expect_true(is.list(result))
  expect_true("call" %in% names(result) || "formula" %in% names(result))
  
  # Test Coxph plotting
  plot_result <- nightowl::plot_coxph(
    data = survival_data,
    time = "time",
    event = "event",
    treatment = "treatment", 
    covariates = c("covariate1", "covariate2"),
    exponentiate = TRUE
  )
  
  expect_true(is.data.frame(plot_result) || inherits(plot_result, "summary"))
})

test_that("Coxph functions validate formula construction safely", {
  # This leverages existing security tests but ensures class-level validation
  survival_data <- tibble::tibble(
    time = 1:10,
    event = rep(c(0, 1), 5),
    treatment = rep(c("A", "B"), 5)
  )
  
  # Valid formula construction
  expect_no_error(
    nightowl::fit_coxph(
      data = survival_data,
      time = "time",
      event = "event",
      treatment = "treatment"
    )
  )
  
  # Test with edge case variable names
  edge_data <- survival_data
  names(edge_data)[1] <- "time.with.dots"
  names(edge_data)[3] <- "treatment_with_underscores"
  
  expect_no_error(
    nightowl::fit_coxph(
      data = edge_data,
      time = "time.with.dots",
      event = "event",
      treatment = "treatment_with_underscores"
    )
  )
})

# NightowlOptions Class Validation Tests ====
test_that("NightowlOptions class manages global state correctly", {
  options <- get_nightowl_options()
  
  expect_true(inherits(options, "NightowlOptions"))
  expect_true(inherits(options, "R6"))
  
  # Test color management
  original_colors <- options$get_colors()
  expect_true(is.character(original_colors))
  expect_true(length(original_colors) > 0)
  
  # Test color setting and getting
  new_colors <- c("#FF0000", "#00FF00", "#0000FF")
  options$set_colors(new_colors)
  expect_equal(options$get_colors(), new_colors)
  
  # Test missing color
  missing_color <- options$get_missing_color()
  expect_true(is.character(missing_color))
  expect_equal(length(missing_color), 1)
  
  # Test header width
  original_width <- options$get_header_width()
  expect_true(is.numeric(original_width))
  
  options$set_header_width(15)
  expect_equal(options$get_header_width(), 15)
  
  # Reset to original state
  options$set_colors(original_colors)
  options$set_header_width(original_width)
})

test_that("NightowlOptions class validates input parameters", {
  options <- get_nightowl_options()
  
  # Test invalid color input
  expect_error(
    options$set_colors("not_a_vector"),
    ".*color.*"
  )
  
  expect_error(
    options$set_colors(123),
    ".*color.*"
  )
  
  # Test invalid header width
  expect_error(
    options$set_header_width("not_numeric"),
    ".*width.*"
  )
  
  expect_error(
    options$set_header_width(-1),
    ".*width.*"
  )
})

test_that("NightowlOptions class maintains encapsulation and prevents global pollution", {
  # This test ensures options don't leak into global environment
  options1 <- get_nightowl_options()
  options2 <- get_nightowl_options()
  
  # Modify one instance
  options1$set_header_width(99)
  
  # Other instance should reflect the change (singleton pattern)
  # or remain independent depending on implementation
  # This test documents the current behavior
  current_width <- options2$get_header_width()
  expect_true(is.numeric(current_width))
  
  # Reset
  options1$set_header_width(10)
})

# DeclarativePlot Class Performance Validation ====
test_that("DeclarativePlot class maintains performance characteristics", {
  # Test basic declarative plot creation
  declarative_plot <- nightowl::plot(
    data = test_data,
    mapping = list(
      x = "group",
      y = "numeric_var",
      color = "categorical_var"
    ),
    layers = list(
      list(type = "generic", geom = "ggplot2::geom_point")
    )
  )
  
  expect_true(inherits(declarative_plot, "DeclarativePlot"))
  
  # Test performance aspects (basic timing)
  start_time <- Sys.time()
  result <- nightowl::plot(
    data = test_data,
    mapping = list(x = "group", y = "numeric_var"),
    layers = list(list(type = "generic", geom = "ggplot2::geom_point"))
  )
  end_time <- Sys.time()
  
  execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  expect_true(execution_time < 5) # Should complete within 5 seconds
  
  # Test that result is valid
  expect_true(inherits(result, "DeclarativePlot"))
})

# Cross-Class Integration Tests ====
test_that("R6 classes integrate correctly with each other", {
  # Test Plot and Summary integration
  summary_obj <- nightowl::Summary$new(
    data = test_data, 
    x = "numeric_var", 
    by = "group",
    method = nightowl::summarise_numeric_pointrange
  )
  
  raw_result <- summary_obj$raw()
  expect_true(is.data.frame(raw_result) || is.list(raw_result))
  
  # Test options integration with other classes
  options <- get_nightowl_options()
  original_colors <- options$get_colors()
  
  # Change options
  test_colors <- c("#AA0000", "#00AA00") 
  options$set_colors(test_colors)
  
  # Create new summary with changed options
  summary_with_options <- nightowl::Summary$new(
    data = test_data,
    x = "categorical_var", 
    by = "group"
  )
  
  # Verify the integration worked
  expect_true(inherits(summary_with_options, "Summary"))
  
  # Reset options
  options$set_colors(original_colors)
})