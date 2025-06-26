# Visual Regression Testing with vdiffr
# Tests plot output consistency across different scenarios

library(testthat)

# Only run visual tests if vdiffr is available
skip_if_not_installed("vdiffr")

# Test Data Setup ====
visual_test_data <- tibble::tibble(
  group = c(rep("Group_A", 30), rep("Group_B", 30), rep("Group_C", 20)),
  numeric_value = c(rnorm(30, 0, 1), rnorm(30, 2, 1.5), rnorm(20, -1, 0.8)),
  category = sample(c("Cat1", "Cat2", "Cat3"), 80, replace = TRUE),
  time = sample(1:100, 80, replace = TRUE),
  event = sample(c(0, 1), 80, replace = TRUE),
  weight = abs(rnorm(80, 50, 15))
)

# Forestplot Visual Tests ====
test_that("forestplot visual output remains consistent", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  
  # Basic forestplot
  basic_forestplot <- nightowl::forestplot(0, -1, 1)
  vdiffr::expect_doppelganger("basic_forestplot", basic_forestplot)
  
  # Forestplot with custom dimensions
  custom_forestplot <- nightowl::forestplot(0.5, -0.8, 1.2, height = 0.5)
  vdiffr::expect_doppelganger("custom_forestplot", custom_forestplot)
  
  # Forestplot with theme
  themed_forestplot <- nightowl::forestplot(
    estimate = 0.3, 
    lower = -0.5, 
    upper = 1.1, 
    height = 0.35, 
    theme = ggplot2::theme_bw()
  )
  vdiffr::expect_doppelganger("themed_forestplot", themed_forestplot)
})

# Declarative Plot Visual Tests ====
test_that("declarative plots maintain visual consistency", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  
  # Basic scatter plot
  scatter_plot <- nightowl::plot(
    data = visual_test_data,
    mapping = list(
      x = "numeric_value",
      y = "weight", 
      color = "group"
    ),
    layers = list(
      list(type = "generic", geom = "ggplot2::geom_point", alpha = 0.7)
    )
  )
  vdiffr::expect_doppelganger("declarative_scatter", scatter_plot)
  
  # Boxplot with jitter
  boxplot_jitter <- nightowl::plot(
    data = visual_test_data,
    mapping = list(
      x = "group",
      y = "numeric_value",
      color = "group"
    ),
    layers = list(
      list(type = "boxplot"),
      list(type = "generic", geom = "ggplot2::geom_jitter", alpha = 0.4, width = 0.2)
    )
  )
  vdiffr::expect_doppelganger("declarative_boxplot_jitter", boxplot_jitter)
  
  # Faceted plot
  faceted_plot <- nightowl::plot(
    data = visual_test_data,
    mapping = list(
      x = "numeric_value",
      y = "weight",
      color = "category",
      facet_row = "group"
    ),
    layers = list(
      list(type = "generic", geom = "ggplot2::geom_point")
    )
  )
  vdiffr::expect_doppelganger("declarative_faceted", faceted_plot)
})

# Inline Plot Visual Tests ====
test_that("inline plots render consistently", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  
  # Inline histogram
  set.seed(42) # For reproducible random data
  inline_histogram <- nightowl::add_inline_plot(
    rnorm(1000, 0, 1),
    style = "Inline-Histogram"
  )
  vdiffr::expect_doppelganger("inline_histogram", inline_histogram)
  
  # Inline violin plot
  set.seed(42)
  inline_violin <- nightowl::add_inline_plot(
    rnorm(1000, 0, 1),
    style = "Inline-Violin",
    height = 0.6
  )
  vdiffr::expect_doppelganger("inline_violin", inline_violin)
  
  # Inline density plot
  set.seed(42)
  inline_density <- nightowl::add_inline_plot(
    rnorm(1000, 0, 1),
    style = "Inline-Density",
    mapping = list(x = "x", y = NULL)
  )
  vdiffr::expect_doppelganger("inline_density", inline_density)
  
  # Inline pointrange
  set.seed(42)
  inline_pointrange <- nightowl::add_inline_plot(
    rnorm(1000, 0, 1),
    style = "Inline-Pointrange"
  )
  vdiffr::expect_doppelganger("inline_pointrange", inline_pointrange)
})

# Summary Plot Visual Tests ====
test_that("summary plots maintain visual consistency", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  
  # Categorical summary barplot
  summary_barplot <- nightowl::Summary$new(
    data = visual_test_data,
    x = "category",
    by = "group",
    method = nightowl::summarise_categorical_barplot
  )$raw()
  
  # Extract first plot for testing
  if (is.data.frame(summary_barplot) && "Barplot" %in% names(summary_barplot)) {
    first_barplot <- summary_barplot$Barplot[[1]]
    vdiffr::expect_doppelganger("summary_categorical_barplot", first_barplot)
  }
  
  # Numeric summary violin
  summary_violin <- nightowl::Summary$new(
    data = visual_test_data,
    x = "numeric_value", 
    by = "group",
    method = nightowl::summarise_numeric_violin
  )$raw()
  
  # Extract first violin plot for testing
  if (is.data.frame(summary_violin) && "Violin" %in% names(summary_violin)) {
    first_violin <- summary_violin$Violin[[1]]
    vdiffr::expect_doppelganger("summary_numeric_violin", first_violin)
  }
  
  # Numeric summary pointrange
  summary_pointrange <- nightowl::Summary$new(
    data = visual_test_data,
    x = "numeric_value",
    by = "group", 
    method = nightowl::summarise_numeric_pointrange
  )$raw()
  
  # Extract first pointrange plot for testing
  if (is.data.frame(summary_pointrange) && "Pointrange" %in% names(summary_pointrange)) {
    first_pointrange <- summary_pointrange$Pointrange[[1]]
    vdiffr::expect_doppelganger("summary_numeric_pointrange", first_pointrange)
  }
})

# Donut Plot Visual Tests ====
test_that("donut plots render consistently", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  
  # Categorical donut plot
  categorical_donut <- nightowl::donut_plot(
    data = visual_test_data,
    x = "category"
  )
  vdiffr::expect_doppelganger("categorical_donut", categorical_donut)
  
  # Numeric donut plot (if supported)
  numeric_donut <- nightowl::donut_plot(
    data = visual_test_data,
    x = "group"
  )
  vdiffr::expect_doppelganger("numeric_donut", numeric_donut)
})

# Survival Plot Visual Tests ====
test_that("survival plots maintain visual consistency", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  
  # Create survival-specific test data
  survival_data <- tibble::tibble(
    time = sample(1:100, 100, replace = TRUE),
    event = sample(c(0, 1), 100, replace = TRUE),
    treatment = sample(c("Control", "Treatment"), 100, replace = TRUE),
    strata1 = sample(c("Low", "High"), 100, replace = TRUE)
  )
  
  # Kaplan-Meier plot
  km_plot <- nightowl::plot_km(
    data = survival_data,
    time = "time",
    event = "event",
    treatment = "treatment"
  )
  vdiffr::expect_doppelganger("kaplan_meier_plot", km_plot)
  
  # Grouped KM plot
  grouped_km_plot <- nightowl::plot_grouped_km(
    data = survival_data,
    time = "time",
    event = "event", 
    treatment = "treatment",
    strata = "strata1"
  )
  vdiffr::expect_doppelganger("grouped_km_plot", grouped_km_plot)
})

# Style and Theme Consistency Tests ====
test_that("different styles and themes render consistently", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  
  # Test with different themes
  base_plot_data <- visual_test_data[1:20, ]
  
  # Default theme
  default_theme_plot <- nightowl::plot(
    data = base_plot_data,
    mapping = list(x = "group", y = "numeric_value"),
    layers = list(list(type = "boxplot"))
  )
  vdiffr::expect_doppelganger("default_theme_plot", default_theme_plot)
  
  # Minimal theme
  minimal_theme_plot <- nightowl::plot(
    data = base_plot_data,
    mapping = list(x = "group", y = "numeric_value"),
    layers = list(list(type = "boxplot"))
  ) + ggplot2::theme_minimal()
  vdiffr::expect_doppelganger("minimal_theme_plot", minimal_theme_plot)
  
  # BW theme
  bw_theme_plot <- nightowl::plot(
    data = base_plot_data,
    mapping = list(x = "group", y = "numeric_value"),
    layers = list(list(type = "boxplot"))
  ) + ggplot2::theme_bw()
  vdiffr::expect_doppelganger("bw_theme_plot", bw_theme_plot)
})

# Color Consistency Tests ====
test_that("color schemes render consistently", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  
  # Store original options
  options <- get_nightowl_options()
  original_colors <- options$get_colors()
  
  # Test with custom colors
  custom_colors <- c("#E31A1C", "#1F78B4", "#33A02C")
  options$set_colors(custom_colors)
  
  custom_color_plot <- nightowl::plot(
    data = visual_test_data[1:30, ],
    mapping = list(
      x = "group", 
      y = "numeric_value",
      color = "group"
    ),
    layers = list(list(type = "generic", geom = "ggplot2::geom_point", size = 3))
  )
  vdiffr::expect_doppelganger("custom_color_plot", custom_color_plot)
  
  # Reset colors
  options$set_colors(original_colors)
})

# Edge Case Visual Tests ====
test_that("edge cases render appropriately", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  
  # Small dataset
  small_data <- visual_test_data[1:5, ]
  small_plot <- nightowl::plot(
    data = small_data,
    mapping = list(x = "group", y = "numeric_value"),
    layers = list(list(type = "generic", geom = "ggplot2::geom_point", size = 4))
  )
  vdiffr::expect_doppelganger("small_dataset_plot", small_plot)
  
  # Single group
  single_group_data <- visual_test_data[visual_test_data$group == "Group_A", ][1:10, ]
  single_group_plot <- nightowl::plot(
    data = single_group_data,
    mapping = list(x = "category", y = "numeric_value"),
    layers = list(list(type = "boxplot"))
  )
  vdiffr::expect_doppelganger("single_group_plot", single_group_plot)
})

# Multi-format Consistency Tests ====
test_that("plots maintain consistency across output formats", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  
  # Create a standard plot
  standard_plot <- nightowl::plot(
    data = visual_test_data[1:25, ],
    mapping = list(
      x = "numeric_value",
      y = "weight", 
      color = "group"
    ),
    layers = list(
      list(type = "generic", geom = "ggplot2::geom_point"),
      list(type = "smooth")
    )
  )
  
  # Test ggplot extraction consistency
  ggplot_extracted <- nightowl::as_ggplot(standard_plot)
  vdiffr::expect_doppelganger("ggplot_extracted", ggplot_extracted)
  
  # Test Plot class wrapper consistency
  plot_wrapped <- nightowl::Plot$new(plot = ggplot_extracted)
  plot_ggplot <- plot_wrapped$ggplot()
  vdiffr::expect_doppelganger("plot_wrapped_ggplot", plot_ggplot)
})