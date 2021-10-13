test_that("line plot works", {
  testdata <- datasets::ChickWeight %>%
    tibble::as_tibble()

  nightowl::line_plot(testdata,
    x = "Time",
    y = "weight",
    fill = "Diet",
    color = "Diet",
    id = "Chick",
    facet_row = "Diet",
    theme = "ggplot2::theme_classic"
  )

  nightowl::line_plot(testdata,
    x = "Time",
    y = "weight",
    id = "Chick",
    facet_row = "Diet",
    title = "auto",
    summarise_y = "mean"
  )

  nightowl::line_plot(testdata,
    x = "Time",
    y = "weight",
    id = "Chick",
    facet_col = "Diet",
    title = "auto",
    summarise_y = "min",
    scales = "free_y"
  )

  nightowl::line_plot(testdata,
    x = "Time",
    y = "weight",
    fill = "Diet",
    id = "Chick",
    facet_col = "Diet",
    title = "auto",
    summarise_y = "min",
    scales = "free_y",
    method_smooth = "median",
    add_whiskers = F
  )

  nightowl::line_plot(testdata,
    x = "Time",
    y = "weight",
    fill = "Diet",
    id = "Chick",
    title = "auto",
    summarise_y = "min",
    scales = "free_y",
    method_smooth = "median",
    add_whiskers = F
  )
})
