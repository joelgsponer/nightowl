test_that("line plot works", {

  library(magrittr)
  testdata <- datasets::ChickWeight %>%
    tibble::as_tibble() %>%
    dplyr::mutate(`Diet 2` = Diet)

  nightowl::line_plot(testdata,
    x = "Time",
    y = "weight",
    fill = "Diet",
    color = "Diet",
    id = "Chick",
    facet_row = "Diet 2",
    theme = "ggplot2::theme_classic",
    ylab = "THIS IS A LABEL",
    notused = "notused"
  )

  nightowl::line_plot(testdata,
    x = "Time",
    y = "weight",
    id = "Chick",
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
    method_smooth = "lm",
    add_whiskers = F
  )
})
