test_that("line plot works", {
  library(magrittr)
  testdata <- datasets::ChickWeight %>%
    tibble::as_tibble() %>%
    dplyr::mutate(`Diet 2` = Diet)

  nightowl::line_plot(testdata,
    x = "Time",
    y = "weight",
    group = "Diet",
    color = "Diet",
    id = "Chick",
    facet_row = "Diet 2",
    theme = "ggplot2::theme_classic",
    ylab = "THIS IS A LABEL",
    log_y = T,
    log_x = T,
    lines_size = 0.1,
    notused = "notused"
  ) %>% peek()

  nightowl::line_plot(testdata,
    x = "Time",
    y = "weight",
    group = "Diet",
    color = "Diet",
    id = "Chick",
    facet_col = "Diet 2",
    scales = "free",
    theme = "ggplot2::theme_classic",
    ylab = "THIS IS A LABEL",
    notused = "notused"
  )

  nightowl::line_plot(testdata,
    x = "Time",
    y = "weight",
    id = "Chick",
    summarise_y = "mean"
  )

  nightowl::line_plot(testdata,
    x = "Time",
    y = "weight",
    title = "auto"
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
    group = "Diet",
    id = "Chick",
    facet_col = "Diet",
    title = "auto",
    summarise_y = "min",
    scales = "free_y",
    add_smooth = "median",
    add_whiskers = F,
    ylim = c(100, 200)
  )

  nightowl::line_plot(testdata,
    x = "Time",
    y = "weight",
    group = "Diet",
    id = "Chick",
    add_smooth = "median",
    add_points = T,
    add_whiskers = T,
    lines_alpha = 0.1,
    points_alpha = 0.1,
    add_violin = T,
    dodge = 2
  )

  nightowl::line_plot(testdata,
    x = "Time",
    y = "weight",
    group = "Diet",
    id = "Chick",
    title = "auto",
    summarise_y = "min",
    scales = "free_y",
    add_smooth = "lm",
    add_whiskers = F
  )

  # Experimental section
  g <- nightowl::line_plot(testdata,
    x = "Time",
    y = "weight",
    group = "Diet",
    color = "Diet",
    id = "Chick",
    facet_col = "Diet 2",
    scales = "free",
    theme = "ggplot2::theme_classic",
    ylab = "THIS IS A LABEL",
    notused = "notused"
  )

  g + ggplot::stat_summary(
    ggplot2::aes_(group = rlang::sym(.aes$id)),
  )
})
