test_that("multiplication works", {

  testdata <- ChickWeight %>%
    dplyr::filter(Time < 10)
  nightowl::plot(testdata,
    mapping = list(
      x = "Time",
      y = "weight",
      fill = "Diet",
      lty = "Diet"
    ),
    boxplot = list(),
    violin = list(),
    dotplot = list(),
    points = list(),
    smooth = list(lm = list()),
    summary = list(line = list()),
    annotation = list(title = "TEST"),
    axis = list(log_y = TRUE, xlim = c(2, 4), units_x = "days", units_y = "g"),
    dodge = 0.5
  ) %>%
  peek()

  nightowl::plot(testdata,
    mapping = list(
      x = "Time",
      y = "weight",
      id = "Chick",
      color = "Diet"
    ),
    points = list(),
    traces = list(line = list()),
    dodge = 0.5
  )
})
