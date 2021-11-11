test_that("multiplication works", {
  testdata <- ChickWeight %>%
    dplyr::filter(Time < 10)
  nightowl::plot(testdata,
    mapping = list(
      x = "Time",
      y = "weight",
      fill = "Diet"
    ),
    boxplot = list(),
    violin = list(),
    dotplot = list(),
    points = list(),
    summary = list(),
    dodge = 0.5
  )

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
