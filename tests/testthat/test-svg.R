test_that("svg works", {
  library(magrittr)
  testdata <- ChickWeight %>%
    dplyr::filter(Time < 10)
  nightowl::plot(testdata,
    transform = list(x = "factor"),
    mapping = list(
      x = "Time",
      y = "weight",
      color = "Diet",
      group = "Diet",
      id = "Chick",
      lty = "Diet"
    ),
    layers = list(
      list(type = "traces", geom = "line", alpha = 0.3),
      list(type = "summary", mapping = list(color = NULL), geom = "line", dodge = 0, size = 1.5)
    )
  ) %>%
    nightowl::render_svg(height = 50, width = 40, scaling = 3) %>%
    replace_all("font-family", "Lato") ->
  a
})
