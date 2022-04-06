test_that("svg works", {
  library(magrittr)
  testdata <- ChickWeight %>%
    dplyr::filter(Time < 10)

  .p <- nightowl::plot(testdata,
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
  )

  nightowl::render_svg(.p, height = 4, width = 4, scaling = 1)
  nightowl::render_svg(.p, height = 4, width = 8, scaling = 0.5)
  nightowl::render_svg(.p, height = 3, width = 8, scaling = 0.5, add_download_button = F)
})
