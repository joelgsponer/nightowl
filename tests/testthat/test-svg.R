test_that("svg works", {
  library(magrittr)
  testdata <- ChickWeight %>%
    dplyr::filter(Time < 10) %>%
    dplyr::mutate(`thisisareally reallylongvariablename` = Diet)

  .p <- nightowl::plot(testdata,
    transform = list(x = "factor"),
    mapping = list(
      x = "Time",
      y = "weight",
      color = "thisisareally reallylongvariablename",
      group = "thisisareally reallylongvariablename",
      id = "Chick",
      lty = "thisisareally reallylongvariablename"
    ),
    layers = list(
      list(type = "traces", geom = "line", alpha = 0.3),
      list(type = "summary", mapping = list(color = NULL), geom = "line", dodge = 0, size = 1.5)
    )
  )
  # ggplot2::guides(colour = ggplot2::guide_legend(nrow = 2))

  .p <- .p + MetBrewer::scale_color_met_d("Demuth")


  a <- nightowl::render_svg(.p, height = 4, width = 4, scaling = 0.5)

  nightowl::render_svg(.p, height = 4, width = 8, scaling = 0.5)
  nightowl::render_svg(.p, height = 3, width = 8, scaling = 0.5, add_download_button = F)

  test_svg <- nightowl::new_svg("test")
  test_svg
  showme(test_svg)

  a <- 1
  class(a) <- "test_class"
  print.nightowl_svg <- function(x) {
    print(paste("test_class: ", class(x)))
  }
  showme(test_svg)
  print(test_svg)
})
