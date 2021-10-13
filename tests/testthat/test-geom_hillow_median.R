test_that("geom hillow median works", {
  testdata <- palmerpenguins::penguins
  testdata

  ggplot2::ggplot(
    testdata,
    ggplot2::aes(x = year, y = bill_length_mm)
  ) +
    ggplot2::geom_line() +
    nightowl::geom_hillow_median()
})
