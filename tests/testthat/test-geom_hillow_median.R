test_that("geom hillow median works", {
  testdata <- tibble::tibble(
    X = rep(seq(1, 5), 2),
    Y = c(seq(1, 5), seq(5, 1))
  )
  testdata
  ggplot2::ggplot(
    testdata,
    ggplot2::aes(x = X, y = Y)
  ) +
    ggplot2::geom_line() +
    nightowl::geom_hillow_median()
})
