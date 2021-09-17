test_that("geom hillow median works", {
  testdata <- tibble::tibble(
    X = rep(seq(1, 5), 3),
    Y = c(seq(1, 5), seq(5, 1), seq(5, 1))
  )
  testdata
  ggplot2::ggplot(
    testdata,
    ggplot2::aes(x = X, y = Y)
  ) +
    ggplot2::geom_line() +
    nightowl::geom_bootstrap_mean()

  testdata <- tibble::tibble(
    X = sample(seq(1, 5), 100, replace = T),
    Y = runif(100)
  )
  testdata
  ggplot2::ggplot(
    testdata,
    ggplot2::aes(x = X, y = Y)
  ) +
    ggplot2::geom_line() +
    nightowl::geom_bootstrap_mean()
})
