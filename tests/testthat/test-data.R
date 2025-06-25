test_that("outlier detection works with numeric vectors containing outliers", {
  x <- c(runif(100), 100)
  x
  nightowl::detect_outliers(x)
})
