test_that("ggally works", {
  testdata <- mtcars[, c(1, 3, 4, 5, 6, 7)]
  nightowl::ggpairs(testdata,
    mapping = list(
      key = names(testdata)
    ),
    svg = list()
  ) %>%
    testthat::expect_s3_class(c("gg", "ggmatrix"))

  nightowl::ggpairs(ChickWeight,
    mapping = list(
      key = "Time",
      value = "weight",
      id = "Chick"
    )
  ) %>%
    testthat::expect_s3_class(c("gg", "ggmatrix"))
})
