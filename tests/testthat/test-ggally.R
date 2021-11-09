test_that("ggally works", {
  testdata <- mtcars[, c(1, 3, 4, 5, 6, 7)]
  nightowl::ggpairs(testdata,
    key = names(testdata),
  ) %>%
    testthat::expect_s3_class(c("gg", "ggmatrix"))

  nightowl::ggpairs(ChickWeight,
    key = c("Time"),
    value = "weight",
    id = "Chick"
  ) %>%
    testthat::expect_s3_class(c("gg", "ggmatrix"))
})
