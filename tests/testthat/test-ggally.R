test_that("ggally works", {
  library(magrittr)
  testdata <- mtcars[, c(1, 3, 4, 5, 6, 7, 8)]
  nightowl::ggpairs(testdata,
    mapping = list(
      key = names(testdata),
      color = "vs",
      group = "vs"
    )
  )

  nightowl::ggpairs(ChickWeight,
    mapping = list(
      key = "Time",
      value = "weight",
      id = "Chick"
    )
  )

  nightowl::ggpairs(ChickWeight,
    transform = list(color = factor),
    mapping = list(
      key = "Time",
      value = "weight",
      id = "Chick",
      color = "Diet"
    )
  )
})
