test_that("ggally works", {

  testdata <- mtcars[, c(1, 3, 4, 5, 6, 7)]
  nightowl::ggpairs(testdata,
    key = names(testdata),
  )
  nightowl::ggpairs(ChickWeight,
    key = c("Time"),
    value = "weight",
    id = "Chick"
  )
})
