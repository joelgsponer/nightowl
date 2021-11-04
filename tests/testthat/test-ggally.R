test_that("ggally works", {

  testdata <- mtcars[, c(1,3,4,5,6,7)]
  GGally::ggcorr(testdata)
  GGally::ggpairs(testdata)
  
})
