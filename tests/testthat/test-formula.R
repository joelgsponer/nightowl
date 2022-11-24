test_that("create_formula works", {
  testdata <- mtcars
  nightowl::create_formula(testdata,
    response = "cyl",
    treatment = "gear"
  )

  testdata <- mtcars
  nightowl::create_formula(testdata,
    response = "cyl",
    treatment = "gear",
    covariates = c("wt", "mpg")
  )
})
