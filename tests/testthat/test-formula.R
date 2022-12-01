test_that("create_formula works", {

   testdata <- mtcars
   nightowl::create_Surv_formula(testdata,
     treatment = "gear",
     time = "mpg",
     event = "cyl"
   )

   testdata <- mtcars
   nightowl::create_formula(testdata,
     response = "cyl",
     covariates = c("wt", "mpg"),
     strata = "am",
     random = "cyl"
   )


})
