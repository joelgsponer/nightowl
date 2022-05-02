test_that("survival functions work", {
  testdata <- tibble::tibble(
    time = sample(1:100, size = 100, replace = TRUE),
    event = sample(c(0, 1), size = 100, replace = TRUE),
    treatment = sample(c("NO", "YES"), size = 100, replace = TRUE),
    c1 = sample(c("NO", "YES"), size = 100, replace = TRUE),
    c2 = sample(c("NO", "YES"), size = 100, replace = TRUE),
    c3 = sample(c("YES"), size = 100, replace = TRUE),
    s1 = sample(c("NO", "YES"), size = 100, replace = TRUE),
    s2 = sample(c("NO", "YES"), size = 100, replace = TRUE),
  )

  nightowl::create_Surv_formula(testdata,
    time = "time",
    event = "event",
    treatment = "treatment",
    covariates = c("c1", "c2", "c3"),
    strata = c("s1", "s2")
  )
})
