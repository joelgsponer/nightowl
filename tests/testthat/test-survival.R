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
    n1 = runif(100),
    split = sample(LETTERS[1:10], size = 100, replace = TRUE)
  )

  nightowl::Coxph$new(
    data = testdata,
    time = "time",
    event = "event",
    treatment = "treatment",
    group_by = "s2",
    covariates = c("c1", "c2", "c3"),
    strata = c("s1", "s2"),
    random_effect = c("split", "Second")
  )

  tmp <- nightowl::Coxph$new(
    data = testdata,
    time = "time",
    event = "event",
    treatment = "treatment",
    group_by = c("s1", "s2"),
    covariates = c("c1", "c2", "c3"),
    random_effect = c("split", "Second"),
    labels = c(c1 = "hello")
    # conf_range = c(-2, 2)
  )
  tmp$kable()

  tmp$reactable()


  nightowl::Coxph$new(
    data = testdata,
    time = "missing",
    event = "event",
    treatment = "treatment",
    group_by = "s2",
    covariates = c("c1", "c2", "c3"),
    strata = c("s1", "s2"),
    random_effect = c("split", "Second")
  )

  nightowl::Coxph$new(
    data = dplyr::group_by(testdata, s2),
    time = "time",
    event = "event",
    treatment = "treatment",
    covariates = c("c1", "c2", "c3"),
    strata = c("s1", "s2"),
    random_effect = c("split", "Second")
  )

  no_data <- nightowl::Coxph$new(
    time = "time",
    event = "event",
    treatment = "treatment",
    covariates = c("c1", "c2", "c3"),
    strata = c("s1", "s2"),
    random_effect = c("split", "Second")
  )
  no_data$set_data(testdata)
  no_data


  nightowl::create_Surv_formula(testdata,
    time = "time",
    event = "event",
    treatment = "treatment",
    covariates = c("c1", "c2", "c3"),
    strata = c("s1", "s2"),
    random_effect = c("split", "Second")
  )

  nightowl::create_Surv_formula(testdata,
    time = "time",
    event = "event",
    treatment = "treatment",
    covariates = c("TRT")
  )

  nightowl::create_Surv_formula(testdata,
    time = "time",
    event = "event",
    treatment = "treatment",
    covariates = c("c1", "c2", "c3", "n1"),
  )

  nightowl::create_Surv_formula(testdata,
    time = "time",
    event = "event",
    treatment = "treatment"
  )

  nightowl::create_Surv_formula(testdata,
    time = "time",
    event = "event",
    treatment = "treatment"
  ) %>%
    nightowl::km_pvalue(testdata)

  nightowl::create_Surv_formula(testdata,
    time = "time",
    event = "event",
    treatment = "treatment"
  ) %>%
    nightowl::km_summary(testdata)

  nightowl::fit_km(testdata,
    time = "time",
    event = "event",
    treatment = "treatment"
  )

  nightowl::fit_km(testdata,
    time = "time",
    event = "event",
    treatment = "treatment"
  ) %>% nightowl::km_table()

  nightowl::fit_km(testdata,
    time = "time",
    event = "event",
    treatment = "treatment"
  ) %>% nightowl::km_add_0()

  nightowl::plot_km(testdata,
    time = "time",
    event = "event",
    treatment = "treatment",
    break_width = 20,
    note = "This is a note"
  )

  nightowl::plot_km(testdata,
    time = "time",
    event = "event",
    treatment = "treatment",
    break_width = 20,
    lowrider_theme = "roche"
  )

  nightowl::plot_km(testdata,
    time = "time",
    event = "event",
    treatment = "treatment",
    break_width = 20,
    lowrider_theme = "roche",
    as_ggplot = TRUE
  )

  nightowl::plot_km(testdata,
    time = "time",
    event = "event",
    treatment = "treatment",
    break_width = 20,
    lowrider_theme = "roche",
    as_ggplot = TRUE
  )


  nightowl::plot_grouped_km(testdata,
    time = "time",
    event = "event",
    treatment = "treatment",
    split = "c1",
    subtitle = function() "Hello"
  )

  nightowl::plot_grouped_km(testdata,
    time = "time",
    event = "event",
    treatment = "treatment",
    split = "c1",
    as_ggplot = T,
    subtitle = function() "Hello"
  )

  nightowl::plot_grouped_km_compact(testdata,
    time = "time",
    event = "event",
    treatment = "treatment",
    split = "split",
    width = "300px"
  )

  nightowl::plot_grouped_km_compact(testdata,
    time = "time",
    event = "event",
    treatment = "treatment",
    split = "split",
    width = "300px",
    as_ggplot = TRUE
  )
})
