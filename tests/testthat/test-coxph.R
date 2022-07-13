test_that("coxph works", {

  testdata <- tibble::tibble(
    time = sample(1:100, size = 100, replace = TRUE),
    event = sample(c(0, 1), size = 100, replace = TRUE),
    treatment = sample(c("NO", "YES"), size = 100, replace = TRUE),
    c1 = sample(c("NO", "YES"), size = 100, replace = TRUE),
    c2 = sample(c("NO", "YES", "MAYBE"), size = 100, replace = TRUE),
    c3 = factor(sample(c("YES"), size = 100, replace = TRUE)),
    n1 = runif(100, 0, 1),
    s1 = sample(c("NO", "YES"), size = 100, replace = TRUE),
    s2 = sample(c("NO", "YES"), size = 100, replace = TRUE),
  )

  nightowl::fit_coxph(testdata,
    time = "time",
    event = "event",
    treatment = "treatment",
    covariates = c("n1", "c1", "c2", "c3"),
    strata = c("s1", "s2"),
    exponentiate = FALSE
  )

  tmp <- nightowl::plot_coxph(testdata,
    time = "time",
    event = "event",
    treatment = "treatment",
    covariates = c("n1", "c1", "c2", "c3"),
    exponentiate = FALSE
  )
  nightowl::render_kable(tmp)


  nightowl::plot_coxph(testdata,
    time = "time",
    event = "event",
    treatment = "n1"
  )

  nightowl::plot_coxph(testdata,
    time = "time",
    event = "event",
    treatment = "treatment",
    covariates = c("n1", "c1", "c2", "c3"),
    strata = c("s1", "s2"),
    labels = c(n1 = "hello")
  )

  nightowl::plot_coxph(testdata,
    time = "time",
    event = "event",
    treatment = "treatment",
    covariates = c("n1", "c1", "c2", "c3"),
    strata = c("s1", "s2"),
    show_only_treatment = TRUE,
    plan = "multicore",
    conf_range = c(-0.1, 0.1)
  )

  nightowl::plot_coxph(testdata,
    time = "time",
    event = "event",
    treatment = "treatment",
    covariates = c("c2", "c3"),
    strata = c("s1", "s2"),
    split = "c1",
    title = "title"
  )


  nightowl::plot_coxph(testdata,
    time = "time",
    event = "event",
    treatment = "treatment",
    covariates = c("c1", "c2", "c3"),
    strata = c("s1", "s2"),
    engine = "reactable"
  )

  nightowl::plot_coxph(testdata,
    time = "time",
    event = "event",
    treatment = "treatment",
    covariates = c("c1", "c2", "c3"),
    strata = c("s1", "s2"),
    engine = "reactable",
    conf_range = c(-1, 1)
  )

  nightowl::forestplot(1.5, 0, 2.1, xlim = c(1.1, 2)) %>%
    htmltools::browsable()
  nightowl::forestplot(1, 0, 2.1, xlim = c(1.1, 2)) %>%
    htmltools::browsable()
  nightowl::forestplot(1.4, 1.1, 2.1, xlim = c(1.1, 2)) %>%
    htmltools::browsable()
  nightowl::forestplot(1.4, 1.1, 2.1, xlim = c(1.1, 2), log = T) %>%
    htmltools::browsable()
})
