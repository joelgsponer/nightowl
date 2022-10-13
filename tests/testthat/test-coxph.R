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
  
  #Basic
  tmp <- nightowl::Coxph$new(
    data = testdata,
    time = "time",
    event = "event",
    treatment = "treatment",
    group_by = "s2",
    covariates = c("c1", "c2", "c3"),
    random_effect = c("split", "Second")
  )
  tmp$output()

  #Interaction
  tmp <- nightowl::Coxph$new(
    data = testdata,
    time = "time",
    event = "event",
    treatment = c("treatment", "s2"),
    covariates = c("c1", "c2", "c3"),
    random_effect = c("split", "Second")
  )
  tmp$output()

  testdata <- random.cdisc.data::cadtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(EVENT = 1 - CNSR ) %>%
    dplyr::mutate(STUDY = sample(LETTERS[1:3], size = nrow(.), replace = TRUE, prob = c(0.2, 0.5, 0.3)))

  tmp <- nightowl::Coxph$new(
    data = testdata,
    time = "AVAL",
    event = "EVENT",
    treatment = "AGE",
    group_by = c("STUDY"),
    covariates = c("SEX", "RACE"),
    labels = c(`SEX` = "Sex", RACE = "Ethnicity")
    # conf_range = c(-2, 2)
  )


  tmp$output()

  tmp$coefficients()
  tmp$TE()
  tmp$se()
  tmp$TE()

  tmp$metagen()
  tmp$metagen_results()
  tmp$metagen_errors()
  tmp$raw()
  tmp$metagen_summarise()
  tmp$metagen_raw()
  tmp$metagen_kable()
  tmp$metagen_output()

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
})
