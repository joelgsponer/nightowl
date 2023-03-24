test_that("coxph works", {

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

  # Using the veterans dataset for reproducibility
  # Variables:
  #   - trt
  #   - celltype
  #   - time
  #   - status
  #   - karno
  #   - diagtime
  #   - age
  #   - prior
  veterans <- survival::veteran %>%
    tibble::as_tibble() %>%
    dplyr::mutate(trt = factor(trt)) %>%
    dplyr::mutate(celltype = factor(celltype)) %>%
    dplyr::mutate(prior = factor(prior))
  levels(veterans$trt) <- c("A", "B")
  levels(veterans$prior) <- c("No prior treament", "Prior treatment")

  # Basic
  basic <- nightowl::Coxph$new(
    data = veterans,
    time = "time",
    event = "status",
    treatment = "trt",
    group_by = "prior",
    covariates = c("karno")
  )
  basic$output(keep_only_treatment = FALSE)

  basic$kable()
  basic$kable(keep_only_treatment = FALSE)

  res <- basic$raw()
  compare <- function(level) {
    HR <- res %>%
      dplyr::filter(prior == level) %>%
      dplyr::pull(`Hazard Ratio`) %>%
      stringr::str_split(., " ")
    HR <- HR[[1]][1]
    # Comparison
    test <-
      survival::coxph(
        survival::Surv(time, status) ~ trt + karno,
        data = veterans %>%
          dplyr::filter(prior == level)
      ) %>%
      broom::tidy() %>%
      dplyr::filter(term == "trtB") %>%
      dplyr::pull(estimate) %>%
      exp()
    test <- test %>%
      round(3) %>%
      as.character()
    testthat::expect_equal(HR, test)
  }
  compare("No prior treament")
  compare("Prior treatment")
  #---------------------------

  # Numeric
  basic_numeric <- nightowl::Coxph$new(
    data = veterans,
    time = "time",
    event = "status",
    treatment = "karno"
  )
  basic_numeric$N()
  basic_numeric$output()

  # Interaction
  tmp <- nightowl::Coxph$new(
    data = testdata,
    time = "time",
    event = "event",
    treatment = "treatment",
    covariates = c("c1", "c2", "s2"),
    interactions = c("treatment:s2")
  )
  tmp

  survival::coxph(survival::Surv(time, event) ~ treatment + c1 + c2 + s2 + treatment:s2, data = testdata)

  tmp$models

  tmp$results()

  tmp$get_variables()

  tmp$formula



  tmp$output()

  tmp <- nightowl::Coxph$new(
    data = testdata,
    time = "time",
    event = "event",
    treatment = "treatment",
    covariates = c("c1", "c2", "c3"),
    interactions = c("treatment:s2")
  )
  tmp$models

  tmp$results()

  tmp <- nightowl::Coxph$new(
    data = veterans,
    time = "time",
    event = "status",
    treatment = "trt",
    covariates = c("celltype"),
    interactions = c("trt:celltype")
  )
  tmp$models
  tmp$results()
  tmp$output()
  tmp$coefficients()
  tmp$se()
  tmp$TE()
  tmp$seTE()
  tmp$TE("celltype")

  tmp <- nightowl::Coxph$new(
    data = veterans,
    time = "time",
    event = "status",
    treatment = "trt",
    covariates = c("age"),
    group_by = "celltype",
    interactions = c("trt:karno")
  )

  tmp$models
  tmp$get_reference()
  tmp$results()
  tmp$output()
  tmp$coefficients()
  tmp$se()
  tmp$TE()
  tmp$TE("age")
  tmp$seTE()
  tmp$seTE("age")
  tmp$metagen()
  tmp$metagen_results()
  tmp$metagen_summarise()
  tmp$metagen_raw()
  tmp$metagen_kable()
  tmp$metagen_output()


  # this needs work but maybe it is out of scope of this object or a separate output
  tmp <- nightowl::Coxph$new(
    data = veterans,
    time = "time",
    event = "status",
    treatment = "trt",
    covariates = c("karno", "age"),
    interactions = c("trt:karno")
  )
  tmp$models

  tmp$formula
  coefficients(tmp$models$Overal$result)

  cmod <- survival::coxph(survival::Surv(time, status) ~ trt + karno + age + trt:karno, data = veterans)
  cmod
  testthat::expect_equal(coefficients(tmp$models$Overall$result), coefficients(cmod))

  mm <-
    emmeans::emmeans(cmod, pairwise ~ karno | trt, weights = "proportional")
  broom::tidy(conf.int = TRUE) %>%
    dplyr::mutate(HR = exp(estimate))

  tmp <- nightowl::Coxph$new(
    data = veterans,
    time = "time",
    event = "status",
    treatment = "karno",
    group_by = "trt"
  )
  tmp$results()


  # Simulation
  t <- function(trt, bm, btrt = 0.5, bbm = 0.5, bint = 0.5) {
    1 * exp(btrt * trt + bm * bbm + trt * bm * bint)
  }
  sim <- tibble::tibble(
    trt = sample(c(0, 1), size = 1000, replace = TRUE),
    bm = rnorm(1000, 0, 1),
    time = rexp(1000, t(trt, bm)),
    status = sample(0:1, size = 1000, replace = TRUE, prob = c(0.1, 0.9)),
  )
  nightowl::plot_km(sim, time = "time", event = "status", treatment = "trt")
  cmod <- survival::coxph(survival::Surv(time, status) ~ factor(trt) + bm + factor(trt):bm, data = sim)


  c1 <- survival::coxph(survival::Surv(time, status) ~ factor(trt) + bm + factor(trt):bm, data = sim)
  c1

  c4 <- survival::coxph(survival::Surv(time, status) ~ factor(trt) + factor(trt):bm, data = sim)
  c4

  c2 <- survival::coxph(survival::Surv(time, status) ~ factor(trt), data = sim)
  c2

  c3 <- survival::coxph(survival::Surv(time, status) ~ factor(trt):bm, data = sim)
  c3

  nightowl::coefficients(list(A = cmod))
  nightowl::coefficients(list(A = cmod, b = cmod))
  nightowl::se(list(A = cmod))
  nightowl::se(list(A = cmod, b = cmod))
  meta <- nightowl::metagen(list(A = c1, B = c2), "bm")

  nightowl::metagen(list(A = c1, b = c2), "bm") %>%
    metafor::forest()

  nightowl::metagen(list(A = cmod, b = cmod), "bm") %>% as.data.frame()

  nightowl::metagen(list(A = cmod, b = cmod), "bm") %>%
    nightowl::metagen_tidy()




  p <- expand.grid(trt = c("A", "B"), bm = seq(0, 10, 0.1))
  tibble::tibble(p, fit = predict(cmod, p)) %>%
    ggplot2::ggplot(ggplot2::aes(x = bm, y = fit, color = trt)) +
    ggplot2::geom_line()



  emmeans::emmeans(cmod, pairwise ~ bm:trt, weights = "proportional")

  #
  #
  emmeans::emmip(tmp$models[[1]]$result, karno ~ trt, adjust = "tukey")
  #
  # emmeans::emmeans(.result, as.formula(paste("pairwise ~", variables$interactions)), weights = "proportional", nesting = NULL)
  #
  # $emmeans %>%
  #   broom::tidy(conf.int = TRUE) %>%
  #   dplyr::mutate(HR = exp(estimate))
  #
  #
  # tern::h_coxreg_inter_estimations(
  #  variable = variables$treatment, given = "celltype",
  #    lvl_var = levels(self$data$trt),
  #    lvl_given = levels(self$data$celltype),
  #    mod = .result, conf_level = .95
  # )


  nightowl::plot_grouped_km(
    data = veterans,
    time = "time",
    event = "status",
    split = "celltype",
    treatment = "trt",
  )


  # Interactions should be defined using : not *
  testthat::expect_error(nightowl::Coxph$new(
    data = testdata,
    time = "time",
    event = "event",
    treatment = "treatment",
    covariates = c("c1", "c2", "c3"),
    interactions = c("treatment*n1")
  ))


  #---------------------------

  testdata <- random.cdisc.data::cadtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(EVENT = 1 - CNSR) %>%
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
