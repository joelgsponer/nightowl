# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
create_Surv_formula <- function(data, time, event, treatment, covariates = NULL, strata = NULL, random_effects = NULL) {
  stopifnot(length(treatment) == 1)
  # Remove time and event if present in covariates or strata -------------------
  # Covariates and strata need to have at least two levels ---------------------
  if (!is.null(covariates)) {
    covariates <- waRRior::pop(covariates, c(time, event))
    covariates <- purrr::map(covariates, function(.covariate) {
      if (waRRior::length_unique(data[[.covariate]]) > 1) {
        return(.covariate)
      } else {
        cli::cli_alert("🦉⛔ Covariate `{.covariate}` has only one level. Skipping.")
        return(NULL)
      }
    }) %>%
      purrr::compact()
  }
  if (!is.null(strata)) {
    strata <- waRRior::pop(strata, c(time, event))
    strata <- purrr::map(strata, function(.stratum) {
      if (waRRior::length_unique(data[[.stratum]]) > 1) {
        return(.stratum)
      } else {
        cli::cli_alert("🦉⛔ Stratum `{.stratum}` has only one level. Skipping.")
        return(NULL)
      }
    }) %>%
      purrr::compact()
  }
  # if (!is.null(random_effects)) {
  #   random_effects <- waRRior::pop(effects, c(time, event))
  #   random_effects <- purrr::map(random_effects, function(.random_effect) {
  #     if (waRRior::length_unique(data[[.random_effect]]) > 1) {
  #       return(.random_effect)
  #     } else {
  #       cli::cli_alert("🦉⛔ Random Effect `{.random_effect}` has only one level. Skipping.")
  #       return(NULL)
  #     }
  #   }) %>%
  #     purrr::compact()
  # }
  # Put everything together ---------------------------------------------------
  str_covariates <- if (!is.null(covariates)) {
    paste0(" + ", covariates, collapse = " ")
  } else {
    NULL
  }
  str_strata <- if (!is.null(strata)) {
    paste0(" + strata(", strata, ")", collapse = " ")
  } else {
    NULL
  }
  str_random_effects <- if (!is.null(random_effects)) {
    paste0(" + (1|", random_effects, ")", collapse = "", sep = "")
  } else {
    NULL
  }
  base <- glue::glue("survival::Surv({time}, {event}) ~ {paste(treatment, str_covariates, str_strata, str_random_effects, collapse = '', sep = '')}")
  cli::cli_alert("🦉 Formula: `{base}`")
  as.formula(base)
}
# ===============================================================================
# .fits <- CHRONO %>%
#   droplevels() %>%
#   waRRior::named_group_split(STUDY) %>%
#   purrr::map(function(.data) {
#     nightowl::create_Surv_formula(
#       data = .data,
#       time = "AVAL_PFS",
#       event = "EVENT_PFS",
#       treatment = "hour",
#       covariates = names(covariates)
#     ) %>%
#       survival::coxph(data = .data)
#   })
# .beta <- .fits %>%
#   purrr::map(function(.fit) {
#     .fit$coef[1]
#   }) %>%
#   unlist()
# .se <- .fits %>%
#   purrr::map(function(.fit) {
#     sqrt(vcov(.fit)[1, 1])
#   }) %>%
#   unlist()
# .studies <- tibble::tibble(STUDY = names(.fits))
# .meta <- meta::metagen(
#   .beta,
#   .se,
#   data = .studies,
#   studlab = STUDY,
#   fixed = FALSE,
#   random = TRUE,
#   method.tau = "SJ",
#   hakn = TRUE,
#   prediction = TRUE,
#   sm = "HR"
# )
# meta::forest(.meta)
