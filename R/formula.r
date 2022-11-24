# =================================================
#' @title
#' MISSING_TITLE
#' @param data A data frame.
#' @param time `string` the time variable
#' @param event `string` the event variable
#' @param treatment `string` the treatment variable
#' @param covariates `character vector` of covariate variables
#' @param strata `character vector` of strata variables
#' @param random_effects `character vector` of random effects variables
#' @param interactions `list` of interaction terms
#' @export
create_Surv_formula <- function(data,
                                time,
                                event,
                                treatment,
                                covariates = NULL,
                                strata = NULL,
                                interactions = NULL,
                                random_effects = NULL) {
  # Remove time and event if present in covariates or strata -------------------
  # Covariates and strata need to have at least two levels ---------------------
  if (!is.null(covariates)) {
    covariates <- waRRior::pop(covariates, c(time, event))
    covariates <- purrr::map(covariates, function(.covariate) {
      if (waRRior::length_unique(data[[.covariate]]) > 1) {
        return(.covariate)
      } else {
        cli::cli_alert_warning("Covariate `{(.covariate)}` has only one level. Skipping.")
        return(NULL)
      }
    }) %>%
      purrr::compact()
  }
  if (!is.null(interactions)) {
    # Code to check if interactions are valid
  }
  if (!is.null(strata)) {
    strata <- waRRior::pop(strata, c(time, event))
    strata <- purrr::map(strata, function(.stratum) {
      if (waRRior::length_unique(data[[.stratum]]) > 1) {
        return(.stratum)
      } else {
        cli::cli_alert_warning(" Stratum `{(.stratum)}` has only one level. Skipping.")
        return(NULL)
      }
    }) %>%
      purrr::compact()
  }
  # Put everything together ---------------------------------------------------
  str_treatment <- if (!is.null(treatment) && length(treatment) > 0) {
    paste0(" + `", treatment, "`", collapse = " ")
  } else {
    rlang::abort("🦉⛔ No treatment provided.")
  }
  str_covariates <- if (!is.null(covariates) && length(covariates) > 0) {
    paste0(" + `", covariates, "`", collapse = " ")
  } else {
    NULL
  }
  str_strata <- if (!is.null(strata) && length(strata) > 0) {
    paste0(" + strata(`", strata, "`)", collapse = " ")
  } else {
    NULL
  }
  str_interactions <- if (!is.null(interactions) && length(interactions) > 0) {
    paste0(" + ", interactions, collapse = " ")
  } else {
    NULL
  }
  str_random_effects <- if (!is.null(random_effects) && length(random_effects) > 0) {
    paste0(" + (1|`", random_effects, "`)", collapse = "", sep = "")
  } else {
    NULL
  }
  base <- glue::glue("survival::Surv({time}, {event}) ~ {paste(str_treatment, str_covariates, str_strata, str_interactions, str_random_effects, collapse = '', sep = '')}")
  cli::cli_alert("🦉 Formula: {base}")
  as.formula(base)
}

