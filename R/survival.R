# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
create_Surv_formula <- function(data, time, event, treatment, covariates = NULL, strata = NULL) {
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
  base <- glue::glue("survival::Surv({time}, {event}) ~ {paste(treatment, str_covariates, str_strata, collapse = '', sep = '')}")
  cli::cli_alert("🦉 Formula: `{base}`")
  as.formula(base)
}
# ===============================================================================
