# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
create_Surv_formula <- function(data, time, event, treatment, covariates, strata) {
  stopifnot(length(treatment) == 1)
  # Remove time and event if present in covariates or strata -------------------
  covariates <- waRRior::pop(covariates, c(time, event))
  strata <- waRRior::pop(strata, c(time, event))
  # Covariates and strata need to have at least two levels ---------------------
  covariates <- purrr::map(covariates, function(.covariate) {
    if (waRRior::length_unique(data[[.covariate]]) > 1) {
      return(.covariate)
    } else {
      cli::cli_alert("🦉⛔ Covariate `{.covariate}` has only one level. Skipping.")
      return(NULL)
    }
  }) %>%
    purrr::compact()
  strata <- purrr::map(strata, function(.stratum) {
    if (waRRior::length_unique(data[[.stratum]]) > 1) {
      return(.stratum)
    } else {
      cli::cli_alert("🦉⛔ Stratum `{.stratum}` has only one level. Skipping.")
      return(NULL)
    }
  }) %>%
    purrr::compact()
  # Put everything together ---------------------------------------------------
  str_covariates <- paste0(covariates, collapse = " + ")
  str_strata <- paste0("strata(", strata, ")", collapse = " + ")
  base <- glue::glue("survival::Surv({time}, {event}) ~ {paste(treatment, str_covariates, str_strata, collapse = ' + ', sep = ' + ')}")
  cli::cli_alert("🦉 Formula: `{base}`")
  as.formula(base)
}
# ===============================================================================
