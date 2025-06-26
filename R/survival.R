# =================================================
#' @title Create secure survival analysis formula with validation
#' @description
#' Constructs a survival analysis formula with comprehensive input validation
#' and secure handling of variable names to prevent code injection vulnerabilities.
#' @param data A data frame containing all specified variables
#' @param time String. Name of the time-to-event variable
#' @param event String. Name of the event indicator variable (0/1 or logical)
#' @param treatment String. Name of the primary treatment variable
#' @param covariates Character vector. Names of covariate variables. Default NULL
#' @param strata Character vector. Names of stratification variables. Default NULL
#' @param random_effects Character vector. Names of random effect variables. Default NULL
#' @return A formula object for use with survival analysis functions (coxph, survreg)
#' @export
create_Surv_formula <- function(data, time, event, treatment, covariates = NULL, strata = NULL, random_effects = NULL) {
  stopifnot(length(treatment) == 1)
  
  # Validate all inputs are valid column names
  all_vars <- c(time, event, treatment, covariates, strata, random_effects)
  if (!all(all_vars %in% names(data))) {
    missing_vars <- all_vars[!all_vars %in% names(data)]
    stop(paste("The following variables are not present in the data:", paste(missing_vars, collapse = ", ")))
  }
  
  # Validate variable names don't contain special characters that could be exploited
  validate_var_names <- function(vars) {
    if (!all(grepl("^[a-zA-Z][a-zA-Z0-9._]*$", vars))) {
      stop("Variable names must start with a letter and contain only letters, numbers, dots, and underscores")
    }
  }
  validate_var_names(all_vars)
  
  # Remove time and event if present in covariates or strata -------------------
  # Covariates and strata need to have at least two levels ---------------------
  if (!is.null(covariates)) {
    covariates <- nightowl_pop(covariates, c(time, event))
    covariates <- purrr::map(covariates, function(.covariate) {
      if (nightowl_length_unique(data[[.covariate]]) > 1) {
        return(.covariate)
      } else {
        nightowl_alert(paste("🦉⛔ Covariate", .covariate, "has only one level. Skipping."))
        return(NULL)
      }
    }) %>%
      purrr::compact()
  }
  if (!is.null(strata)) {
    strata <- nightowl_pop(strata, c(time, event))
    strata <- purrr::map(strata, function(.stratum) {
      if (nightowl_length_unique(data[[.stratum]]) > 1) {
        return(.stratum)
      } else {
        nightowl_alert(paste("🦉⛔ Stratum", .stratum, "has only one level. Skipping."))
        return(NULL)
      }
    }) %>%
      purrr::compact()
  }
  
  # Build formula using safe methods -------------------------------------------
  # Start with the left-hand side (response)
  lhs <- rlang::call2("Surv", rlang::sym(time), rlang::sym(event), .ns = "survival")
  
  # Build the right-hand side (predictors)
  rhs_terms <- list()
  
  # Add treatment
  rhs_terms <- append(rhs_terms, rlang::sym(treatment))
  
  # Add covariates
  if (!is.null(covariates) && length(covariates) > 0) {
    covariate_syms <- purrr::map(covariates, rlang::sym)
    rhs_terms <- append(rhs_terms, covariate_syms)
  }
  
  # Add strata terms
  if (!is.null(strata) && length(strata) > 0) {
    strata_terms <- purrr::map(strata, function(s) {
      rlang::call2("strata", rlang::sym(s))
    })
    rhs_terms <- append(rhs_terms, strata_terms)
  }
  
  # Add random effects
  if (!is.null(random_effects) && length(random_effects) > 0) {
    re_terms <- purrr::map(random_effects, function(re) {
      # Create (1|re) structure
      rlang::call2("||", 1, rlang::sym(re))
    })
    rhs_terms <- append(rhs_terms, re_terms)
  }
  
  # Combine all RHS terms with +
  if (length(rhs_terms) == 1) {
    rhs <- rhs_terms[[1]]
  } else {
    rhs <- purrr::reduce(rhs_terms, function(x, y) rlang::call2("+", x, y))
  }
  
  # Create the formula
  formula_obj <- rlang::new_formula(lhs, rhs)
  
  # Display the formula (safely)
  formula_str <- rlang::expr_text(formula_obj)
  nightowl_alert(paste("🦉 Formula:", formula_str))
  
  return(formula_obj)
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
