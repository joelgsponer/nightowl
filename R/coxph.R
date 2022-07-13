#' R6 Class
#' @description
#' @detail
#' @importFrom survival strata
#' @export
Coxph <- R6::R6Class("Coxph",
  public = list(
    initialize = function(..., labels = NULL) {
      purrr::imap(list(...), function(.x, .y) {
        if (.y %in% names(self)) {
          self[[.y]] <- .x
        }
      })
      # Set data
      self$set_data()
      self$set_labels(labels)
      self$set_reference()
      self$set_formula()
      self$fit()
    },
    # Print
    print = function() {
      cli::cli_h1("Coxph Object")
      cli::cli_h3("Data")
      print(self$data)
      cli::cli_h3("Formula")
      print(self$formula)
      cli::cli_h3("Variables")
      purrr::iwalk(self$get_variables(), ~ cli::cli_li("{.y}: {.x}"))
      cli::cli_h2("Reference")
      purrr::iwalk(self$get_reference(), ~ cli::cli_li("{.y}: {.x}"))
      cli::cli_h2("Results")
      purrr::iwalk(self$results, function(.x, .y) {
        cli::cli_h3(.y)
        print(.x)
      })
    },
    # Variables
    time = NULL,
    check_time = function() {
      if (is.null(self$time)) {
        rlang::abort("`time` is not set")
      }
    },
    event = NULL,
    check_event = function() {
      if (is.null(self$event)) {
        rlang::abort("`event` is not set")
      }
    },
    treatment = NULL,
    check_treatment = function() {
      if (is.null(self$treatment)) {
        rlang::abort("`treatment` is not set")
      }
    },
    covariates = NULL,
    strata = NULL,
    random_effects = NULL,
    get_variables = function() {
      list(
        time = self$time,
        event = self$event,
        treatment = self$treatment,
        covariates = self$covariates,
        strata = self$strata,
        random_effects = self$random_effects,
        group_by = self$group_by
      )
    },
    # Checks
    check_variables = function() {
      vars <- unlist(unname(self$get_variables()))
      if (!all(vars %in% names(self$data))) {
        missing <- vars[!vars %in% names(self$data)]
        msg <- glue::glue("`{missing}` not present in data")
        rlang::abort(msg)
      }
    },
    # Data
    data = NULL,
    group_by = NULL,
    set_data = function(data = self$data) {
      if (!is.null(data)) {
        self$data <- data
      } else {
        data <- self$data
      }
      if (is.null(self$data)) rlang::abort("No data provided - use `set_data` method to update")
      self$check_variables()
      if (is.null(self$group_by)) self$group_by <- waRRior::get_groups(data)
      data <- data %>%
        dplyr::ungroup() %>%
        dplyr::select_at(c(unname(unlist(self$get_variables())))) %>%
        dplyr::mutate_if(is.character, factor) %>%
        dplyr::group_by_at(self$group_by)
      self$data <- data
      invisible(self)
    },
    check_data = function() {
      if (is.null(self$data)) rlang::abort("No data provided - use `set_data` method to update")
    },
    # Reference ------------------------------------------------
    reference = NULL,
    set_reference = function(data = self$data) {
      if (!is.null(data)) {
        .reference <- data %>%
          dplyr::select_at(unique(c(self$treatment, self$covariates, self$strata, self$random_effects))) %>%
          dplyr::select_if(is.factor) %>%
          droplevels() %>%
          purrr::map(~ levels(.x)[1])
        .numeric <- data %>%
          dplyr::select_if(is.numeric) %>%
          purrr::map(~"")
        self$reference <- c(.reference, .numeric)
      }
      invisible(self)
    },
    get_reference = function() self$reference,
    # Formula ------------------------------------------------------------------
    formula = NULL,
    set_formula = function() {
      self$formula <- nightowl::create_Surv_formula(
        data = self$data,
        time = self$time,
        event = self$event,
        treatment = self$treatment,
        covariates = self$covariates,
        strata = self$strata,
        random_effects = self$random_effects
      )
    },
    check_formula = function() {
      if (is.null(self$formula)) {
        rlang::abort("No formula provided - use `set_formula` method to update")
      }
    },
    # Model --------------------------------------------------------------------
    models = NULL,
    args_model = NULL,
    fit = function() {
      self$check_data()
      self$check_time()
      self$check_event()
      self$check_treatment()
      self$check_variables()
      self$check_formula()
      if (!dplyr::is_grouped_df(self$data)) {
        data_list <- list(ALL = self$data)
      } else {
        data_list <- waRRior::named_group_split_at(self$data, self$group_by, keep = T, verbose = T)
      }
      self$models <- purrr::map(data_list, purrr::safely(function(.data) {
        do.call(
          survival::coxph,
          c(
            list(
              data = .data,
              formula = self$formula
            ),
            self$args_model
          )
        )
      }))
    },
    # Results ------------------------------------------------------------------
    exponentiate = TRUE,
    log_x = TRUE,
    results = function() {
      variables <- self$get_variables()
      purrr::map(self$models, "result") %>%
      purrr::imap(function(.result, .group){
      .result %>%
        broom::tidy(exponentiate = self$exponentiate, conf.int = TRUE) %>%
        dplyr::mutate(Subgroup = .group) %>%
        purrr::reduce(c(variables$treatment, variables$covariates), function(.in, .cov) {
          .in$term <- stringr::str_replace(.in$term, .cov, paste0(.cov, "splithere"))
          .in
        }, .init = .) %>%
        {
          x <- .
          x$comparison <- stringr::str_split(x$term, stringr::fixed("splithere")) %>%
            purrr::map(~ .x[[2]]) %>%
            unlist()
          x$term <- stringr::str_split(x$term, stringr::fixed("splithere")) %>%
            purrr::map(~ .x[[1]]) %>%
            unlist()
          x
        } %>%
        dplyr::mutate_if(is.numeric, ~ round(.x, 4)) %>%
        dplyr::arrange(estimate) %>%
        dplyr::mutate(term = factor(term, c(variables$treatment, unique(waRRior::pop(.$term, variables$treatment))))) %>%
        dplyr::arrange(term) %>%
        dplyr::mutate(reference = purrr::map_chr(as.character(term), ~ self$get_reference()[[.x]])) %>%
        dplyr::select(Subgroup, term, reference, comparison, estimate, tidyselect::everything())
     })

    },
    # Errors -------------------------------------------------------------------
    errors = function(){
      purrr::map(self$models, "error")
    },
    # N ------------------------------------------------------------------
    N = function(){
      variables <- self$get_variables()
      data_list <- waRRior::named_group_split_at(self$data, self$group_by, keep = T, verbose = T)
      purrr::imap(data_list, function(.data, .subgroup){
        res <- purrr::map(c(variables$treatment, self$covariates, self$strata), function(.var) {
          if (!is.numeric(.data[[.var]])) {
            dplyr::select_at(.data, c(.var, variables$event)) %>%
              waRRior::tally_at(c(.var, variables$event)) %>%
              dplyr::mutate(!!rlang::sym(variables$event) := as.character(!!rlang::sym(variables$event))) %>%
              dplyr::select_at(c(.var, "n", variables$event)) %>%
              dplyr::rename(comparison = .var) %>%
              dplyr::mutate(term = .var) %>%
              # dplyr::rename(`Events/N` = n) %>%
              dplyr::mutate(comparison = as.character(comparison))
          } else {
            dplyr::select_at(data, c(.var, event)) %>%
              waRRior::tally_at(c(event)) %>%
              dplyr::mutate(!!rlang::sym(variables$event) := as.character(!!rlang::sym(variables$event))) %>%
              dplyr::select_at(c(.var, "n", variables$event)) %>%
              dplyr::select_at(c("n")) %>%
              dplyr::mutate(term = .var) %>%
              # dplyr::rename(`Events/N` = n) %>%
              dplyr::mutate(comparison = "")
          }
        }) %>%
          dplyr::bind_rows()
        res %>%
         dplyr::group_by_at(c("comparison", "term")) %>%
         dplyr::summarise(N = sum(n, na.rm = T), event = !!rlang::sym(variables$event)) %>%
         dplyr::inner_join(res) %>%
         dplyr::mutate(`Events/N` = paste(n, N, sep = "/")) %>%
         dplyr::filter(!!rlang::sym(variables$event) == "1") %>%
         waRRior::drop_columns(c("n", "N", variables$event)) %>%
         dplyr::mutate(Subgroup = .subgroup)
      }) %>%
      dplyr::bind_rows()
    },
    # Annotation ----------------------------------------------------------------
    add_caption = TRUE,
    caption = function() {
      variables <- self$get_variables()
      if (self$add_caption) {
        if (is.null(variables$covariates) && is.null(variables$strata)) {
          .title <- "Univariate Cox's Proportional Hazard Model"
        } else {
          .title <- "Multivariate Cox's Proportional Hazard Model"
        }
        .title
      } else {
        NULL
      }
    },
    add_footnote = TRUE,
    footnote = function() {
      variables <- self$get_variables()
      if (self$add_footnote) {
        if (is.null(variables$covariates) && is.null(variables$strata)) {
          .footnote <- "Univariate Analysis"
        } else {
          comb <- c(variables$covariates, variables$strata)
          if (is.null(self$labels)){
            labels <- comb %>% purrr::set_names(comb)
          } else{
            labels <- self$labels
          } 
          .labeled_covariates <- labels[variables$covariates]
          .labeled_covariates[is.na(.labeled_covariates)] <- variables$covariates[is.na(.labeled_covariates)]
          .labeled_strata <- labels[variables$strata]
          .labeled_strata[is.na(.labeled_strata)] <- variables$strata[is.na(.labeled_strata)]

          if (length(.labeled_covariates) == 0) .labeled_covariates <- "none"
          if (length(.labeled_strata) == 0) .labeled_strata <- "none"
          .footnote <- paste(
            stringr::str_wrap(paste("Covariates: ", paste(.labeled_covariates, collapse = "; ")), width = 200),
            stringr::str_wrap(paste("Stratified by: ", paste(.labeled_strata, collapse = "; ")), width = 200),
            sep = "\n"
          )
        }
        .footnote
      } else {
        NULL
      }
    },
    # Labels ------------------------------------------------------------------
    labels = NULL,
    set_labels = function(labels) {
      if (!is.null(labels)) {
        .data <- self$data
        names(.data) <- nightowl::get_labels(names(.data), labels)
        self$data <- .data
        self$get_variables() %>%
          purrr::iwalk(function(.x, .y){
           self[[.y]] <- nightowl::get_labels(.x, labels)
          })
      }
      self$labels <- labels
    },
    # Arrange ------------------------------------------------------------------
    arrange_by = NULL,
    arrange = function(x) {
      if (!is.null(self$arrange_by)) {
        dplyr::arrange(x, !!rlang::sym(self$arrange_by))
      } else {
        x
      }
    },
    # Raw ---------------------------------------------------------------------
    conf_range = NULL,
    label_left = "Comparison better",
    label_right = "Reference better",
    raw = function(drop = NULL, keep_only_treatment = TRUE){

      results <- dplyr::inner_join(
        self$N(),
        dplyr::bind_rows(self$results())
      )

      if(is.null(self$conf_range)){
        self$conf_range <- c(
          min(results$conf.low[results$conf.low != -Inf], na.rm = T),
          max(results$conf.high[results$conf.high != Inf], na.rm = T)
        )
      }
      res <- results %>%
        dplyr::group_by_all() %>%
        dplyr::group_split() %>%
        purrr::map(function(.x) {
          .p <- nightowl::add_inline_forestplot(
            x = log(.x$estimate),
            xmin = log(.x$conf.low),
            xmax = log(.x$conf.high),
            xlim = log(self$conf_range),
            xintercept = 0
          )
          .x$Visualization <- .p
          .x
        }) %>%
        dplyr::bind_rows()
      if(keep_only_treatment){
        res <- res %>%
          dplyr::filter(term == self$get_variables()$treatment)
      }
      forest_label <- glue::glue("
        <div 
        style= 'font-size: smaller;display:flex; flex-direction:column;align-items:center;'>
        <div>log(HR)</div>
        <div>← {self$label_left} | {self$label_right} →</div>
        </div>
      ")
      res <- res %>%
        dplyr::arrange(`estimate`) %>%
        dplyr::mutate(`conf.low` = round(`conf.low`, 2)) %>%
        dplyr::mutate(`conf.high` = round(`conf.high`, 2)) %>%
        dplyr::mutate_if(is.numeric, ~ round(.x, 3)) %>%
        dplyr::mutate(HR = glue::glue("{estimate} ({conf.low}-{conf.high})")) %>%
        dplyr::select(
          Subgroup,
          Term = term,
          Comparison = comparison,
          Reference = reference,
          !!rlang::sym(forest_label) := Visualization,
          `Hazard Ratio` = HR,
          `p Value` = p.value,
          `Events/N` = `Events/N`,
        )
        res$`p Value` <- purrr::map_chr(res$`p Value`, ~ nightowl::format_p_value(.x))
        if (!is.null(drop)) {
          res <- waRRior::drop_columns(res, drop)
        }
        subgroups <- stringr::str_split(res$Subgroup, " / ") %>%
        purrr::map(~stringr::str_split(.x, "==")) %>%
        purrr::map(function(.x){
          purrr::map(.x, function(.y){
            .res <- list()
            .res[[.y[[1]]]] <- .y[[2]]
            tibble::as_tibble(.res)
          }) %>%
          dplyr::bind_cols()
       }) %>%
       dplyr::bind_rows()
       
      dplyr::bind_cols(subgroups, res) %>%
        dplyr::select(-Subgroup)
    },
    # Kabel ------------------------------------------------------------------
    options_kable = list(),
    kable = function(drop = NULL, keep_only_treatment = TRUE) {
      do.call(nightowl::render_kable, c(list(.tbl = self$raw(drop = drop, keep_only_treatment = keep_only_treatment), caption = self$caption(), footnote = self$footnote()), self$options_kable))
    },
    # HTML ---------------------------------------------------------------------
    html = function(drop = NULL) {
      shiny::HTML(self$kable(drop = drop))
    },
    # Reactables ---------------------------------------------------------------
    options_reactables = list(defaultPageSize = 30),
    reactable = function(drop = NULL, keep_only_treatment = FALSE, ...) {
      .args <- list(...)
      .options <- c(.args, self$options_reactables[!names(self$options_reactables) %in% names(.args)])
      res <- shiny::div(
        style = "
          font-family: 'Lato', sans-serif;
          display: flex;
          flex-direction: column;
          align-content: flex-start;
          align-items: center;
        ",
        shiny::h3(self$caption()),
        do.call(nightowl::render_reactable, c(list(.tbl = self$raw(drop = drop, keep_only_treatment = keep_only_treatment)), .options)),
        shiny::div(
          style = "font-size: 0.8em;",
          glue::glue(self$footnote())
        )
      ) %>%
        htmltools::browsable()
      return(res)
    }
  )
)
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
fit_coxph <- function(data, time, event, treatment, covariates, strata, exponentiate = FALSE, ...) {
  cli::cli_h1("🦉 Fitting Cox Proportional Hazard Model")
  require(survival)
  # Select relevant variables --------------------------------------------------
  .data <- data %>%
    dplyr::select_at(c(time, event, treatment, covariates, strata)) %>%
    dplyr::mutate_if(is.character, factor)
  # Store reference information ------------------------------------------------
  .reference <- .data %>%
    dplyr::select_if(is.factor) %>%
    droplevels() %>%
    purrr::map(~ levels(.x)[1])
  .numeric <- .data %>%
    dplyr::select_if(is.numeric) %>%
    purrr::map(~"")
  .reference <- c(.reference, .numeric)
  # Fit CoxPH model ------------------------------------------------------------
  .formula <- nightowl::create_Surv_formula(.data, time, event, treatment, covariates, strata)
  .model <- survival::coxph(.formula, data = .data, ...)
  # Tidy outputs ---------------------------------------------------------------
  res <- .model %>%
    broom::tidy(exponentiate = exponentiate, conf.int = TRUE) %>%
    purrr::reduce(c(treatment, covariates), function(.in, .cov) {
      .in$term <- stringr::str_replace(.in$term, .cov, paste0(.cov, "splithere"))
      .in
    }, .init = .) %>%
    {
      x <- .
      x$group <- stringr::str_split(x$term, stringr::fixed("splithere")) %>%
        purrr::map(~ .x[[2]]) %>%
        unlist()
      x$term <- stringr::str_split(x$term, stringr::fixed("splithere")) %>%
        purrr::map(~ .x[[1]]) %>%
        unlist()
      x
    } %>%
    dplyr::mutate_if(is.numeric, ~ round(.x, 4)) %>%
    dplyr::arrange(estimate) %>%
    dplyr::mutate(term = factor(term, c(treatment, unique(waRRior::pop(.$term, treatment))))) %>%
    dplyr::arrange(term) %>%
    dplyr::mutate(reference = purrr::map_chr(as.character(term), ~ .reference[[.x]])) %>%
    dplyr::select(variable = term, reference, group, estimate, tidyselect::everything())

  .n <- purrr::map(c(treatment, covariates, strata), function(.var) {
    if (!is.numeric(data[[.var]])) {
      dplyr::select_at(data, c(.var, event)) %>%
        waRRior::tally_at(c(.var, event)) %>%
        dplyr::filter(!!rlang::sym(event) == 1) %>%
        dplyr::select_at(c(.var, "n")) %>%
        dplyr::rename(group = .var) %>%
        dplyr::mutate(variable = .var) %>%
        dplyr::rename(`Events/N` = n) %>%
        dplyr::mutate(group = as.character(group))
    } else {
      dplyr::select_at(data, c(.var, event)) %>%
        waRRior::tally_at(c(event)) %>%
        dplyr::filter(!!rlang::sym(event) == 1) %>%
        dplyr::select_at(c("n")) %>%
        dplyr::mutate(variable = .var) %>%
        dplyr::rename(`Events/N` = n) %>%
        dplyr::mutate(group = "")
    }
  }) %>%
    dplyr::bind_rows()
  res <- dplyr::inner_join(res, .n)
  # Store metainfomation -------------------------------------------------------
  cli::cli_progress_step("🦉 Storing metainfomation")
  attributes(res)$model <- .model
  attributes(res)$formula <- .formula
  attributes(res)$data <- .data
  attributes(res)$time <- time
  attributes(res)$event <- event
  attributes(res)$covariates <- covariates
  attributes(res)$strata <- strata
  attributes(res)$exponentiate <- exponentiate
  # Return results -------------------------------------------------------------
  cli::cli_progress_step("🦉 Returning results")
  res
}
# =================================================
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @oetun
#' @export
plot_coxph <- function(data,
                       time,
                       event,
                       treatment,
                       covariates = NULL,
                       strata = NULL,
                       show_only_treatment = FALSE,
                       engine = "kable",
                       kable_style = kableExtra::kable_paper,
                       split = NULL,
                       conf_range = NULL,
                       add_scale = T,
                       label_left = "Comparison better",
                       label_right = "Reference better",
                       title = "",
                       labels = NULL,
                       plan = "sequential",
                       ...) {
  future::plan(plan)
  if (is.null(split)) {
    .result <- nightowl::fit_coxph(data, time, event, treatment, covariates, strata, exponentiate = TRUE)
  } else {
    .result <- data %>%
      waRRior::named_group_split_at(split) %>%
      furrr::future_imap(~ nightowl::fit_coxph(.x, time, event, treatment, covariates, strata, exponentiate = TRUE) %>%
        dplyr::mutate(!!rlang::sym(split) := .y)) %>%
      dplyr::bind_rows() %>%
      dplyr::ungroup()
  }
  .attributes <- attributes(.result)
  .result <- dplyr::filter(.result, !is.na(estimate))
  if (show_only_treatment) .result <- dplyr::filter(.result, variable == treatment) %>% waRRior::drop_empty_columns()
  if (is.null(conf_range)) {
    conf_range <- range.default(.result$conf.low, .result$conf.high, na.rm = TRUE, finite = TRUE)
  }

  .result <- .result %>%
    dplyr::group_by_all() %>%
    dplyr::group_split() %>%
    purrr::map(function(.x) {
      .p <- nightowl::add_inline_forestplot(
        x = .x$estimate,
        xmin = .x$conf.low,
        xmax = .x$conf.high,
        xlim = conf_range,
        xintercept = 1
      )
      .x$Visualization <- .p
      .x
    }) %>%
    dplyr::bind_rows()
  .result <- .result %>%
    dplyr::mutate_if(is.numeric, ~ round(.x, 3)) %>%
    dplyr::mutate(HR = glue::glue("{estimate} ({conf.low}-{conf.high})"))
  forest_label <- glue::glue("← {label_left} | {label_right} →")
  if (!is.null(split)) {
    .result <- .result %>%
      dplyr::select(
        Variable = variable,
        Comparison = group,
        Reference = reference,
        `Events/N`,
        `Hazard Ratio` = HR,
        `p Value` = p.value,
        !!rlang::sym(forest_label) := Visualization,
        split
      ) %>%
      dplyr::select_at(c(split, waRRior::pop(names(.), split)))
    collapse_this <- 1
  } else {
    .result <- .result %>%
      dplyr::select(
        Variable = variable,
        Comparison = group,
        Reference = reference,
        `Events/N`,
        `Hazard Ratio` = HR,
        `p Value` = p.value,
        !!rlang::sym(forest_label) := Visualization,
      )
    collapse_this <- 1
  }
  .result$`p Value` <- purrr::map_chr(.result$`p Value`, ~ nightowl::format_p_value(.x))
  .result <- .result[, purrr::map_lgl(.result, ~ !all(identical(.x, "")))]
  if (!is.null(labels)) {
    .result$Variable <- purrr::reduce(names(labels), function(.x, .y) {
      stringr::str_replace_all(.x, .y, labels[.y])
    }, .init = .result$Variable)
  }

  # Add Scale
  # if (add_scale) {
  #   .result <- nightowl::add_scale(.result, scaling = 2.9, height = 0.6)
  # }

  if (is.null(covariates) && is.null(strata)) {
    .footnote <- "Univariate Analysis"
  } else {
    comb <- c(covariates, strata)
    if (is.null(labels)) labels <- comb %>% purrr::set_names(comb)
    .labeled_covariates <- labels[covariates]
    .labeled_covariates[is.na(.labeled_covariates)] <- covariates[is.na(.labeled_covariates)]
    .labeled_strata <- labels[strata]
    .labeled_strata[is.na(.labeled_strata)] <- strata[is.na(.labeled_strata)]

    if (length(.labeled_covariates) == 0) .labeled_covariates <- "none"
    if (length(.labeled_strata) == 0) .labeled_strata <- "none"
    .footnote <- paste(
      stringr::str_wrap(paste("Covariates: ", paste(.labeled_covariates, collapse = "; ")), width = 200),
      stringr::str_wrap(paste("Stratified by: ", paste(.labeled_strata, collapse = "; ")), width = 200),
      sep = "\n"
    )
  }
  .result
  # if (engine == "kable") {
  #   options(knitr.kable.NA = "")
  #   .result %>%
  #     knitr::kable("html", escape = F, caption = glue::glue("Cox's Proportional Hazard Model {title}")) %>%
  #     kableExtra::column_spec(1, bold = T) %>%
  #     kableExtra::collapse_rows(columns = collapse_this, valign = "top") %>%
  #     kableExtra::footnote(.footnote) %>%
  #     kableExtra::kable_styling(full_width = F)
  # } else if (engine == "reactable") {
  #   col_def <- list()
  #   col_def[[forest_label]] <- reactable::colDef(html = TRUE, minWidt = 200)
  #   tbl <- .result %>%
  #     reactable::reactable(
  #       columns = col_def,
  #       filterable = T,
  #       style = list(fontFamily = "Work Sans, sans-serif", fontSize = "14px", padding = "10px"),
  #       searchable = TRUE,
  #       bordered = TRUE,
  #       onClick = "select",
  #       showPageSizeOptions = TRUE,
  #       pageSizeOptions = c(10, 25, 50, 100)
  #     )
  #   shiny::div(
  #     shiny::h3(glue::glue("Cox's Proportional Hazard Model {title}")),
  #     tbl,
  #     stringr::str_replace(.footnote, "\n", "<br>")
  #   ) %>%
  #     htmltools::browsable()
  # }
}
# =================================================
