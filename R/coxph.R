# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
fit_coxph <- function(data, time, event, treatment, covariates, strata, ...) {
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
    broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>%
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
        dplyr::rename(`N Events` = n) %>%
        dplyr::mutate(group = as.character(group))
    } else {
      dplyr::select_at(data, c(.var, event)) %>%
        waRRior::tally_at(c(event)) %>%
        dplyr::filter(!!rlang::sym(event) == 1) %>%
        dplyr::select_at(c("n")) %>%
        dplyr::mutate(variable = .var) %>%
        dplyr::rename(`N Events` = n) %>%
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
                       show_only_treatment = FALSE,
                       strata = NULL,
                       engine = "kable",
                       kable_style = kableExtra::kable_paper,
                       split = NULL,
                       conf_range = NULL,
                       label_left = "Comparison better",
                       label_right = "Reference better",
                       title = "",
                       ...) {
  if (is.null(split)) {
    .result <- nightowl::fit_coxph(data, time, event, treatment, covariates, strata)
  } else {
    .result <- data %>%
      waRRior::named_group_split_at(split) %>%
      purrr::imap(~ nightowl::fit_coxph(.x, time, event, treatment, covariates, strata) %>%
        dplyr::mutate(!!rlang::sym(split) := .y)) %>%
      dplyr::bind_rows() %>%
      dplyr::ungroup()
  }
  .attributes <- attributes(.result)
  .result <- dplyr::filter(.result, !is.na(estimate))
  if (show_only_treatment) .result <- dplyr::filter(.result, variable == treatment)
  if (is.null(conf_range)) {
    conf_range <- range.default(.result$conf.low, .result$conf.high, na.rm = TRUE, finite = TRUE)
    conf_range[conf_range > 5] <- 3
  }
  .result <- .result %>%
    dplyr::group_by_all() %>%
    dplyr::group_split() %>%
    purrr::map(function(.x) {
      .p <- ggplot2::ggplot(.x, ggplot2::aes(
        y = variable,
        x = estimate,
        xmin = conf.low,
        xmax = conf.high
      )) +
        ggplot2::geom_vline(xintercept = 1, color = picasso::roche_colors("red"), linetype = "solid", size = 1) +
        ggplot2::geom_errorbarh(position = ggplot2::position_dodge(width = 0.5)) +
        ggplot2::geom_point(cex = 8, shape = 18, position = ggplot2::position_dodge(width = 0.5), color = picasso::roche_colors("blue")) +
        ggplot2::xlim(conf_range) +
        ggplot2::theme_void() +
        ggplot2::theme(legend.position = "none")
      .p <- nightowl::render_svg(.p, height = 0.3, add_download_button = FALSE)
      .x$Visualization <- .p
      .x
    }) %>%
    dplyr::bind_rows()

  .result <- .result %>%
    dplyr::mutate_if(is.numeric, ~ round(.x, 3)) %>%
    dplyr::mutate(HR = glue::glue("{estimate}({conf.low}-{conf.high})"))


  forest_label <- glue::glue("← {label_left} | {label_right} →")

  if (!is.null(split)) {
    .result <- .result %>%
      dplyr::select(
        Variable = variable,
        Comparison = group,
        Reference = reference,
        `N Events`,
        `Hazard Ratio` = HR,
        `p  Value` = p.value,
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
        `N Events`,
        `Hazard Ratio` = HR,
        `p  Value` = p.value,
        !!rlang::sym(forest_label) := Visualization,
      )
    collapse_this <- 1
  }

  if (engine == "kable") {
    .result %>%
      knitr::kable("html", escape = F, caption = glue::glue("Cox's Proportional Hazard Model {title}")) %>%
      kableExtra::column_spec(1, bold = T) %>%
      kableExtra::collapse_rows(columns = collapse_this, valign = "top") %>%
      kableExtra::footnote(
        paste(
          paste("Covariates: ", paste(.attributes$covariates, collapse = "; ")),
          paste("Stratified by: ", paste(.attributes$strata, collapse = "; ")),
          sep = "\n"
        )
      ) %>%
      kableExtra::kable_style(full_width = F)
  } else if (engine == "reactable") {
    col_def <- list()
    col_def[[forest_label]] <- reactable::colDef(html = TRUE, minWidt = 200)
    tbl <- .result %>%
      reactable::reactable(
        columns = col_def,
        filterable = T,
        style = list(fontFamily = "Work Sans, sans-serif", fontSize = "14px", padding = "10px"),
        searchable = TRUE,
        bordered = TRUE,
        onClick = "select",
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(10, 25, 50, 100)
      )
    shiny::div(
      shiny::h3(glue::glue("Cox's Proportional Hazard Model {title}")),
      tbl,
      paste("Covariates: ", paste(.attributes$covariates, collapse = "; ")),
      paste("Stratified by: ", paste(.attributes$strata, collapse = "; ")),
    ) %>%
      htmltools::browsable()
  }
}
# =================================================
