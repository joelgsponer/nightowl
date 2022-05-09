# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
plot_grouped_km <- function(data, time, event, treatment, covariates = NULL, split = NULL, width = "75vw", flex_direction = "column", ...) {
  .formula <- nightowl::create_Surv_formula(data = data, time = time, event = event, treatment = treatment, covariates = covariates)
  data %>%
    waRRior::named_group_split_at(split) %>%
    purrr::imap(function(.data, .split) {
      cli::cli_progress_step(.split)
      .data <- droplevels(.data)
      nightowl::plot_km(.data,
        time,
        event = event,
        title = glue::glue("{.split} {event}"),
        treatment = treatment,
        covariates = covariates,
        width = width,
        ...
      )
    }) %>%
    shiny::div(style = glue::glue("
                                  display:flex;
                                  flex-wrap:wrap;
                                  flex-direction:{flex_direction};
                                  align-content: stretch;
                                  align-items: center;
                                  justify-content: space-evenly;
                                  ")) %>%
    htmltools::browsable()
}

# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
plot_km <- function(data,
                    time,
                    event,
                    treatment,
                    covariates = NULL,
                    title = event,
                    subtitle = NULL,
                    landmark = NULL,
                    ylab = "Survival",
                    xlab = "Time (Days)",
                    add_p = TRUE,
                    add_table = TRUE,
                    add_summary = FALSE,
                    add_median = TRUE,
                    legend_position = "top",
                    width = "66%",
                    colors = unname(unlist(picasso::roche_colors())),
                    lowrider_theme = "bulma",
                    break_width = 10) {
  .formula <- nightowl::create_Surv_formula(data = data, time = time, event = event, treatment = treatment, covariates = covariates)
  fit <- nightowl::fit_km(data, time, event, treatment, covariates)
  fit <- nightowl::km_add_0(fit)

  .p <- ggplot2::ggplot(
    fit,
    ggplot2::aes(
      x = time,
      y = estimate,
      color = strata,
      nrisk = n.risk
    )
  ) +
    ggplot2::geom_ribbon(data = dplyr::filter(fit, !is.na(conf.low) & !is.na(conf.high)), ggplot2::aes(fill = strata, ymin = conf.low, ymax = conf.high, color = NULL), alpha = 0.2) +
    ggplot2::geom_step() +
    ggplot2::geom_point(alpha = 0.1) +
    ggplot2::geom_point(data = dplyr::filter(fit, n.censor > 0), shape = 3) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::scale_size_manual(values = c(0, 2)) +
    ggplot2::theme_bw() +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::guides(
      color = ggplot2::guide_legend(
        title = glue::glue("{covariates}"),
        override.aes = list(alpha = 1)
      ),
      fill = "none",
      color = "none",
      shape = "none",
      size = "none"
    ) +
    ggplot2::theme(legend.position = legend_position)

  .p$guides$colour$title <- ""

  if (add_median) {
    .median <- base::summary(survival::survfit(.formula, data))$table %>%
      as.data.frame()

    .p <- .p + ggplot2::geom_vline(
      xintercept = .median$median,
      color = colors[1:nrow(.median)],
      linetype = "dashed"
    ) +
      ggplot2::geom_point(
        data = .median,
        mapping = ggplot2::aes(x = median, y = 0.8, label = round(median, 2), nrisk = NULL, color = NULL),
        size = 4,
        color = "white"
      ) +
      ggrepel::geom_text_repel(
        data = .median,
        mapping = ggplot2::aes(x = median, y = 0.8, label = round(median, 2), nrisk = NULL),
        size = 4,
        color = "black"
      ) +
      ggplot2::geom_text(
        data = .median,
        mapping = ggplot2::aes(x = mean(median), y = 0.95, label = "Median Survival Time", nrisk = NULL),
        size = 2,
        color = "black"
      )
  }

  legend_orientation <- if (legend_position == "top") {
    "h"
  } else {
    "v"
  }
  .img <- plotly::ggplotly(.p) %>%
    plotly::layout(legend = list(
      orientation = legend_orientation,
      xanchor = "center",
      y = 1.2,
      x = 0.5
    ))
  shiny::div(
    style = "
    display:flex;
    flex-direction:column;
    align-items:center;
    ",
    shiny::div(
      lowRider::useLowRider(lowrider_theme),
      class = "lowrider-card",
      style = glue::glue("width:{width};"),
      shiny::div(
        class = "lowrider-card-header",
        shiny::h4(title),
        shiny::h5(subtitle)
      ),
      shiny::div(
        class = "lowrider-card-body",
        .img,
        if (add_p) {
          nightowl::km_pvalue(.formula, data)
        } else {
          shiny::div()
        },
        if (add_table) {
          nightowl::km_table(fit, break_width = break_width)
        } else {
          shiny::div()
        },
        if (add_summary) {
          nightowl::km_summary(.formula, data)
        } else {
          shiny::div()
        },
      )
    )
  ) %>%
    htmltools::browsable()
}
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
km_pvalue <- function(.formula, data, html = TRUE) {
  p <- survival::survdiff(.formula, data = data) %>%
    {
      1 - pchisq(.$chisq, length(.$n) - 1)
    } %>%
    round(4)
  p <- if (html) {
    shiny::tag("pre", paste0("p-value: ", p))
  } else {
    p
  }
  return(p)
}
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
fit_km <- function(data, time, event, treatment, covariates = NULL, landmark = NULL, ...) {
  # Landmark -------------------------------------------------------------------
  if (!is.null(landmark)) {
    cli::cli_progress_step("Calculating landmark analysis at {landmark}")
    data <- data %>%
      dplyr::mutate(!!rlang::sym(time) := !!rlang::sym(time) - landmark) %>%
      dplyr::filter(!!rlang::sym(time) > 0)
  }
  # Formula -------------------------------------------------------------------
  .formula <- nightowl::create_Surv_formula(time = time, event = event, treatment = treatment, covariate = covariates)
  # Data ----------------------------------------------------------------------
  data <- droplevels(data)
  # Fit ------------------------------------------------------------------------
  survival::survfit(.formula, data = data) %>%
    broom::tidy() %>%
    tibble::rowid_to_column() %>%
    dplyr:::mutate(tooltip = round(estimate, 2))
}
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
km_add_0 <- function(fit) {
  fit %>%
    dplyr::group_by(strata) %>%
    dplyr::summarise(
      time = 0,
      n.risk = max(n.risk, na.rm = T),
      n.censor = 0,
      tooltip = 1,
      conf.high = NA,
      conf.low = NA,
      error = 0,
      std.error = 0,
      n.censor = 0,
      n.event = 0,
      estimate = 1,
      rowid = max(rowid) + 1
    ) %>%
    dplyr::bind_rows(fit) %>%
    dplyr::arrange(strata, time) %>%
    dplyr::ungroup()
}
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
km_summary <- function(.formula, data) {
  res <- survival::survdiff(.formula, data = data) %>%
    shiny::renderPrint() %>%
    .() %>%
    stringr::str_replace(stringr::fixed(".formula"), as.character(.formula)[2])
  shiny::tag("pre", res)
}
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
km_table <- function(fit, what = "n.risk", break_width = 10) {
  fit <- nightowl::km_add_0(fit)
  breakpoints <- seq(0, max(fit$time), break_width)
  risk.table <- waRRior::named_group_split(fit, strata) %>%
    purrr::imap(function(.x, .group) {
      N <- max(.x$n.risk, na.rm = T)
      purrr::map(breakpoints, function(.y) {
        tibble::tibble(
          breakpoint = .y,
          n = N - sum(dplyr::filter(.x, time < .y)$n.event) - sum(dplyr::filter(.x, time <= .y)$n.censor)
        )
      }) %>%
        dplyr::bind_rows() %>%
        dplyr::mutate(Group = .group)
    }) %>%
    dplyr::bind_rows() %>%
    tidyr::pivot_wider(names_from = "breakpoint", values_from = "n") %>%
    kableExtra::kable() %>%
    kableExtra::kable_paper()
  risk.table
  title <- stringr::str_replace(what, stringr::fixed("."), " ") %>%
    Hmisc::capitalize()
  shiny::div(
    style = "width:75%;",
    kableExtra:::html_dependency_lightable(),
    shiny::h4(title),
    shiny::HTML(risk.table)
  ) %>%
    htmltools::browsable()
}
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
plot_grouped_km_compact <- function(data,
                                    time,
                                    event,
                                    treatment,
                                    covariates = NULL,
                                    split,
                                    width = "300px",
                                    add_p = TRUE,
                                    ...) {
  nightowl::plot_grouped_km(
    data = data,
    time = time,
    event = event,
    treatment = treatment,
    covariates = covariates,
    split = split,
    width = width,
    legend_position = "none",
    flex_direction = "row",
    add_p = add_p,
    add_table = FALSE,
    add_summary = FALSE,
    add_median = FALSE,
    ...
  )
}
# =================================================
