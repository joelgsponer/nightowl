# =================================================
#' @title
#' MISSING_TITLE
#' @export
plot_grouped_km <- function(data,
                            time,
                            event,
                            treatment,
                            covariates = NULL,
                            split = NULL,
                            width = "95vw",
                            flex_direction = "column",
                            as_ggplot = FALSE,
                            title = function() {
                              glue::glue("{.split}", .envir = parent.frame())
                            },
                            subtitle = function() NULL,
                            note = function() NULL,
                            style = "
                              border-style: solid;
                              border-width: thin;
                              padding: 5px;
                              margin-bottom: 10px;
                            ",
                            break_width = (max(data[[time]], na.rm = T) / 20),
                            ...) {
  .formula <- nightowl::create_Surv_formula(data = data, time = time, event = event, treatment = treatment, covariates = covariates)
  res <- data %>%
    waRRior::named_group_split_at(split) %>%
    purrr::imap(function(.data, .split) {
      cli::cli_progress_step(.split)
      .data <- droplevels(.data)
      nightowl::plot_km(.data,
        time,
        event = event,
        title = title(),
        subtitle = subtitle(),
        note = note(),
        treatment = treatment,
        covariates = covariates,
        width = width,
        as_ggplot = as_ggplot,
        break_width = break_width,
        ...
      )
    })
  if (as_ggplot) {
    require("patchwork")
    .p <- purrr::reduce(res, ~ .x + .y)
    return(.p)
  }

  res <- purrr::map(res, function(.x) {
    shiny::div(style = style, .x)
  })

  res %>%
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
                    wrap = NULL,
                    legend_position = "top",
                    height = 600,
                    width = "100%",
                    colors = unname(unlist(picasso::roche_colors())),
                    lowrider_theme = "print",
                    as_ggplot = FALSE,
                    note = NULL,
                    break_width = (max(data[[time]], na.rm = T) / 20)) {
  .formula <- nightowl::create_Surv_formula(data = data, time = time, event = event, treatment = treatment, covariates = covariates)
  fit <- nightowl::fit_km(data = data, time = time, event = event, treatment = treatment, covariates = covariates)
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
      tibble::as_tibble() %>%
      dplyr::arrange(median)

    .p <- .p + ggplot2::geom_vline(
      xintercept = .median$median,
      color = colors[1:nrow(.median)],
      linetype = "dashed"
    ) +
      ggplot2::geom_point(
        data = .median,
        mapping = ggplot2::aes(x = median, y = seq(0.8, 0.8 - 0.03 * (nrow(.median) - 1), -0.03), nrisk = NULL, color = NULL),
        size = 5,
        color = "white"
      ) +
      ggplot2::geom_text(
        data = .median,
        mapping = ggplot2::aes(x = median, y = seq(0.8, 0.8 - 0.03 * (nrow(.median) - 1), -0.03), label = round(median, 2), nrisk = NULL),
        size = 4,
        color = "black"
      ) +
      ggplot2::geom_text(
        data = .median %>%
          dplyr::summarise(mid = mean(median)),
        mapping = ggplot2::aes(x = mid, y = 0.95, label = "Median Survival Time", nrisk = NULL),
        size = 3,
        color = "black"
      )
  }

  legend_orientation <- if (legend_position == "top") {
    "h"
  } else {
    "v"
  }
  if (as_ggplot) {
    .p <- .p + ggplot2::ggtitle(title) +
      ggplot2::labs(subtitle = subtitle)
    return(.p)
  }
  # Create plotly
  .img <- plotly::ggplotly(.p) %>%
    plotly::layout(legend = list(
      orientation = legend_orientation,
      xanchor = "center",
      y = 1.2,
      x = 0.5
    ))
  # Fix Legend title
  .img$x$data <- purrr::map(.img$x$data, function(.old, .new) {
    purrr::imap(.old, function(.x, .y) {
      if (.y == "name") {
        stringr::str_replace(.x, ",1,NA", "")
      } else {
        .x
      }
    })
  })

  if (add_table) {
    risk.table <- nightowl::km_table(fit, kable = F, break_width = break_width)
    risk.table.plot <- plotly::plot_ly(
      x = risk.table$breakpoint,
      height = height,
      y = risk.table$Group,
      text = risk.table$n,
      type = "scatter",
      mode = "text",
      showlegend = FALSE
    ) %>%
      plotly::layout(
        plot_bgcolor = "#ffffff",
        font = list(family = "Lato, sans-serif"),
        xaxis = list(
          zerolinecolor = "#ffff",
          zerolinewidth = 2,
          color = "#ffff",
          gridcolor = "ffff"
        ),
        yaxis = list(
          ticklabelposition = "outside left",
          range = c(-1, length(unique(risk.table$Group)) + 1),
          zerolinecolor = "#ffff",
          zerolinewidth = 2,
          gridcolor = "ffff"
        )
      )
    .img <- plotly::subplot(.img, risk.table.plot, nrows = 2, heights = c(0.8, 0.2))
  }
  shiny::div(
    style = "
    display:flex;
    flex-direction:column;
    align-items:center;
    font-family: 'Lato', sans-serif;
    ",
    shiny::div(
      # lowRider::includeCSS(lowrider_theme),
      class = "lowrider-card",
      style = glue::glue("width:{width};"),
      shiny::div(
        class = "lowrider-card-header",
        style = "
          font-size:1.2em;
          font-weight:bolder;
        ",
        Hmisc::capitalize(title)
      ),
      shiny::div(
        class = "lowrider-card-body",
        shiny::div(
          style = "
          font-size:1em;
          font-weight:bold;
          color: #505050;
          ",
          subtitle
        ),
        shiny::div(
          .img
        ),
        if (add_p) {
          shiny::div(
            style = "text-align:right; margin-right: 10px;",
            nightowl::km_pvalue(.formula, data)
          )
        } else {
          shiny::div()
        },
        if (add_summary) {
          nightowl::km_summary(.formula, data)
        } else {
          shiny::div()
        },
        if (!is.null(note)) {
          shiny::div(style = "font-size: 0.8rem;", note)
        } else {
          shiny::div()
        }
      )
    )
  ) %>%
    htmltools::browsable()
}
# =================================================
#' @title
#' MISSING_TITLE
#' @export
km_pvalue <- function(.formula, data, html = TRUE) {
  p <- survival::survdiff(.formula, data = data) %>%
    {
      1 - pchisq(.$chisq, length(.$n) - 1)
    } %>%
    round(4)
  p <- if (html) {
    shiny::tag("pre", paste0("Logrank Test p-Value: ", p))
  } else {
    p
  }
  return(p)
}
# =================================================
#' @title
#' MISSING_TITLE
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
  .formula <- nightowl::create_Surv_formula(data = data, time = time, event = event, treatment = treatment, covariate = covariates)
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
#' @export
km_table <- function(fit, what = "n.risk", break_width = 10, kable = T) {
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
    dplyr::bind_rows()
  if (kable) {
    risk.table <- risk.table %>%
      tidyr::pivot_wider(names_from = "breakpoint", values_from = "n") %>%
      kableExtra::kable() %>%
      kableExtra::kable_paper()
    title <- stringr::str_replace(what, stringr::fixed("."), " ") %>%
      Hmisc::capitalize()
    shiny::div(
      style = "width:75%;",
      kableExtra:::html_dependency_lightable(),
      shiny::h4(title),
      shiny::HTML(risk.table)
    ) %>%
      htmltools::browsable()
  } else {
    risk.table
  }
}
# =================================================
#' @title
#' MISSING_TITLE
#' @export
plot_grouped_km_compact <- function(data,
                                    time,
                                    event,
                                    treatment,
                                    covariates = NULL,
                                    split,
                                    width = "400px",
                                    add_table = TRUE,
                                    break_width = (max(data[[time]], na.rm = T) / 6),
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
    add_table = add_table,
    add_summary = FALSE,
    add_median = FALSE,
    break_width = break_width,
    ...
  )
}
# =================================================
#' @title
#' MISSING_TITLE
#' @export
plot_km_covariates <- function(data,
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
                               wrap = NULL,
                               legend_position = "top",
                               height = 600,
                               width = "100%",
                               colors = unname(unlist(picasso::roche_colors())),
                               lowrider_theme = "print",
                               as_ggplot = FALSE,
                               note = NULL,
                               break_width = (max(data[[time]], na.rm = T) / 20)) {
  .formula <- nightowl::create_Surv_formula(data = data, time = time, event = event, treatment = treatment)
  fit <- nightowl::fit_km(data, time, event, treatment)
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

    browser()
    .p <- .p + ggplot2::geom_vline(
      xintercept = .median$median,
      color = colors[1:nrow(.median)],
      linetype = "dashed"
    ) +
      ggplot2::geom_point(
        data = .median,
        mapping = ggplot2::aes(x = median, y = seq(0.8, 0.8 - 0.2 * nrow(.median), 0.2), nrisk = NULL, color = NULL),
        size = 5,
        color = "white"
      ) +
      ggplot2::geom_text(
        data = .median,
        mapping = ggplot2::aes(x = median, y = seq(0.8, 0.8 - 0.2 * nrow(.median), 0.2), label = round(median, 2), nrisk = NULL),
        size = 4,
        color = "black"
      ) +
      ggplot2::geom_text(
        data = .median %>%
          dplyr::summarise(mid = mean(median)),
        mapping = ggplot2::aes(x = mid, y = 0.95, label = "Median Survival Time", nrisk = NULL),
        size = 3,
        color = "black"
      )
  }

  legend_orientation <- if (legend_position == "top") {
    "h"
  } else {
    "v"
  }
  if (as_ggplot) {
    .p <- .p + ggplot2::ggtitle(title) +
      ggplot2::labs(subtitle = subtitle)
    return(.p)
  }
  # Create plotly
  .img <- plotly::ggplotly(.p) %>%
    plotly::layout(legend = list(
      orientation = legend_orientation,
      xanchor = "center",
      y = 1.2,
      x = 0.5
    ))
  # Fix Legend title
  .img$x$data <- purrr::map(.img$x$data, function(.old, .new) {
    purrr::imap(.old, function(.x, .y) {
      if (.y == "name") {
        stringr::str_replace(.x, ",1,NA", "")
      } else {
        .x
      }
    })
  })


  cplots <- purrr::map(covariates, function(.covariate) {
    waRRior::named_group_split_at(data, c(treatment)) %>%
      purrr::map(purrr::safely(function(.x) {
        times <- .x[[time]] %>% sort()
        res <- purrr::map_df(times, function(.y) {
          .data <- .x %>% dplyr::filter(time >= .y)
          dplyr::select_at(.data, .covariate) %>%
            tidyr::pivot_longer(cols = names(.)) %>%
            dplyr::group_by(name) %>%
            dplyr::add_count() %>%
            dplyr::group_by(name, value) %>%
            dplyr::add_count() %>%
            dplyr::mutate(freq = nn / n) %>%
            dplyr::mutate(time = .y) %>%
            unique()
        })

        .res <- waRRior::named_group_split(res, value)
        .p <- plotly::plot_ly()
        .treatment <- unique(.x$treatment)
        purrr::reduce(.res, function(.x, .y) {
          .x %>%
            plotly::add_trace(
              name = paste(.covariate, .treatment, unique(.y$value)),
              x = .y$time,
              y = .y$freq,
              name = .y,
              type = "scatter",
              mode = "lines",
              stackgroup = "one"
            )
        }, .init = .p)
      })) %>%
      purrr::map("result")
  }) %>%
    waRRior::collapse_top_level()
  l <- length(cplots) + 1
  h <- 0.7 / l
  hh <- c(0.3, rep(h, l - 1))
  do.call(plotly::subplot, c(list(.img), cplots, list(nrows = l, heights = hh)))
}
