# =================================================
#' @title Plot Grouped Kaplan-Meier Curves
#' @description Creates multiple Kaplan-Meier survival curves grouped by a splitting variable
#' @param data Data frame containing the survival data
#' @return HTML object with arranged Kaplan-Meier plots that can be viewed in a browser
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
    nightowl_named_group_split_at(data, split) %>%
    purrr::imap(function(data, .split) {
      cli::cli_progress_step(.split)
      data <- droplevels(data)
      nightowl::plot_km(data,
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
#' @title Plot Kaplan-Meier Survival Curve
#' @description Creates an interactive Kaplan-Meier survival curve with risk table and statistical tests
#' @param data Data frame containing the survival data
#' @return HTML object with interactive Kaplan-Meier plot or ggplot object if as_ggplot = TRUE
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
                    colors = unname(unlist(nightowl::nightowl_colors())),
                    lowrider_theme = "print",
                    as_ggplot = FALSE,
                    note = NULL,
                    break_width = (max(data[[time]], na.rm = T) / 20)) {
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
        mapping = ggplot2::aes(x = median, y = 0.8, nrisk = NULL, color = NULL),
        size = 5,
        color = "white"
      ) +
      ggplot2::geom_text(
        data = .median,
        mapping = ggplot2::aes(x = median, y = 0.8, label = round(median, 2), nrisk = NULL),
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
#' @title Calculate Kaplan-Meier P-value
#' @description Calculates the log-rank test p-value for survival differences
#' @param .formula Survival formula object
#' @param data Data frame containing the survival data
#' @param html Logical indicating whether to return HTML formatted output (default: TRUE)
#' @return P-value from log-rank test, optionally formatted as HTML
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
#' @title Fit Kaplan-Meier Survival Model
#' @description Fits a Kaplan-Meier survival model and returns tidy results
#' @param data Data frame containing the survival data
#' @param time Character string specifying the time variable name
#' @param event Character string specifying the event variable name
#' @param treatment Character string specifying the treatment/grouping variable name
#' @param covariates Character vector of covariate variable names (optional)
#' @param landmark Numeric value for landmark analysis time point (optional)
#' @param ... Additional arguments passed to survfit
#' @return Tidy data frame with survival estimates and confidence intervals
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
#' @title Add Time Zero to Kaplan-Meier Fit
#' @description Adds time zero point to Kaplan-Meier survival data for complete curves
#' @param fit Data frame from fit_km containing survival estimates
#' @return Data frame with time zero point added for each stratum
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
#' @title Generate Kaplan-Meier Summary
#' @description Creates a formatted summary of survival differences using survdiff
#' @param .formula Survival formula object
#' @param data Data frame containing the survival data
#' @return HTML formatted summary of survival differences
#' @export
km_summary <- function(.formula, data) {
  res <- survival::survdiff(.formula, data = data) %>%
    shiny::renderPrint() %>%
    .() %>%
    stringr::str_replace(stringr::fixed(".formula"), as.character(.formula)[2])
  shiny::tag("pre", res)
}
# =================================================
#' @title Create Kaplan-Meier Risk Table
#' @description Generates a risk table showing number at risk at specified time intervals
#' @param fit Data frame from fit_km containing survival estimates
#' @param what Character string specifying what to display (default: "n.risk")
#' @param break_width Numeric value for time interval width (default: 10)
#' @param kable Logical indicating whether to return formatted kable table (default: TRUE)
#' @return Risk table as kable HTML or data frame
#' @export
km_table <- function(fit, what = "n.risk", break_width = 10, kable = T) {
  fit <- nightowl::km_add_0(fit)
  breakpoints <- seq(0, max(fit$time), break_width)
  risk.table <- nightowl_named_group_split(fit, !!rlang::sym(strata)) %>%
    purrr::imap(function(.x, .group) {
      N <- max(.x$n.risk, na.rm = T)
      
      # Optimize O(n²) to O(n) using cumulative sums instead of repeated filtering
      .x_sorted <- .x %>% dplyr::arrange(time)
      
      # Pre-compute cumulative sums - O(n) operation
      cum_events <- cumsum(.x_sorted$n.event)
      cum_censors <- cumsum(.x_sorted$n.censor)
      
      # Use vectorized operations with findInterval - O(k log n) instead of O(k*n)
      # where k is number of breakpoints, much better when k scales with n
      event_indices <- findInterval(breakpoints, .x_sorted$time, left.open = TRUE)
      censor_indices <- findInterval(breakpoints, .x_sorted$time, left.open = FALSE)
      
      # Handle edge cases where indices are 0
      event_sums <- ifelse(event_indices == 0, 0, cum_events[event_indices])
      censor_sums <- ifelse(censor_indices == 0, 0, cum_censors[censor_indices])
      
      tibble::tibble(
        breakpoint = breakpoints,
        n = N - event_sums - censor_sums,
        Group = .group
      )
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
#' @title Plot Compact Grouped Kaplan-Meier Curves
#' @description Creates compact multiple Kaplan-Meier curves with minimal styling
#' @param data Data frame containing the survival data
#' @return HTML object with compact arranged Kaplan-Meier plots
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
#' @title Plot Kaplan-Meier with Covariate Distribution
#' @description Creates Kaplan-Meier curves with additional panels showing covariate distributions over time
#' @param data Data frame containing the survival data
#' @return Interactive plot combining survival curves and covariate distributions
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
                               colors = unname(unlist(nightowl::nightowl_colors())),
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


    .p <- .p + ggplot2::geom_vline(
      xintercept = .median$median,
      color = colors[1:nrow(.median)],
      linetype = "dashed"
    ) +
      ggplot2::geom_point(
        data = .median,
        mapping = ggplot2::aes(x = median, y = 0.8, nrisk = NULL, color = NULL),
        size = 5,
        color = "white"
      ) +
      ggplot2::geom_text(
        data = .median,
        mapping = ggplot2::aes(x = median, y = 0.8, label = round(median, 2), nrisk = NULL),
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
    nightowl_named_group_split_at(data, treatment) %>%
      purrr::map(purrr::safely(function(.x) {
        times <- .x[[time]] %>% sort() %>% unique()
        
        # Optimize O(n²) to O(n) by avoiding repeated filtering
        # Pre-sort data once and use vectorized operations
        .x_sorted <- .x %>% 
          dplyr::arrange(!!rlang::sym(time)) %>%
          dplyr::select_at(c(time, .covariate))
        
        # Create expanded grid for all time-covariate combinations
        covariate_data <- .x_sorted %>%
          dplyr::select_at(.covariate) %>%
          tidyr::pivot_longer(cols = names(.)) %>%
          dplyr::distinct()
        
        # Use vectorized approach instead of nested loops
        res <- purrr::map_df(times, function(.y) {
          # Use binary search approach - much faster than filtering
          valid_indices <- which(.x_sorted[[time]] >= .y)
          if (length(valid_indices) == 0) return(tibble::tibble())
          
          subset_data <- .x_sorted[valid_indices, ]
          
          subset_data %>%
            dplyr::select_at(.covariate) %>%
            tidyr::pivot_longer(cols = names(.)) %>%
            dplyr::group_by(name) %>%
            dplyr::add_count() %>%
            dplyr::group_by(name, value) %>%
            dplyr::add_count() %>%
            dplyr::mutate(freq = nn / n) %>%
            dplyr::mutate(time = .y) %>%
            unique()
        })

        .res <- nightowl_named_group_split(res, !!rlang::sym("value"))
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
    nightowl_collapse_top_level()
  l <- length(cplots) + 1
  h <- 0.7 / l
  hh <- c(0.3, rep(h, l - 1))
  do.call(plotly::subplot, c(list(.img), cplots, list(nrows = l, heights = hh)))
}
