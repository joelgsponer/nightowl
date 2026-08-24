# =================================================
#' @title Plot Grouped Kaplan-Meier Survival Curves
#' @description Creates multiple Kaplan-Meier survival curves grouped by a splitting variable.
#'   This function generates a collection of survival plots, one for each level of the
#'   splitting variable, arranged in a flexible layout for comparative analysis.
#' @param data A data frame containing survival data
#' @param time Character string specifying the time-to-event variable name
#' @param event Character string specifying the event indicator variable name (0/1 or FALSE/TRUE)
#' @param treatment Character string specifying the treatment group variable name
#' @param covariates Character vector of covariate names to include in the model (optional)
#' @param split Character string specifying the variable to split/group plots by
#' @param width Character string specifying the width of each plot (default: "95vw")
#' @param flex_direction Character string specifying flex direction ("column" or "row")
#' @param as_ggplot Logical indicating whether to return ggplot objects instead of interactive plots
#' @param title Function to generate plot titles, with access to .split variable
#' @param subtitle Function to generate plot subtitles
#' @param note Function to generate plot notes
#' @param style Character string with CSS styling for plot containers
#' @param break_width Numeric value for time axis break intervals
#' @param ... Additional arguments passed to plot_km function
#' @return If as_ggplot is TRUE, returns a patchwork object combining ggplot objects.
#'   Otherwise returns an HTML div containing interactive survival curves.
#' @export
#' @examples
#' \dontrun{
#' # Plot survival curves grouped by study site
#' plot_grouped_km(
#'   data = clinical_data,
#'   time = "survival_time",
#'   event = "death",
#'   treatment = "treatment_arm",
#'   split = "study_site"
#' )
#' }
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
#' @title Plot Kaplan-Meier Survival Curves
#' @description Creates an interactive Kaplan-Meier survival curve with optional risk table,
#'   confidence intervals, and statistical annotations. This is the core function for
#'   visualizing time-to-event data in clinical research applications.
#' @param data A data frame containing survival data
#' @param time Character string specifying the time-to-event variable name
#' @param event Character string specifying the event indicator variable name (0/1 or FALSE/TRUE)
#' @param treatment Character string specifying the treatment group variable name
#' @param covariates Character vector of covariate names to include in the model (optional)
#' @param title Character string for plot title (default: event name)
#' @param subtitle Character string for plot subtitle
#' @param landmark Numeric value for landmark analysis time point
#' @param ylab Character string for y-axis label (default: "Survival")
#' @param xlab Character string for x-axis label (default: "Time (Days)")
#' @param add_p Logical indicating whether to add log-rank test p-value
#' @param add_table Logical indicating whether to add numbers-at-risk table
#' @param add_summary Logical indicating whether to add survival summary
#' @param add_median Logical indicating whether to add median survival time markers
#' @param wrap Character string for facet wrapping variable
#' @param legend_position Character string for legend position ("top", "bottom", "left", "right")
#' @param height Numeric value for plot height in pixels
#' @param table_height_fraction Numeric fraction of total height for risk table
#' @param width Character string specifying plot width
#' @param colors Character vector of colors for survival curves
#' @param lowrider_theme Character string specifying theme
#' @param as_ggplot Logical indicating whether to return ggplot object instead of interactive plot
#' @param note Character string for plot notes
#' @param break_width Numeric value for time axis break intervals
#' @return If as_ggplot is TRUE, returns a ggplot object. Otherwise returns an HTML div
#'   containing an interactive plotly survival curve with optional risk table.
#' @export
#' @examples
#' \dontrun{
#' # Basic Kaplan-Meier plot
#' plot_km(
#'   data = clinical_data,
#'   time = "survival_time",
#'   event = "death",
#'   treatment = "treatment_arm"
#' )
#' 
#' # With landmark analysis at 30 days
#' plot_km(
#'   data = clinical_data,
#'   time = "survival_time",
#'   event = "death",
#'   treatment = "treatment_arm",
#'   landmark = 30
#' )
#' }
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
                    table_height_fraction = 0.2,
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
    .img <- plotly::subplot(.img, risk.table.plot, nrows = 2, heights = c(1 - table_height_fraction, table_height_fraction))
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
#' @title Calculate Log-Rank Test P-Value for Survival Curves
#' @description Computes the log-rank test p-value for comparing survival curves between groups.
#'   This test evaluates whether there are statistically significant differences in
#'   survival distributions between treatment groups.
#' @param .formula A survival formula object created by create_Surv_formula
#' @param data A data frame containing survival data
#' @param html Logical indicating whether to return HTML formatted output
#' @return If html is TRUE, returns a shiny pre tag with formatted p-value.
#'   If html is FALSE, returns numeric p-value rounded to 4 decimal places.
#' @export
#' @examples
#' \dontrun{
#' # Calculate p-value for survival comparison
#' formula <- create_Surv_formula(
#'   data = clinical_data,
#'   time = "survival_time",
#'   event = "death",
#'   treatment = "treatment_arm"
#' )
#' km_pvalue(formula, clinical_data)
#' }
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
#' @description Fits a Kaplan-Meier survival model and returns tidy survival estimates.
#'   This function handles survival formula creation, optional landmark analysis,
#'   and returns survival probabilities over time for visualization.
#' @param data A data frame containing survival data
#' @param time Character string specifying the time-to-event variable name
#' @param event Character string specifying the event indicator variable name (0/1 or FALSE/TRUE)
#' @param treatment Character string specifying the treatment group variable name
#' @param covariates Character vector of covariate names to include in the model (optional)
#' @param landmark Numeric value for landmark analysis time point. If specified,
#'   analysis starts from this time point forward
#' @param ... Additional arguments passed to survfit function
#' @return A tidy data frame with survival estimates including time, estimate,
#'   confidence intervals, and strata information
#' @export
#' @examples
#' \dontrun{
#' # Fit basic Kaplan-Meier model
#' km_fit <- fit_km(
#'   data = clinical_data,
#'   time = "survival_time",
#'   event = "death",
#'   treatment = "treatment_arm"
#' )
#' 
#' # Fit with landmark analysis
#' km_fit_landmark <- fit_km(
#'   data = clinical_data,
#'   time = "survival_time",
#'   event = "death",
#'   treatment = "treatment_arm",
#'   landmark = 90
#' )
#' }
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
#' @title Add Time Zero to Kaplan-Meier Survival Fit
#' @description Adds time zero (baseline) to Kaplan-Meier survival estimates with 100% survival
#'   probability. This ensures survival curves start at (0, 1) for proper visualization.
#' @param fit A tidy survival fit data frame from fit_km function
#' @return A data frame with time zero added for each stratum, showing 100% survival
#'   at baseline with appropriate number at risk
#' @export
#' @examples
#' \dontrun{
#' # Add time zero to survival fit
#' km_fit <- fit_km(clinical_data, "survival_time", "death", "treatment_arm")
#' km_fit_with_zero <- km_add_0(km_fit)
#' }
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
#' @title Generate Kaplan-Meier Survival Summary
#' @description Creates a formatted summary of survival differences between groups,
#'   including the log-rank test results and group comparisons.
#' @param .formula A survival formula object created by create_Surv_formula
#' @param data A data frame containing survival data
#' @return A shiny pre tag containing formatted survival test summary
#' @export
#' @examples
#' \dontrun{
#' # Generate survival summary
#' formula <- create_Surv_formula(
#'   data = clinical_data,
#'   time = "survival_time",
#'   event = "death",
#'   treatment = "treatment_arm"
#' )
#' km_summary(formula, clinical_data)
#' }
km_summary <- function(.formula, data) {
  res <- survival::survdiff(.formula, data = data) %>%
    shiny::renderPrint() %>%
    .() %>%
    stringr::str_replace(stringr::fixed(".formula"), as.character(.formula)[2])
  shiny::tag("pre", res)
}
# =================================================
#' @title Create Numbers-at-Risk Table for Kaplan-Meier Plots
#' @description Generates a numbers-at-risk table showing how many subjects remain
#'   at specified time intervals for each treatment group. Essential for interpreting
#'   the reliability of survival estimates over time.
#' @param fit A tidy survival fit data frame from fit_km function
#' @param what Character string specifying what to display (default: "n.risk")
#' @param break_width Numeric value for time interval width between table columns
#' @param kable Logical indicating whether to return formatted kable output
#' @return If kable is TRUE, returns formatted HTML table with kableExtra styling.
#'   If kable is FALSE, returns raw data frame with risk table data.
#' @export
#' @examples
#' \dontrun{
#' # Create risk table
#' km_fit <- fit_km(clinical_data, "survival_time", "death", "treatment_arm")
#' risk_table <- km_table(km_fit, break_width = 30)
#' 
#' # Get raw risk table data
#' risk_data <- km_table(km_fit, kable = FALSE)
#' }
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
#' @title Plot Compact Grouped Kaplan-Meier Survival Curves
#' @description Creates compact Kaplan-Meier survival curves arranged horizontally with
#'   minimal legends and reduced spacing. Optimized for displaying multiple survival
#'   curves in limited space, such as in reports or dashboards.
#' @param data A data frame containing survival data
#' @param time Character string specifying the time-to-event variable name
#' @param event Character string specifying the event indicator variable name (0/1 or FALSE/TRUE)
#' @param treatment Character string specifying the treatment group variable name
#' @param covariates Character vector of covariate names to include in the model (optional)
#' @param split Character string specifying the variable to split/group plots by
#' @param width Character string specifying width of each plot (default: "400px")
#' @param add_table Logical indicating whether to add numbers-at-risk table
#' @param break_width Numeric value for time axis break intervals (default: max time / 6)
#' @param add_p Logical indicating whether to add log-rank test p-value
#' @param ... Additional arguments passed to plot_grouped_km function
#' @return HTML div containing horizontally arranged compact survival curves
#' @export
#' @examples
#' \dontrun{
#' # Create compact grouped survival plots
#' plot_grouped_km_compact(
#'   data = clinical_data,
#'   time = "survival_time",
#'   event = "death",
#'   treatment = "treatment_arm",
#'   split = "study_site",
#'   width = "300px"
#' )
#' }
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
#' @title Plot Kaplan-Meier Curves with Covariate Distribution Over Time
#' @description Creates Kaplan-Meier survival curves with additional panels showing
#'   how covariate distributions change over time within each treatment group.
#'   Useful for understanding selection bias and covariate patterns in survival data.
#' @param data A data frame containing survival data
#' @param time Character string specifying the time-to-event variable name
#' @param event Character string specifying the event indicator variable name (0/1 or FALSE/TRUE)
#' @param treatment Character string specifying the treatment group variable name
#' @param covariates Character vector of covariate names to display distributions for
#' @param title Character string for plot title (default: event name)
#' @param subtitle Character string for plot subtitle
#' @param landmark Numeric value for landmark analysis time point
#' @param ylab Character string for y-axis label (default: "Survival")
#' @param xlab Character string for x-axis label (default: "Time (Days)")
#' @param add_p Logical indicating whether to add log-rank test p-value
#' @param add_table Logical indicating whether to add numbers-at-risk table
#' @param add_summary Logical indicating whether to add survival summary
#' @param add_median Logical indicating whether to add median survival time markers
#' @param wrap Character string for facet wrapping variable
#' @param legend_position Character string for legend position
#' @param height Numeric value for plot height in pixels
#' @param width Character string specifying plot width
#' @param colors Character vector of colors for survival curves
#' @param lowrider_theme Character string specifying theme
#' @param as_ggplot Logical indicating whether to return ggplot object
#' @param note Character string for plot notes
#' @param break_width Numeric value for time axis break intervals
#' @return Interactive plotly object with survival curves and covariate distribution panels
#' @export
#' @examples
#' \dontrun{
#' # Plot survival curves with covariate distributions
#' plot_km_covariates(
#'   data = clinical_data,
#'   time = "survival_time",
#'   event = "death",
#'   treatment = "treatment_arm",
#'   covariates = c("age_group", "gender")
#' )
#' }
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
