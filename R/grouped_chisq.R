# =================================================
#' @title Perform Grouped Chi-Square Tests
#' @description 
#' Perform Chi-square tests on subgroups of data split by a specified variable.
#' Useful for analyzing associations across different strata or subgroups.
#' @param df A data frame containing the variables to analyze
#' @param split_by Character string specifying the grouping variable
#' @param x Character string specifying the first categorical variable
#' @param y Character string specifying the second categorical variable
#' @param ... Additional arguments passed to chisq.test
#' @return A list of test results for each subgroup
#' @export
grouped_chisq <- function(df, split_by, x, y, ...) {
  cli::cli_h1("Calculating grouped chisq test")
  df %>%
    waRRior::named_group_split_at(split_by) %>%
    purrr::imap(purrr::safely({
      function(.df, .split) {
        cli::cli_progress_step("{.split}")
        if (is.factor(.df[x])) .df[[x]] <- droplevels(.df[[x]])
        if (is.factor(.df[y])) .df[[y]] <- droplevels(.df[[y]])
        .x <- .df[[x]]
        .y <- .df[[y]]
        info <- base::table(.x, .y) %>%
          chisq.test(...) %>%
          broom::tidy()
        info[[split_by]] <- .split
        info$N <- nrow(.df)
        info <- info %>%
          dplyr::select(!!rlang::sym(split_by), N, statistic, p.value, tidyselect::everything())
        info
      }
    }))
}
# =================================================
#' @title Extract Results from Grouped Chi-Square Tests
#' @description 
#' Extract successful test results from a list of grouped chi-square test results,
#' combining them into a single data frame sorted by p-value.
#' @param x A list of grouped chi-square test results
#' @return A data frame of successful test results sorted by p-value
#' @export
extract_results_grouped_chisq <- function(x) {
  purrr::map(x, "result") %>%
    purrr::compact() %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(p.value)
}
# =================================================
#' @title Extract Errors from Grouped Chi-Square Tests
#' @description 
#' Extract error messages from a list of grouped chi-square test results
#' for debugging and quality assessment.
#' @param x A list of grouped chi-square test results
#' @return A list of error messages from failed tests
#' @export
extract_errors_grouped_chisq <- function(x) {
  purrr::map(x, "error") %>%
    purrr::compact()
}
# =================================================
#' @title Generate Report of Grouped Chi-Square Results
#' @description 
#' Create an interactive reactable report showing the results of grouped 
#' chi-square tests with default formatting.
#' @param x A list of grouped chi-square test results
#' @return A reactable table displaying the test results
#' @export
report_results_grouped_chisq <- function(x) {
  extract_results_grouped_chisq(x) %>%
    nightowl::reactable_default()
}
# =================================================
#' @title Generate Error Report for Grouped Chi-Square Tests
#' @description 
#' Create an HTML report displaying errors encountered during grouped chi-square
#' testing for troubleshooting and quality control.
#' @param x A list of grouped chi-square test results
#' @return A browsable HTML div containing formatted error messages
#' @export
report_errors_grouped_chisq <- function(x) {
  shiny::div(
    shiny::h3("Errors:"),
    extract_errors_grouped_chisq(x) %>%
      purrr::imap(~ shiny::div(
        class = "error lowrider-error",
        shiny::h5(.y),
        as.character(.x)
      ))
  ) %>%
    htmltools::browsable()
}
# =================================================
# =================================================
#' @title Create Plots for Grouped Chi-Square Tests
#' @description 
#' Generate interactive stacked percentage plots for each subgroup in grouped 
#' chi-square analysis with test statistics and p-values.
#' @param df A data frame containing the variables to analyze
#' @param split_by Character string specifying the grouping variable
#' @param x Character string specifying the first categorical variable
#' @param y Character string specifying the second categorical variable
#' @param test Optional pre-computed test results (default: NULL)
#' @param order_by Variable to order results by (default: "p.value")
#' @param pal Optional color palette for plots
#' @param width Plot width in inches (default: 3)
#' @param height Plot height in inches (default: 3)
#' @param flex_direction CSS flex direction for layout (default: "row")
#' @param flex_wrap CSS flex wrap setting (default: "wrap")
#' @param show_info Logical indicating whether to show test statistics
#' @param ... Additional arguments
#' @return A browsable HTML div containing the plots
#' @export
plot_grouped_chisq <- function(df, split_by, x, y, test = NULL, order_by = "p.value",
                               pal = NULL,
                               width = 3,
                               height = 3,
                               flex_direction = "row",
                               flex_wrap = "wrap",
                               show_info = TRUE,
                               ...) {
  if (is.null(test)) {
    test <- nightowl::grouped_chisq(df, split_by, x, y) %>%
      nightowl::extract_results_grouped_chisq()
  }
  test <- test %>%
    dplyr::arrange(.data[[order_by]])
  split_order <- test[[split_by]]
  test <- waRRior::named_group_split_at(test, split_by)
  df_split <- waRRior::named_group_split_at(df, split_by)
  df_split <- df_split[split_order]
  cli::cli_h1("Plotting grouped chisq")

  .l <- list(data = df_split, info = test, name = names(df_split))

  purrr::pmap(.l, function(data, info, name) {
    cli::cli_progress_step("{name}")
    if (show_info) {
      info <- info %>%
        dplyr::select(N, statistic, `p.value`) %>%
        dplyr::mutate_if(is.numeric, function(x) round(x, 5)) %>%
        purrr::imap(~ glue::glue("{.y}: {.x}")) %>%
        paste(collapse = "\n") %>%
        paste("χ2 -- ", .)
    } else {
      info <- NULL
    }
    p <- nightowl::plot_stacked_percentages(data, x = x, y = y, add_labels = F) +
      ggplot2::coord_flip() +
      ggplot2::ggtitle(stringr::str_wrap(name)) +
      ggplot2::labs(caption = info) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        legend.position = "none",
        plot.caption = ggplot2::element_text(hjust = 0, face = "italic")
      )
    if (!is.null(pal)) {
      p <- p +
        ggplot2::scale_fill_manual(values = pal)
    }
    nightowl::ggplot_to_girafe(p, width = width, height = height)
  }) %>%
    shiny::div(style = glue::glue("display:flex;flex-wrap:{flex_wrap};flex-direction:{flex_direction};")) %>%
    htmltools::browsable()
}
# =================================================
#' @title Create Reactable for Grouped Chi-Square Tests  
#' @description 
#' Generate an interactive reactable table display for grouped chi-square analysis
#' with embedded plots and test statistics.
#' @param df A data frame containing the variables to analyze
#' @param split_by Character string specifying the grouping variable
#' @param x Character string specifying the first categorical variable
#' @param y Character string specifying the second categorical variable
#' @param test Optional pre-computed test results (default: NULL)
#' @param order_by Variable to order results by (default: "p.value")
#' @param pal Optional color palette for plots
#' @param width Plot width in inches (default: 3)
#' @param height Plot height in inches (default: 3)
#' @param ... Additional arguments
#' @return A browsable HTML div containing the reactable
#' @export
reactable_grouped_chisq <- function(df, split_by, x, y, test = NULL, order_by = "p.value",
                                    pal = NULL,
                                    width = 3,
                                    height = 3,
                                    ...) {
  if (is.null(test)) {
    test <- nightowl::grouped_chisq(df, split_by, x, y) %>%
      nightowl::extract_results_grouped_chisq()
  }
  test <- test %>%
    dplyr::arrange(.data[[order_by]])
  split_order <- test[[split_by]]
  test <- waRRior::named_group_split_at(test, split_by)
  df_split <- waRRior::named_group_split_at(df, split_by)
  df_split <- df_split[split_order]
  cli::cli_h1("Reactable Grouped Chisq")

  .l <- list(data = df_split, info = test, name = names(df_split))
  purrr::pmap(.l, function(data, info, name) {
    cli::cli_progress_step("{name}")
    info <- info %>%
      dplyr::select(N, statistic, `p.value`) %>%
      dplyr::mutate_if(is.numeric, function(x) round(x, 5)) %>%
      purrr::imap(~ glue::glue("{.y}: {.x}")) %>%
      paste(collapse = "\n") %>%
      paste("χ2 -- ", .)
    p <- nightowl::plot_stacked_percentages(data, x = x, y = y, add_labels = F) +
      ggplot2::coord_flip() +
      ggplot2::ggtitle(stringr::str_wrap(name)) +
      ggplot2::labs(caption = info) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        legend.position = "none",
        plot.caption = ggplot2::element_text(hjust = 0, face = "italic")
      )
    if (!is.null(pal)) {
      p <- p +
        ggplot2::scale_fill_manual(values = pal)
    }
    nightowl::ggplot_to_girafe(p, width = width, height = height)
  }) %>%
    shiny::div(style = glue::glue("display:flex;flex-wrap:{flex_wrap};flex-direction:{flex_direction};")) %>%
    htmltools::browsable()
}
# =================================================
