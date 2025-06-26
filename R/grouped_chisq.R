# =================================================
#' @title Perform chi-square tests across grouped data
#' @description
#' Conducts chi-square tests of independence between two categorical variables
#' for each group defined by a splitting variable, with error handling.
#' @param df A data frame containing the variables for analysis
#' @param split_by String. Name of the grouping variable to split data by
#' @param x String. Name of the first categorical variable for the chi-square test
#' @param y String. Name of the second categorical variable for the chi-square test
#' @param ... Additional arguments passed to chisq.test()
#' @return A list of safely executed chi-square test results for each group
#' @export
grouped_chisq <- function(data, group_by, x, y, ...) {
  cli::cli_h1("Calculating grouped chisq test")
  data %>%
    nightowl_named_group_split_at(data, group_by) %>%
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
#' @title Extract successful chi-square test results
#' @description
#' Extracts and combines successful chi-square test results from grouped analysis,
#' excluding any failed tests and sorting by p-value.
#' @param x List output from grouped_chisq() containing results and errors
#' @return A tibble with combined chi-square test results sorted by p-value
#' @export
extract_results_grouped_chisq <- function(x) {
  purrr::map(x, "result") %>%
    purrr::compact() %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(p.value)
}
# =================================================
#' @title Extract chi-square test errors
#' @description
#' Extracts error messages from grouped chi-square tests that failed to execute,
#' useful for debugging problematic groups.
#' @param x List output from grouped_chisq() containing results and errors
#' @return A list of error messages from failed chi-square tests
#' @export
extract_errors_grouped_chisq <- function(x) {
  purrr::map(x, "error") %>%
    purrr::compact()
}
# =================================================
#' @title Generate interactive table report of chi-square results
#' @description
#' Creates an interactive reactable displaying chi-square test results
#' from grouped analysis in a user-friendly format.
#' @param x List output from grouped_chisq() containing test results
#' @return An interactive reactable widget displaying the chi-square results
#' @export
report_results_grouped_chisq <- function(x) {
  extract_results_grouped_chisq(x) %>%
    nightowl::reactable_default()
}
# =================================================
#' @title Generate HTML report of chi-square test errors
#' @description
#' Creates an HTML-formatted report displaying any errors that occurred
#' during grouped chi-square testing for troubleshooting.
#' @param x List output from grouped_chisq() containing error information
#' @return An HTML div containing formatted error messages
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
#' @title Create stacked percentage plots for chi-square results
#' @description
#' Generates interactive stacked percentage plots for each group in chi-square analysis,
#' ordered by statistical significance and annotated with test statistics.
#' @param df A data frame containing the variables for visualization
#' @param split_by String. Name of the grouping variable to split plots by
#' @param x String. Name of the first categorical variable for plotting
#' @param y String. Name of the second categorical variable for plotting
#' @param test Optional. Pre-computed chi-square test results. If NULL, tests are computed
#' @param order_by String. Variable to order plots by. Default "p.value"
#' @param pal Named vector of colors for the plots. If NULL, uses default colors
#' @param width Numeric. Width of individual plots. Default 3
#' @param height Numeric. Height of individual plots. Default 3
#' @param flex_direction String. CSS flex direction for layout. Default "row"
#' @param flex_wrap String. CSS flex wrap for layout. Default "wrap"
#' @param show_info Logical. Whether to show test statistics. Default TRUE
#' @param ... Additional arguments passed to chi-square testing
#' @return An HTML div containing interactive plots arranged in a flexible layout
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
#' @title Create reactable visualization for grouped chi-square analysis
#' @description
#' Generates an interactive reactable with embedded plots for grouped chi-square analysis,
#' combining statistical results with visual representations.
#' @param df A data frame containing the variables for analysis
#' @param split_by String. Name of the grouping variable to split analysis by
#' @param x String. Name of the first categorical variable
#' @param y String. Name of the second categorical variable
#' @param test Optional. Pre-computed chi-square test results. If NULL, tests are computed
#' @param order_by String. Variable to order results by. Default "p.value"
#' @param pal Named vector of colors for the plots. If NULL, uses default colors
#' @param width Numeric. Width of embedded plots. Default 3
#' @param height Numeric. Height of embedded plots. Default 3
#' @param ... Additional arguments passed to chi-square testing
#' @return An interactive reactable widget with embedded visualizations
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
