# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
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
#' @title
#' missing_title
#' @description
#' @detail
#' @param
#' @return
#' @export
extract_results_grouped_chisq <- function(x) {
  purrr::map(x, "result") %>%
    purrr::compact() %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(p.value)
}
# =================================================
#' @title
#' missing_title
#' @description
#' @detail
#' @param
#' @return
#' @export
extract_errors_grouped_chisq <- function(x) {
  purrr::map(x, "error") %>%
    purrr::compact()
}
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
report_results_grouped_chisq <- function(x) {
  extract_results_grouped_chisq(x) %>%
    nightowl::reactable_default()
}
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
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
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
plot_grouped_chisq <- function(df, split_by, x, y, test = NULL, order_by = "p.value",
                               pal = NULL,
                               width = 3,
                               height = 3,
                               flex_direction = "row",
                               flex_wrap = "wrap",
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
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
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
  browser()
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
