# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
summary <- function(data,
                    column,
                    group_by = NULL,
                    .mean = ggplot2::mean_cl_boot,
                    add_forest = TRUE,
                    output = "raw",
                    ...) {
  cli::cli_h1("Calculating summary for {column}")
  if (output == "raw") add_forest <- FALSE
  .data <- data %>%
    dplyr::select_at(c(column, group_by))
  if (!is.null(group_by)) {
    .data <- dplyr::group_by_at(.data, group_by)
  }

  if (is.numeric(.data[[column]])) {
    raw <- nightowl::calc_summary_numeric(data = .data, column = column, ...)
    if (add_forest) raw <- nightowl::add_forestplot(raw, "Mean.y", "Mean.ymin", "Mean.ymax", xintercept = mean(raw$Mean.y))
    res <- raw %>%
      dplyr::mutate_if(is.numeric, ~ round(.x, 1)) %>%
      dplyr::mutate(Mean = glue::glue("{Mean.y}({Mean.ymin}-{Mean.ymax})")) %>%
      dplyr::select(-Mean.y, -Mean.ymin, -Mean.ymax)
    if (add_forest) {
      res <- res %>%
        dplyr::select_at(c(waRRior::pop(names(res), "Forest"), "Forest"))
    }
  } else {
    res <- nightowl::calc_summary_categorical(data = .data, column = column, ...)
  }



  attributes(res)$column <- column
  attributes(res)$group_by <- group_by
  if (output == "raw") {
    return(res)
  }
  if (output == "kable") {
    return(nightowl::render_kable(res, caption = glue::glue("Summary for {column}"), ...))
  }
}
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
calc_summary <- function(data, column, calculations = list(`N.` = nightowl::n), unnest = TRUE, names_sep = ".") {
  .calculations <- purrr::imap(calculations, ~ list(column = .y, calculation = .x))
  .group <- attributes(data)$groups
  if (!is.null(.group)) {
    .group <- names(.group)[names(.group) %in% names(data)]
    cli::cli_progress_step("Data is grouped: {.group}")
  }
  res <- purrr::reduce(.calculations, function(.x, .y) {
    cli::cli_progress_step("Calculating {.y$column}")
    .x <- .x %>%
      dplyr::group_split() %>%
      purrr::map(
        function(.thisgroup) {
          dplyr::mutate(dplyr::ungroup(.thisgroup), !!rlang::sym(.y$column) := .y$calculation(!!rlang::sym(column)))
        }
      )
    .x <- dplyr::bind_rows(.x) %>%
      dplyr::group_by_at(.group)
    if (unnest) {
      if (any(class(.x[[.y$column]]) %in% c("tibble", "data.frame", "list"))) {
        cli::cli_progress_step("Unesting {.y$column}")
        .x <- tidyr::unnest(.x, !!rlang::sym(.y$column), names_sep = names_sep, names_repair = "minimal")
      }
    }
    return(.x)
  }, .init = data)

  .new_columns <- names(res)[!names(res) %in% names(data)]

  res <- res %>%
    dplyr::select_at(c(.group, .new_columns)) %>%
    dplyr::distinct()

  return(res)
}
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
calc_summary_numeric <- function(data, column, calculations = list(`N.` = nightowl::n, Median = median, Mean = ggplot2::mean_cl_boot), unnest = TRUE) {
  do.call(nightowl::calc_summary, as.list(environment()))
}
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
calc_summary_categorical <- function(data, column, calculations = list(`N.` = nightowl::n, Freq = nightowl::frequencies, Bar = function(x) {
                                       nightowl::frequencies(x,
                                         output = "barplot"
                                       )
                                     }), unnest = TRUE, names_sep = NULL) {
  do.call(nightowl::calc_summary, as.list(environment()))
}
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
frequencies <- function(x, output = "print") {
  N <- length(x)
  raw <- tibble::tibble(x = x) %>%
    dplyr::mutate(x = factor(x)) %>%
    dplyr::mutate(x = forcats::fct_explicit_na(x)) %>%
    dplyr::group_by(x) %>%
    dplyr::summarise(count = dplyr::n(), percent = dplyr::n() / N * 100)
  stopifnot(round(sum(raw$percent)) == 100)
  if (output == "raw") res <- raw
  if (output == "count") {
    res <- raw %>%
      dplyr::select_at(c("x", "count")) %>%
      tidyr::pivot_wider(names_from = "x", values_from = "count", names_repair = "minimal")
  }
  if (output == "percent") {
    res <- raw %>%
      dplyr::select_at(c("x", "percent")) %>%
      tidyr::pivot_wider(names_from = "x", values_from = "percent", names_repair = "minimal")
  }
  if (output == "barplot") res <- nightowl::add_barplot(raw)
  if (output == "print") {
    res <- raw %>%
      dplyr::mutate_if(is.numeric, ~ round(.x, 1)) %>%
      dplyr::mutate(value = glue::glue("{percent}% ({count})")) %>%
      dplyr::select_at(c("x", "value")) %>%
      tidyr::pivot_wider(names_from = "x", values_from = "value", names_repair = "minimal")
  }
  return(res)
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
n <- function(...) {
  dplyr::n()
}
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
add_barplot <- function(raw) {
  raw <- raw %>%
    dplyr::mutate(x = forcats::fct_inorder(x)) %>%
    dplyr::mutate(x = forcats::fct_rev(x))
  .p <- raw %>%
    ggplot2::ggplot(ggplot2::aes(x = 1, y = percent, fill = x, tooltip = x, data_id = x)) +
    ggiraph::geom_col_interactive() +
    ggplot2::theme_void() +
    ggplot2::coord_flip() +
    ggplot2:::scale_fill_viridis_d() +
    ggplot2::theme(
      plot.margin = ggplot2::margin(0, 0, 0, 0, "cm"),
      panel.spacing = grid::unit(0, "cm"),
      legend.position = "none"
    )
  nightowl::render_svg(.p, height = 0.8, add_download_button = FALSE) %>%
    shiny::HTML()
}
# =================================================
