#' Summarise one column per group
#'
#' The engine behind [Summary]. A summary is a named list of `calculations`,
#' each a function of the column's values, applied within every group of
#' `data`. A calculation may return a scalar (one column), a one-row data
#' frame (several columns), or a [NightowlPlots] vector (an inline plot).
#' `parameters` supplies extra arguments per calculation. A `template` bundles
#' both; the `summarise_*()` functions are templates.
#'
#' @param data A data frame, grouped with [dplyr::group_by()] for per-group
#'   summaries.
#' @param column Name of the column to summarise.
#' @param template A list with elements `calculations` and `parameters`, or a
#'   function returning one when called with `list(data, column)`.
#' @param calculations Named list of functions (or function names). Overrides
#'   the template.
#' @param parameters Named list of argument lists, by calculation name.
#' @param unnest Spread data-frame results into columns (`TRUE`) or keep them
#'   as a list column.
#' @param name_for_column Name of the column holding `column`.
#' @return A tibble with the grouping columns, `name_for_column`, and one
#'   column per calculation result, grouped like `data`.
#' @examples
#' calc_summary(dplyr::group_by(mtcars, cyl), "mpg", template = summarise_numeric())
#' calc_summary(mtcars, "mpg", calculations = list(Mean = mean, SD = sd))
#' @export
calc_summary <- function(data, column, template = NULL, calculations = NULL, parameters = NULL,
                         unnest = TRUE, name_for_column = "Variable") {
  check_data_frame(data)
  check_string(column)
  check_columns(data, column)
  if (is.function(template)) template <- template(list(data = data, column = column))
  calculations <- calculations %||% template$calculations
  parameters <- parameters %||% template$parameters %||% list()
  if (is.null(calculations)) {
    cli::cli_inform("No calculations given; counting observations.")
    calculations <- list(N = length)
  }
  if (!is.list(calculations) || is.null(names(calculations)) || any(!nzchar(names(calculations)))) {
    cli::cli_abort("{.arg calculations} must be a named list of functions.")
  }
  groups <- dplyr::group_vars(data)
  pieces <- named_group_split(data, groups)
  keys <- attr(pieces, "keys")
  rows <- purrr::map(pieces, function(piece) {
    values <- piece[[column]]
    cells <- purrr::imap(calculations, function(fn, name) {
      fn <- resolve_function(fn)
      res <- rlang::exec(fn, values, !!!(parameters[[name]] %||% list()))
      wrap_result(res, name, unnest)
    })
    dplyr::bind_cols(unname(cells), .name_repair = "minimal")
  })
  out <- dplyr::bind_rows(rows)
  out <- vctrs::vec_cbind(tibble::tibble(!!name_for_column := rep(column, nrow(out))), out, .name_repair = "minimal")
  if (length(groups) > 0) {
    out <- vctrs::vec_cbind(keys, out, .name_repair = "minimal")
  }
  out <- tibble::as_tibble(out, .name_repair = "unique_quiet")
  dplyr::group_by(out, dplyr::across(dplyr::all_of(groups)))
}

wrap_result <- function(res, name, unnest) {
  if (is_Plot(res)) res <- new_NightowlPlots(res)
  if (is_NightowlPlots(res)) {
    return(tibble::tibble(!!name := res))
  }
  if (is.data.frame(res)) {
    if (nrow(res) != 1) {
      cli::cli_abort("Calculation {.val {name}} returned {nrow(res)} rows; one row is required.")
    }
    if (unnest) {
      return(tibble::as_tibble(res))
    }
    return(tibble::tibble(!!name := list(res)))
  }
  if (length(res) == 1 && is.atomic(res)) {
    return(tibble::tibble(!!name := res))
  }
  tibble::tibble(!!name := list(res))
}

#' Summary templates
#'
#' @description
#' Ready-made sets of calculations for [calc_summary()] and [Summary]. Each
#' returns `list(calculations, parameters)`. Templates that draw inline plots
#' need the data to compute shared axis limits, so they take a `summary`
#' argument: any object with `$data` and `$column`, such as a [Summary] or
#' `list(data = , column = )`. Called without it they still return a usable
#' template with unshared limits.
#'
#' * `summarise_categorical()`: count and percent per level.
#' * `summarise_categorical_barplot()`: adds a stacked bar.
#' * `summarise_numeric()`: count, missing, extreme values, median, range,
#'   mean with CI.
#' * `summarise_numeric_pointrange()`: mean, median and an inline point range.
#' * `summarise_numeric_histogram()`: descriptives and an inline histogram.
#' * `summarise_numeric_violin()`: descriptives and an inline violin.
#'
#' @param summary Optional object with `$data` and `$column`.
#' @param ... Overrides for `parameters`, by calculation name.
#' @return A list with elements `calculations` and `parameters`.
#' @examples
#' summarise_numeric()$calculations |> names()
#' s <- Summary$new(mtcars, "mpg", group_by = "cyl", method = summarise_numeric_pointrange)
#' s$raw()
#' @name summary_templates
NULL

#' @rdname summary_templates
#' @export
summarise_categorical <- function(summary = NULL, ...) {
  template(
    calculations = list(N = length, Freq = format_frequencies),
    parameters = list(...)
  )
}

#' @rdname summary_templates
#' @export
summarise_categorical_barplot <- function(summary = NULL, ...) {
  template(
    calculations = list(N = length, Freq = format_frequencies, Barplot = add_inline_barplot),
    parameters = list(...)
  )
}

#' @rdname summary_templates
#' @export
summarise_numeric <- function(summary = NULL, ...) {
  template(
    calculations = list(
      N = length,
      Missing = function(x) sum(is.na(x)),
      Extreme = count_extreme_values,
      Median = function(x) median(x, na.rm = TRUE),
      Min = function(x) min(x, na.rm = TRUE),
      Max = function(x) max(x, na.rm = TRUE),
      Mean = format_mean_ci
    ),
    parameters = list(...)
  )
}

#' @rdname summary_templates
#' @export
summarise_numeric_pointrange <- function(summary = NULL, ...) {
  template(
    calculations = list(
      N = length,
      Median = function(x) median(x, na.rm = TRUE),
      Mean = format_mean_ci,
      Pointrange = add_inline_pointrange
    ),
    parameters = list(Pointrange = list(xlim = column_range(summary))),
    ...
  )
}

#' @rdname summary_templates
#' @export
summarise_numeric_histogram <- function(summary = NULL, ...) {
  template(
    calculations = list(
      N = function(x) sum(!is.na(x)),
      Median = function(x) median(x, na.rm = TRUE),
      Mean = format_mean_ci,
      Min = function(x) min(x, na.rm = TRUE),
      Max = function(x) max(x, na.rm = TRUE),
      Histogram = add_inline_histogram
    ),
    parameters = list(Histogram = list(xlim = column_range(summary, pad = 0.05))),
    ...
  )
}

#' @rdname summary_templates
#' @export
summarise_numeric_violin <- function(summary = NULL, ...) {
  template(
    calculations = list(
      N = length,
      Median = function(x) median(x, na.rm = TRUE),
      Mean = format_mean_ci,
      Violin = add_inline_violin
    ),
    parameters = list(Violin = list(ylim = column_range(summary))),
    ...
  )
}

template <- function(calculations, parameters = list(), ...) {
  overrides <- list(...)
  parameters <- purrr::map(parameters, purrr::compact)
  parameters <- purrr::compact(parameters)
  for (name in names(overrides)) {
    parameters[[name]] <- modifyList(parameters[[name]] %||% list(), overrides[[name]])
  }
  list(calculations = calculations, parameters = parameters)
}

column_range <- function(summary, pad = 0) {
  if (is.null(summary)) {
    return(NULL)
  }
  values <- summary$data[[summary$column]]
  if (!is.numeric(values) || all(is.na(values))) {
    return(NULL)
  }
  r <- range(values, na.rm = TRUE)
  r + c(-1, 1) * pad * diff(r)
}
