#' Test for a difference between groups
#'
#' Picks a test from the type of the response: Kruskal-Wallis for numeric
#' columns, Pearson's chi-squared test for factors and characters.
#'
#' @param data A data frame.
#' @param column Response column.
#' @param groups Grouping columns; defaults to the data's `dplyr` groups.
#' @param correct Continuity correction for the chi-squared test.
#' @param ... Passed to the test function.
#' @return A list with `method`, `statistic`, `p_value`, `footnote` (a
#'   formatted sentence) and `test` (the `htest` object).
#' @examples
#' calc_group_test(mtcars, "mpg", groups = "cyl")$footnote
#' calc_group_test(mtcars, "am", groups = "cyl")$method
#' @export
calc_group_test <- function(data, column, groups = dplyr::group_vars(data), correct = FALSE, ...) {
  check_data_frame(data)
  check_string(column)
  check_columns(data, c(column, groups))
  if (length(groups) == 0) {
    cli::cli_abort("No {.arg groups} given and {.arg data} is not grouped.")
  }
  data <- dplyr::ungroup(data)
  y <- data[[column]]
  g <- interaction(data[groups], drop = TRUE)
  if (is.numeric(y)) {
    test <- stats::kruskal.test(y, g, ...)
    method <- "Kruskal-Wallis test"
  } else if (is.factor(y) || is.character(y) || is.logical(y)) {
    test <- suppressWarnings(stats::chisq.test(table(explicit_missing(y), g), correct = correct, ...))
    method <- "Pearson's chi-squared test"
  } else {
    cli::cli_abort("No test available for a column of type {.cls {class(y)}}.")
  }
  p <- unname(test$p.value)
  list(
    method = method,
    statistic = unname(test$statistic),
    p_value = p,
    footnote = glue::glue("{method}: p = {format_p_value(p, html = FALSE)}"),
    test = test
  )
}
