# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
calc_test <- function(.data, y, x = NULL, gracefully = T, ...) {
  tryCatch(
    {
      stopifnot(y %in% names(.data))
      if (is.null(x)) {
        .groups <- waRRior::get_groups(.data)
      } else {
        .groups <- x
      }
      if (is.null(.groups)) stop("No groups found")
      type_y <- class(.data[[y]])
      switch(type_y,
        numeric = nightowl::calc_test_kruskal(.data, y, ...),
        factor = nightowl::calc_test_chisq(.data, y, ...),
        character = nightowl::calc_test_chisq(.data, y, ...)
      )
    },
    error = function(e) {
      if (!gracefully) stop(e)
      list(
        test = NULL,
        method = "Calculation failed",
        p_value = NA,
        error = e
      )
    }
  )
}
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
calc_test_kruskal <- function(.data, y, x = NULL, ...) {
  if (is.null(x)) {
    .groups <- waRRior::get_groups(.data)
  } else {
    .groups <- x
  }
  test <- .data %>%
    dplyr::group_split() %>%
    purrr::imap(~ .x[[y]]) %>%
    kruskal.test(...)
  list(
    test = test,
    method = "Kruskal-Wallis",
    p_value = test$p.value
  )
}
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
calc_test_chisq <- function(.data, y, x = NULL, correct = F, ...) {
  if (is.null(x)) {
    .groups <- waRRior::get_groups(.data)
  } else {
    .groups <- x
  }
  cont_table <- .data %>%
    dplyr::select_at(c(y, .groups)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(group = paste(!!!rlang::syms(.groups))) %>%
    waRRior::drop_columns(.groups) %>%
    droplevels() %>%
    base::table()
  test <- chisq.test(cont_table, correct = correct, ...)
  list(
    test = test,
    table = cont_table,
    method = "Pearson's χ2 test",
    p_value = test$p.value
  )
}
# =================================================
#' R6 Class
#' @description
#' @detail
#' @export
Test <- R6::R6Class("Test",
  public = list(
    data = NULL,
    x = NULL,
    y = NULL,
    groups = NULL,
    method = NULL,
    test = NULL,
    p_value = NULL,
    table = NULL,
    error = NULL,
    footnote = NULL,
    args = NULL,
    initialize = function(.data, y, x = NULL, ...) {
      if (!inherits(.data, "tbl_df")) .data <- tibble::as_tibble(.data)
      if (is.null(x)) {
        self$x <- waRRior::get_groups(.data)
        if (is.null(self$x)) stop("No groups found - either provide x variables or group the data beforehand")
      } else {
        self$x <- x
        .data <- dplyr::ungroup(.data)
      }
      self$data <- .data
      self$y <- y
      self$args <- list(...)
      .test <- do.call(nightowl::calc_test, c(list(.data = self$data, y = self$y, x = self$x), self$args))
      purrr::imap(.test, function(.x, .y) self[[.y]] <- .x)
      self$footnote <- glue::glue("{self$method}: {nightowl::format_p_value(self$p_value)}")
      invisible(self)
    }
  )
)
