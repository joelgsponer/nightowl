# =================================================
#' @title Calculate Statistical Test
#' @description Automatically selects and performs appropriate statistical test based on variable type
#' @param data Data frame containing the variables to test
#' @param y Character string specifying the dependent variable name
#' @param x Character string specifying the grouping variable name (optional, uses existing groups if NULL)
#' @param gracefully Logical indicating whether to return an error object instead of stopping on error (default: TRUE)
#' @param ... Additional arguments passed to the test function
#' @return List containing test results with components: test (test object), method (test name), p_value (p-value), and optionally error (if gracefully = TRUE)
#' @export
calc_test <- function(data, y, x = NULL, gracefully = T, ...) {
  tryCatch(
    {
      stopifnot(y %in% names(data))
      if (is.null(x)) {
        .groups <- nightowl_get_groups(data)
      } else {
        .groups <- x
      }
      if (is.null(.groups)) stop("No groups found")
      type_y <- class(data[[y]])
      switch(type_y,
        numeric = nightowl::calc_test_kruskal(data, y, ...),
        factor = nightowl::calc_test_chisq(data, y, ...),
        character = nightowl::calc_test_chisq(data, y, ...)
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
#' @title Calculate Kruskal-Wallis Test
#' @description Performs Kruskal-Wallis rank sum test for numeric variables across groups
#' @param data Data frame containing the variables to test
#' @param y Character string specifying the numeric dependent variable name
#' @param x Character string specifying the grouping variable name (optional, uses existing groups if NULL)
#' @param ... Additional arguments passed to kruskal.test
#' @return List containing test results with components: test (kruskal.test object), method ("Kruskal-Wallis"), p_value (p-value)
#' @export
calc_test_kruskal <- function(data, y, x = NULL, ...) {
  if (is.null(x)) {
    .groups <- nightowl_get_groups(data)
  } else {
    .groups <- x
  }
  test <- data %>%
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
#' @title Calculate Chi-Square Test
#' @description Performs Pearson's chi-square test for categorical variables across groups
#' @param data Data frame containing the variables to test
#' @param y Character string specifying the categorical dependent variable name
#' @param x Character string specifying the grouping variable name (optional, uses existing groups if NULL)
#' @param correct Logical indicating whether to apply Yates' continuity correction (default: FALSE)
#' @param ... Additional arguments passed to chisq.test
#' @return List containing test results with components: test (chisq.test object), table (contingency table), method ("Pearson's χ2 test"), p_value (p-value)
#' @export
calc_test_chisq <- function(data, y, x = NULL, correct = F, ...) {
  if (is.null(x)) {
    .groups <- nightowl_get_groups(data)
  } else {
    .groups <- x
  }
  cont_table <- data %>%
    dplyr::select_at(c(y, .groups)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(group = paste(!!!rlang::syms(.groups))) %>%
    nightowl_drop_columns(.groups) %>%
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
#' @title Test R6 Class
#' @description R6 class for conducting and storing statistical test results
#' @details This class provides a convenient interface for performing statistical tests on grouped data,
#' automatically selecting the appropriate test based on the variable type and storing all relevant results
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
    initialize = function(data, y, x = NULL, ...) {
      if (!inherits(data, "tbl_df")) data <- tibble::as_tibble(data)
      if (is.null(x)) {
        self$x <- nightowl_get_groups(data)
        if (is.null(self$x)) stop("No groups found - either provide x variables or group the data beforehand")
      } else {
        self$x <- x
        data <- dplyr::ungroup(data)
      }
      self$data <- data
      self$y <- y
      self$args <- list(...)
      .test <- do.call(nightowl::calc_test, c(list(data = self$data, y = self$y, x = self$x), self$args))
      purrr::imap(.test, function(.x, .y) self[[.y]] <- .x)
      self$footnote <- glue::glue("{self$method}: {nightowl::format_p_value(self$p_value)}")
      invisible(self)
    }
  )
)
