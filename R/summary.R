#' A summary table for one variable
#'
#' @description
#' `Summary` summarises one column of a data frame, per group, with a
#' template of calculations (see [summary_templates]), optionally runs a
#' test for group differences, and renders the result as a tibble, a
#' kableExtra table, an HTML block, or a reactable. Inline plots produced by
#' the template are rendered inside the table.
#'
#' @examples
#' s <- Summary$new(mtcars, "mpg", group_by = "cyl", method = summarise_numeric_pointrange)
#' s$raw()
#' s$footnote()
#' \donttest{
#' s$reactable()
#' }
#' @export
Summary <- R6::R6Class(
  "Summary",
  public = list(
    #' @field data The data, grouped by `group_by`, restricted to the used columns.
    data = NULL,
    #' @field column The summarised column.
    column = NULL,
    #' @field group_by Grouping columns.
    group_by = NULL,
    #' @field method Template function or list, see [summary_templates].
    method = NULL,
    #' @field labels Named list or vector mapping column names to labels.
    labels = NULL,
    #' @field keep_variable Keep the column naming the variable.
    keep_variable = TRUE,
    #' @field add_caption Toggle for the caption.
    add_caption = TRUE,
    #' @field add_footnote Toggle for the footnote.
    add_footnote = TRUE,
    #' @field add_test Toggle for the group test.
    add_test = TRUE,
    #' @field arrange_by Column to sort the result by, or `NULL`.
    arrange_by = NULL,
    #' @field options_kable Extra arguments for [render_kable()].
    options_kable = list(),
    #' @field options_reactable Extra arguments for [render_reactable()].
    options_reactable = list(),
    #' @field options_test Extra arguments for [calc_group_test()].
    options_test = list(),

    #' @description Create a summary.
    #' @param data A data frame. If grouped, the groups are used unless
    #'   `group_by` is given.
    #' @param column Column to summarise.
    #' @param group_by Grouping columns.
    #' @param method Template; defaults to [summarise_numeric()] or
    #'   [summarise_categorical()] by column type.
    #' @param labels Optional labels for column names.
    #' @param keep_variable,add_caption,add_footnote,add_test,arrange_by,options_kable,options_reactable,options_test
    #'   See the fields.
    initialize = function(data, column, group_by = NULL, method = NULL, labels = NULL,
                          keep_variable = TRUE, add_caption = TRUE, add_footnote = TRUE, add_test = TRUE,
                          arrange_by = NULL, options_kable = list(), options_reactable = list(),
                          options_test = list()) {
      check_data_frame(data)
      check_string(column)
      group_by <- group_by %||% dplyr::group_vars(data)
      check_columns(data, c(column, group_by))
      self$column <- column
      self$group_by <- group_by
      self$labels <- labels
      self$keep_variable <- keep_variable
      self$add_caption <- add_caption
      self$add_footnote <- add_footnote
      self$add_test <- add_test && length(group_by) > 0
      self$arrange_by <- arrange_by
      self$options_kable <- options_kable
      self$options_reactable <- options_reactable
      self$options_test <- options_test
      self$data <- dplyr::ungroup(data) |>
        dplyr::select(dplyr::all_of(c(group_by, column))) |>
        dplyr::mutate(dplyr::across(dplyr::where(is.character), factor)) |>
        dplyr::mutate(dplyr::across(dplyr::where(is.factor), explicit_missing)) |>
        dplyr::group_by(dplyr::across(dplyr::all_of(group_by)))
      self$method <- method %||% if (is.numeric(self$data[[column]])) summarise_numeric else summarise_categorical
      invisible(self)
    },

    #' @description The resolved template (`calculations` and `parameters`).
    template = function() {
      if (is.function(self$method)) self$method(self) else self$method
    },

    #' @description Run the group test.
    #' @return A list as returned by [calc_group_test()], or `NULL` when
    #'   there is no test; a failed test yields `p_value = NA` and the error
    #'   message in `error`.
    test = function() {
      if (!self$add_test) {
        return(NULL)
      }
      if (!is.null(private$test_cache)) {
        return(private$test_cache)
      }
      private$test_cache <- tryCatch(
        rlang::exec(calc_group_test, self$data, self$column, groups = self$group_by, !!!self$options_test),
        error = function(e) list(method = "Test failed", p_value = NA_real_,
                                 footnote = glue::glue("Test failed: {conditionMessage(e)}"), error = e)
      )
      private$test_cache
    },

    #' @description Caption text, or `NULL`.
    caption = function() {
      if (!self$add_caption) {
        return(NULL)
      }
      glue::glue("Summary of {apply_labels(self$column, self$labels)}")
    },

    #' @description Footnote text (the test result), or `NULL`.
    footnote = function() {
      if (!self$add_footnote || is.null(self$test())) {
        return(NULL)
      }
      as.character(self$test()$footnote)
    },

    #' @description The summary as a tibble.
    #' @param drop Columns to drop from the result.
    raw = function(drop = NULL) {
      tpl <- self$template()
      res <- calc_summary(self$data, self$column, calculations = tpl$calculations, parameters = tpl$parameters)
      res <- dplyr::ungroup(res)
      if (!self$keep_variable) res$Variable <- NULL
      if (!is.null(self$arrange_by)) res <- dplyr::arrange(res, .data[[self$arrange_by]])
      if (!is.null(drop)) res <- dplyr::select(res, -dplyr::any_of(drop))
      if (!is.null(self$labels)) {
        names(res) <- apply_labels(names(res), self$labels)
        if ("Variable" %in% names(res)) res$Variable <- apply_labels(res$Variable, self$labels)
      }
      dplyr::group_by(res, dplyr::across(dplyr::all_of(apply_labels(self$group_by, self$labels))))
    },

    #' @description Render with [render_kable()].
    #' @param drop Columns to drop.
    #' @param ... Passed to [render_kable()].
    kable = function(drop = NULL, ...) {
      rlang::exec(render_kable, self$raw(drop), caption = self$caption(), footnote = self$footnote(),
                  !!!modifyList(self$options_kable, list(...)))
    },

    #' @description Render with [render_html()].
    #' @param drop Columns to drop.
    #' @param ... Passed to [render_html()].
    html = function(drop = NULL, ...) {
      rlang::exec(render_html, self$raw(drop), title = self$caption(), footnote = self$footnote(),
                  !!!modifyList(self$options_kable, list(...)))
    },

    #' @description Render with [render_reactable()], wrapped in the nightowl
    #'   card with caption and footnote.
    #' @param drop Columns to drop.
    #' @param ... Passed to [render_reactable()].
    reactable = function(drop = NULL, ...) {
      widget <- rlang::exec(render_reactable, self$raw(drop), !!!modifyList(self$options_reactable, list(...)))
      html_card(widget, title = self$caption(), footnote = self$footnote())
    },

    #' @description Print the raw summary.
    #' @param ... Ignored.
    print = function(...) {
      cat("--", self$caption() %||% glue::glue("Summary of {self$column}"), "--\n")
      print(self$raw())
      if (!is.null(self$footnote())) cat(self$footnote(), "\n")
      invisible(self)
    }
  ),
  private = list(
    test_cache = NULL
  )
)
