# #=================================================
# #' R6 Class
# #' @description
# #' @detail
# #' @export
Summary <- R6::R6Class("Summary",
  public = list(
    column = NULL,
    group_by = NULL,
    .mean = ggplot2::mean_cl_boot,
    wrap_header = TRUE,
    .range = NULL,
    options_summarise = list(),
    options_reactables = list(defaultPageSize = 30),
    options_kable = list(),
    options_test = list(),
    # Initalize
    initialize = function(.data, column, group_by = NULL, summarise = NULL, labels = NULL, ...) {
      self$column <- column
      self$group_by <- group_by
      self$set_data(.data)
      self$set_labels(labels)
      self$set_type()
      self$set_method(summarise)
      # Arguments
      purrr::imap(list(...), function(.x, .y) {
        if (.y %in% names(self)) {
          self[[.y]] <- .x
        }
      })
      self$set_hash()
      # Return
      invisible(self)
    },
    # Is dirty
    hash = NULL,
    hash_fields = c("data", "columm", "group_by", "summarise", "labels"),
    is_dirty = function(fields = self$hash_fields) {
      purrr::map(fields, function(.x) {
        !digest::digest(self[[.x]]) == self$hash[[.x]]
      }) %>%
        purrr::set_names(fields)
    },
    set_hash = function(fields = self$hash_fields) {
      self$hash <- purrr::map(fields, ~ digest::digest(self[[.x]])) %>%
        purrr::set_names(fields)
    },
    # Data
    data = NULL,
    set_data = function(.data) {
      # Grouping
      if (!is.null(self$group_by)) {
        .data <- dplyr::ungroup(.data) %>%
          dplyr::group_by_at(self$group_by)
      }
      self$group_by <- waRRior::get_groups(.data)
      .data <- .data %>%
        dplyr::select_at(c(self$column, self$group_by)) %>%
        dplyr::mutate_if(is.character, factor) %>%
        dplyr::mutate_if(is.factor, forcats::fct_explicit_na)
      self$data <- .data
    },
    # Labels
    labels = NULL,
    set_labels = function(labels) {
      if (!is.null(labels)) {
        .data <- self$data
        names(.data) <- nightowl::get_labels(names(.data), labels)
        self$data <- .data
        self$column <- nightowl::get_labels(self$column, labels)
        self$group_by <- nightowl::get_labels(self$group_by, labels)
      }
      self$labels <- labels
    },
    # Type
    type = NULL,
    set_type = function(.data) {
      self$type <- class(self$data[[self$column]])
    },
    # Method
    method = NULL,
    set_method = function(summarise) {
      if (is.null(summarise)) {
        if ("numeric" %in% self$type) {
          self$method <- nightowl::summarise_numeric
        } else {
          self$method <- nightowl::summarise_categorical
        }
      }
    },
    # Utils
    keep_y = TRUE,
    drop_variable = function(x) {
      if (!self$keep_y) {
        dplyr::select(x, -Variable)
      } else {
        x
      }
    },
    # Annotation
    add_caption = TRUE,
    caption = function() {
      if (self$add_caption) {
        glue::glue("Summary for {self$column}")
      } else {
        NULL
      }
    },
    add_footnote = TRUE,
    footnote = function() {
      if (self$add_footnote) {
        self$calc_test()$test$footnote
      } else {
        NULL
      }
    },
    # Test
    test = NULL,
    add_test = TRUE,
    calc_test = function() {
      if (self$add_test) {
        self$test <- do.call(nightowl::Test$new, c(list(.data = self$data, y = self$column), self$options_test))
      } else {
        self$test <- NULL
      }
      invisible(self)
    },
    # Arrange
    arrange_by = NULL,
    arrange = function(x) {
      if (!is.null(self$arrange_by)) {
        dplyr::arrange(x, !!rlang::sym(self$arrange_by))
      } else {
        x
      }
    },
    # Outputs
    raw = function() {
      .data <- self$data
      res <- self$method(self$data, self$column)
      res <- dplyr::select(res, Variable, tidyselect::everything())
      res <- self$drop_variable(res)
      res <- self$arrange(res)
      return(res)
    },
    kable = function() {
      do.call(nightowl::render_kable, c(list(.tbl = self$raw(), caption = self$caption(), footnote = self$footnote()), self$options_kable))
    },
    html = function() {
      shiny::HTML(self$kable())
    },
    reactable = function(...) {
      .args <- list(...)
      .options <- c(.args, self$options_reactables[!names(self$options_reactables) %in% names(.args)])
      res <- shiny::div(
        style = "
          font-family: 'Lato', sans-serif;
          display: flex;
          flex-direction: column;
          align-content: flex-start;
          align-items: center;
        ",
        shiny::h3(self$caption()),
        do.call(nightowl::render_reactable, c(list(.tbl = self$raw()), .options)),
        shiny::div(
          style = "font-size: 0.8em;",
          glue::glue(self$footnote())
        )
      ) %>%
        htmltools::browsable()
      return(res)
    }
  )
)
