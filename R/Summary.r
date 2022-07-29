# =================================================
#' R6 Class
#' @description
#' @detail
#' @export
Summary <- R6::R6Class("Summary",
  public = list(
    column = NULL,
    group_by = NULL,
    wrap_header = TRUE,
    .range = NULL,
    options_summarise = list(),
    options_reactables = list(defaultPageSize = 30),
    options_kable = list(),
    options_test = list(),
    initialize = function(.data, column, group_by = NULL, method = NULL, labels = NULL, debug = F, ...) {
      if (debug) browser()
      purrr::imap(list(...), function(.x, .y) {
        if (.y %in% names(self)) {
          self[[.y]] <- .x
        }
      })
      self$column <- column
      self$group_by <- group_by
      self$set_data(.data)
      self$set_labels(labels)
      self$set_type()
      self$set_method(method)
      if(!is.null(self$method)){
        self$set_calculations(self$method(self)$calculations)
        self$set_parameters(self$method(self)$parameters)
      }
      invisible(self)
    },
    # Variables --------------------------------------------------
    get_variables = function() {
      list(
        column = self$column,
        group_by = self$group_by
      )
    },
    check_variables = function() {
      vars <- unlist(unname(self$get_variables()))
      if (!all(vars %in% names(self$data))) {
        missing <- vars[!vars %in% names(self$data)]
        msg <- glue::glue("`{missing}` not present in data")
        rlang::abort(msg)
      }
    },
    # Data ---------------------------------------------------------------------
    data = NULL,
    set_data = function(data = self$data) {
      if (!is.null(data)) {
        self$data <- data
      } else {
        data <- self$data
      }
      if (is.null(self$data)) rlang::abort("No data provided - use `set_data` method to update")
      self$check_variables()
      if (is.null(self$group_by)) self$group_by <- waRRior::get_groups(data)
      data <- data %>%
        dplyr::ungroup() %>%
        dplyr::select_at(c(unname(unlist(self$get_variables())))) %>%
        dplyr::mutate_if(is.character, factor) %>%
        dplyr::mutate_if(is.factor, forcats::fct_explicit_na) %>%
        dplyr::group_by_at(self$group_by)
      self$data <- data
      invisible(self)
    },
    check_data = function() {
      if (is.null(self$data)) rlang::abort("No data provided - use `set_data` method to update")
    },
    # Labels ------------------------------------------------------------------
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
    # Type ---------------------------------------------------------------------
    type = NULL,
    set_type = function(.data) {
      self$type <- class(self$data[[self$column]])
    },
    # Method -------------------------------------------------------------------
    calculations = NULL,
    set_calculations = function(calculations) {
      self$calculations <- calculations
    },
    add_calculation = function(calculation, parameters = NULL) {
      self$calculations <- append(self$calculations, calculation)
      self$parameters <- append(self$parameters, parameters)
    },
    parameters = NULL,
    set_parameters = function(parameters) {
      if (rlang::is_expression(parameters)) {
        parameters <- eval(parameters)
      }
      self$parameters <- parameters
    },
    method = NULL,
    set_method = function(summarise) {
      if (is.null(summarise) && is.null(self$calculations)) {
        if (is.numeric(self$data[[self$column]])) {
          self$method <- memoise::memoise(nightowl::summarise_numeric)
        } else {
          self$method <- memoise::memoise(nightowl::summarise_categorical)
        }
      } else {
        self$method <- memoise::memoise(summarise)
      }
    },
    # Annotation --------------------------------------------------------------
    keep_y = TRUE,
    drop_variable = function(x) {
      if (!self$keep_y) {
        dplyr::select(x, -Variable)
      } else {
        x
      }
    },
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
    # Arrange ------------------------------------------------------------------
    arrange_by = NULL,
    arrange = function(x) {
      if (!is.null(self$arrange_by)) {
        dplyr::arrange(x, !!rlang::sym(self$arrange_by))
      } else {
        x
      }
    },
    # Test ---------------------------------------------------------------------
    test = NULL,
    add_test = TRUE,
    calc_test = function() {
      if (is.null(self$group_by)) {
        self$add_test <- FALSE
      }
      if (self$add_test) {
        self$test <- do.call(nightowl::Test$new, c(list(.data = self$data, y = self$column), self$options_test))
      } else {
        self$test <- NULL
      }
      invisible(self)
    },
    # Raw ----------------------------------------------------------------------
    unnest = TRUE,
    name_for_column = "Variable",
    names_sep = ".",
    raw = function(drop = NULL) {
      .data <- self$data
      res <- nightowl::summarise(
        data = self$data,
        column = self$column,
        calculations = self$calculations,
        parameters = self$parameters,
        unnest = self$unnest,
        name_for_column = self$name_for_column,
        names_sep =  self$names_sep)
      res <- dplyr::select(res, Variable, tidyselect::everything())
      res <- self$drop_variable(res)
      res <- self$arrange(res)
      if (!is.null(drop)) {
        res <- waRRior::drop_columns(res, drop)
      }
      return(res)
    },
    # Kabel ------------------------------------------------------------------
    kable = function(drop = NULL) {
      do.call(nightowl::render_kable, c(list(.tbl = self$raw(drop = drop), caption = self$caption(), footnote = self$footnote()), self$options_kable))
    },
    # HTML ---------------------------------------------------------------------
    html = function(drop = NULL) {
      shiny::HTML(self$kable(drop = drop))
    },
    # Reactables ---------------------------------------------------------------
    reactable = function(drop = NULL, ...) {
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
        do.call(nightowl::render_reactable, c(list(.tbl = self$raw(drop = drop)), .options)),
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
