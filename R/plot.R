# ===============================================================================
#' Plot
#' @importFrom ggplot2 aes mean_cl_boot mean_cl_normal mean_se mean_sdl label_both
#' @export
plot <- function(DATA,
                 mapping = list(
                   x = NULL,
                   y = NULL,
                   group = NULL,
                   fill = NULL,
                   color = NULL,
                   size = NULL,
                   shape = NULL,
                   lty = NULL,
                   id = NULL
                 ),
                 processing = NULL,
                 transform = NULL,
                 layers = list(),
                 annotation = NULL,
                 axis = NULL,
                 colors = NULL,
                 theming = NULL,
                 facets = NULL,
                 svg = NULL,
                 dodge = 1,
                 ...) {
  #*******************************************************************************
  # Drop columns that are not needed
  cols <- c(
    unlist(unname(mapping)),
    purrr::map(layers, ~ unlist(unname(.x$mapping))) %>% unlist()
  ) %>%
    unique()

  if (inherits(DATA, "data.frame")) {
    DATA <- tibble::as_tibble(DATA)
  }
  DATA <- DATA %>%
    dplyr::select_at(cols)
  if (any(dim(DATA) == 0)) rlang::abort("No data, check mapping")
  #*******************************************************************************
  # Transformations
  if (!is.null(transform)) {
    transform_data <- transform$data
    transform <- transform[waRRior::pop(names(transform), "data")]
    purrr::iwalk(transform, function(.f, .var) {
      if (is.character(.f)) .f <- waRRior::getfun(.f)
      .var <- mapping[[.var]]
      if (!is.null(.var)) {
        DATA <<- DATA %>%
          dplyr::mutate(!!rlang::sym(.var) := .f(!!rlang::sym(.var)))
      }
    })
    if (!is.null(transform_data)) {
      if (is.character(transform_data)) .f <- waRRior::getfun(transform_data)
      res_tranform_data <- .f(DATA, mapping)
      DATA <- res_tranform_data$data
      mapping <- res_tranform_data$mapping
    }
  }
  #*******************************************************************************
  # Prepare facets (if any)
  if (is.null(facets) &&
    (!is.null(mapping$facet_row) ||
      !is.null(mapping$facet_col))
  ) {
    facets <- list()
  }
  if (!is.null(mapping$facet_col)) facets$column <- mapping$facet_col
  if (!is.null(mapping$facet_row)) facets$row <- mapping$facet_row
  mapping <- mapping[waRRior::pop(names(mapping), c("facet_row", "facet_col"))]
  #*******************************************************************************
  # Setup Plot
  .aes <- nightowl:::aes(mapping)
  g <- ggplot2::ggplot(DATA, .aes)
  #*******************************************************************************
  # Add type to layers
  # layers <- purrr::imap(layers, function(.x, .y) {
  #   c(list(type = .y), .x)
  # })
  #*******************************************************************************
  # Add layers
  g <- purrr::reduce(layers, function(.x, .y) {
    .y$g <- .x
    .y <- rev(.y)
    if (is.null(.y$dodge)) .y$dodge <- dodge
    type <- .y$type
    .y$type <- NULL
    .y <- purrr::compact(.y)
    thiscall <- glue::glue("do.call(nightowl::{type}, .y)")
    eval(parse(text = thiscall))
  }, .init = g)
  #*******************************************************************************
  # Facets
  g <- do.call(nightowl::facets, c(list(g = g), facets))
  #*******************************************************************************
  # Axis
  g <- do.call(nightowl::axis, c(list(g = g), axis))
  # #*******************************************************************************
  # Colors and theming
  g <- do.call(nightowl::colors, c(list(g = g, DATA = DATA, mapping = mapping), colors))
  # # Add Theme
  g <- do.call(nightowl::theme, c(list(g = g), theming))
  # #*******************************************************************************
  # # Annotation
  g <- do.call(nightowl::annotation, c(list(g = g), mapping, annotation, axis))
  #*******************************************************************************
  # Create SVG
  if (!is.null(svg)) {
    g <- do.call(nightowl::render_svg, c(list(g = g), svg))
  }
  # Finishing up
  return(g)
}
# ===============================================================================
#' R6 Class
#'
#' @description
#'
#' @detail
#'
Plot <- R6::R6Class("Plot",
  public = list(
    data = NULL,
    mapping = list(
      x = NULL,
      y = NULL,
      group = NULL,
      fill = NULL,
      color = NULL,
      size = NULL,
      shape = NULL,
      lty = NULL,
      id = NULL
    ),
    processing = NULL,
    transform = NULL,
    layers = list(),
    scales = list(),
    annotation = NULL,
    axis = NULL,
    colors = NULL,
    theming = NULL,
    facets = NULL,
    svg = NULL,
    initialize = function(...) {
      args <- list(...)
      purrr::imap(list(...), function(.x, .y) {
        if (.y %in% names(self)) {
          self[[.y]] <- .x
        }
      })
      self$select_data()
      self$transform_data()
      self$prepare_facets()
      return(self)
    },
    # select data
    select_data = function() {
      cols <- c(
        unlist(unname(self$mapping)),
        purrr::map(self$layers, ~ unlist(unname(.x$mapping))) %>% unlist()
      ) %>%
        unique()
      if (inherits(self$data, "data.frame")) {
        .data <- tibble::as_tibble(self$data)
      }
      self$data <- .data %>%
        dplyr::select_at(cols)
      if (any(dim(self$data) == 0)) rlang::abort("No data, check mapping")
    },
    # tranform data (this also potentionally updates the mapping if for example frequencies are calculated)
    transform_data = function() {
      transform <- self$transform
      mapping <- self$mapping
      .data <- self$data
      if (!is.null(transform)) {
        transform_data <- transform$data
        transform <- transform[waRRior::pop(names(transform), "data")]
        purrr::iwalk(transform, function(.f, .var) {
          if (is.character(.f)) .f <- waRRior::getfun(.f)
          .var <- mapping[[.var]]
          if (!is.null(.var)) {
            .data <<- .data %>%
              dplyr::mutate(!!rlang::sym(.var) := .f(!!rlang::sym(.var)))
          }
        })
        if (!is.null(transform_data)) {
          if (is.character(transform_data)) .f <- waRRior::getfun(transform_data)
          res_tranform_data <- .f(.data, mapping)
          self$data <- res_tranform_data$data
          self$mapping <- res_tranform_data$mapping
        }
      }
    },
    # prepare facets
    prepare_facets = function() {
      facets <- self$facets
      mapping <- self$mapping
      # Prepare facets (if any)
      if (is.null(facets) &&
        (!is.null(mapping$facet_row) ||
          !is.null(mapping$facet_col))
      ) {
        facets <- list()
      }
      if (!is.null(mapping$facet_col)) facets$column <- mapping$facet_col
      if (!is.null(mapping$facet_row)) facets$row <- mapping$facet_row
      self$mapping <- mapping[waRRior::pop(names(mapping), c("facet_row", "facet_col"))]
      self$facets <- facets
    },
    # print
    print = function() {
      cli::cli_h3("Data:")
      cli::cli_li("{self$data}")
      cli::cli_h3("Mapping:")
      cli::cli_li("{self$mapping}")
      cli::cli_h3("Facets:")
      cli::cli_li("{self$facets}")
    }
  )
)
# ===============================================================================
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
is_Plot <- function(x) {
  all(
    inherits(x, "Plot"),
    inherits(x, "R6")
  )
}
# =================================================
