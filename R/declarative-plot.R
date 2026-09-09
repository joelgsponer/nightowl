#' A plot declared as data
#'
#' @description
#' `DeclarativePlot` builds a ggplot from a specification: a data frame, a
#' mapping of aesthetics to column names, and an ordered list of layers, plus
#' optional transforms, scales, facets, axis settings, colours, theme and
#' annotation. The same specification can be stored in a YAML style file and
#' applied with [styled_plot()]. The result is a [Plot], so it renders to SVG
#' and can sit in a table column.
#'
#' The build order is fixed: select the mapped columns, apply `transform`,
#' create the ggplot, add `layers`, `scales`, `facets`, `axis`, `colours`,
#' `theming`, `annotation`. Method captions (summary functions, zooms, hidden
#' legends) are collected and written to the plot caption so a figure
#' documents itself.
#'
#' @examples
#' p <- DeclarativePlot$new(
#'   data = ChickWeight,
#'   mapping = list(x = "Time", y = "weight", colour = "Diet", id = "Chick"),
#'   layers = list(
#'     list(type = "traces", alpha = 0.2),
#'     list(type = "summary", fun.data = "mean_se", geom = "line", linewidth = 1)
#'   ),
#'   svg = list(width = 6, height = 4)
#' )
#' class(p$plot)
#' @export
DeclarativePlot <- R6::R6Class(
  "DeclarativePlot",
  inherit = Plot,
  public = list(
    #' @field data The selected and transformed data.
    data = NULL,
    #' @field mapping Named list of column names, including the nightowl-only
    #'   keys `id`, `facet_row`, `facet_col`.
    mapping = NULL,
    #' @field layers List of layer specifications; each has a `type` (see
    #'   [nightowl_layers()]) and arguments for that layer verb.
    layers = NULL,
    #' @field transform Named list of functions (or their names) applied to
    #'   the column behind a mapping key, e.g. `list(x = "factor")`.
    transform = NULL,
    #' @field scales List of `list(scale = "ggplot2::scale_...", ...)`.
    scales = NULL,
    #' @field facets `list(type, row, column, scales, label_width)`.
    facets = NULL,
    #' @field axis `list(log_x, log_y, xlim, ylim, units_x, units_y)`.
    axis = NULL,
    #' @field colours `list(palette, max_levels)`.
    colours = NULL,
    #' @field theming `list(theme = "theme_nightowl", <element> = list(element = ..., ...))`.
    theming = NULL,
    #' @field annotation `list(title, subtitle, caption, xlab, ylab, legend_position,
    #'   axis_text_x_angle, wrap_x, wrap_y, wrap_title, wrap_legend)`.
    annotation = NULL,
    #' @field dodge Default dodge width for layers that do not set one.
    dodge = NULL,
    #' @field name Free text carried over from a style file.
    name = NULL,
    #' @field description Free text carried over from a style file.
    description = NULL,
    #' @field captions Provenance notes collected while building.
    captions = character(0),

    #' @description Build the plot.
    #' @param data A data frame.
    #' @param mapping Named list of column names.
    #' @param layers,transform,scales,facets,axis,colours,theming,annotation,dodge
    #'   See the fields.
    #' @param svg Rendering options, see [Plot].
    #' @param resize,class See [Plot].
    #' @param name,description See the fields.
    #' @param colors American spelling of `colours`; use one or the other.
    initialize = function(data, mapping, layers = list(), transform = NULL, scales = list(),
                          facets = NULL, axis = NULL, colours = NULL, theming = NULL,
                          annotation = NULL, dodge = 0.75, svg = list(), resize = TRUE,
                          class = NULL, name = NULL, description = NULL, colors = NULL) {
      check_data_frame(data)
      spec <- list(transform = transform, layers = layers, scales = scales, facets = facets,
                   svg = svg)
      validate_spec(purrr::compact(spec), mapping = mapping)
      self$mapping <- mapping
      self$layers <- layers
      self$transform <- transform
      self$scales <- scales
      self$axis <- axis
      self$colours <- colours %||% colors
      self$theming <- theming
      self$annotation <- annotation
      self$dodge <- dodge
      self$name <- name
      self$description <- description
      private$select_data(data)
      private$transform_data()
      private$prepare_facets(facets)
      g <- private$build()
      super$initialize(plot = g, svg = svg, type = name %||% "DeclarativePlot", resize = resize, class = class)
      invisible(self)
    }
  ),
  private = list(
    select_data = function(data) {
      layer_cols <- unlist(lapply(self$layers, function(l) unlist(l$mapping)))
      cols <- unique(c(unlist(self$mapping), layer_cols))
      check_columns(data, cols)
      data <- tibble::as_tibble(dplyr::ungroup(data))
      data <- dplyr::select(data, dplyr::all_of(cols))
      if (nrow(data) == 0) cli::cli_abort("{.arg data} has no rows.")
      self$data <- data
    },
    transform_data = function() {
      for (key in names(self$transform %||% list())) {
        var <- self$mapping[[key]]
        if (is.null(var)) next
        fn <- resolve_function(self$transform[[key]])
        self$data[[var]] <- fn(self$data[[var]])
      }
    },
    prepare_facets = function(facets) {
      facets <- facets %||% list()
      if (!is.null(self$mapping$facet_row)) facets$row <- self$mapping$facet_row
      if (!is.null(self$mapping$facet_col)) facets$column <- self$mapping$facet_col
      self$facets <- if (length(facets) > 0) facets else NULL
    },
    build = function() {
      g <- ggplot2::ggplot(self$data, aes_from_list(self$mapping, keep_null = FALSE))
      captions <- character(0)
      for (spec in self$layers) {
        fn <- resolve_layer(spec$type)
        args <- spec[setdiff(names(spec), "type")]
        args <- args[!vapply(args, is.null, logical(1)) | names(args) == "mapping"]
        nulled <- names(spec$mapping)[vapply(spec$mapping, is.null, logical(1))]
        if (any(nulled %in% c("x", "y"))) {
          # Dropping a positional aesthetic (e.g. y for a histogram): the layer
          # declares the merged mapping itself instead of inheriting.
          args$mapping <- modifyList(aesthetic_mapping(self$mapping), spec$mapping)
          args$inherit.aes <- FALSE
        }
        if ("dodge" %in% names(formals(fn)) && is.null(args$dodge)) args$dodge <- self$dodge
        if ("id" %in% names(formals(fn)) && is.null(args$id)) args$id <- self$mapping$id
        captions <- c(captions, layer_caption(spec))
        g <- rlang::exec(fn, g, !!!args)
      }
      g <- apply_scales(g, self$scales)
      g <- apply_facets(g, self$facets)
      step <- take_captions(apply_axis(g, self$axis, self$mapping))
      captions <- c(captions, step$captions)
      step <- take_captions(apply_colours(step$plot, self$data, self$mapping, self$colours))
      captions <- c(captions, step$captions)
      g <- apply_theme(step$plot, self$theming)
      self$captions <- unique(captions)
      apply_annotation(g, self$mapping, self$annotation, self$captions)
    }
  )
)

#' Provenance note for a layer specification
#' @noRd
layer_caption <- function(spec) {
  type <- if (is.character(spec$type)) spec$type else "custom"
  if (type == "summary") {
    method <- spec$fun.data %||% spec$fun %||% "mean_se"
    if (!is.character(method)) method <- "custom function"
    return(glue::glue("Summary: {method}"))
  }
  if (type == "smooth") {
    return(glue::glue("Smoothing: {spec$method %||% 'lm'}"))
  }
  character(0)
}

#' The aesthetic part of a mapping (drops nightowl-only keys)
#' @noRd
aesthetic_mapping <- function(mapping) {
  mapping[setdiff(names(mapping), c("id", "facet_row", "facet_col"))]
}
