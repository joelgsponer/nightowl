#' A rendered plot
#'
#' @description
#' `Plot` wraps a ggplot object together with its SVG rendering options.
#' Rendering is memoised per object, so `$svg()`, `$html()` and the size
#' getters share one rendering. `Plot` objects are the elements of a
#' [NightowlPlots] vector and therefore what ends up inside table columns.
#'
#' @examples
#' p <- Plot$new(
#'   ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point(),
#'   svg = list(width = 4, height = 3)
#' )
#' p$width()
#' html <- p$html()
#' @export
Plot <- R6::R6Class(
  "Plot",
  public = list(
    #' @field plot The ggplot object.
    plot = NULL,
    #' @field type A short label used when the object is printed inside a
    #'   table column.
    type = "Plot",
    #' @field svg_options List of rendering options merged over the `svg`
    #'   package option: `width`, `height`, `scaling`, and any argument of
    #'   [render_svg()].
    svg_options = NULL,
    #' @field resize Whether the HTML wrapper lets the SVG scale with its
    #'   container (`TRUE`) or fixes it to its native pixel size.
    resize = TRUE,
    #' @field class Extra CSS classes for the HTML wrapper.
    class = NULL,

    #' @description Create a `Plot`.
    #' @param plot A ggplot object.
    #' @param svg List of rendering options, see the `svg_options` field.
    #' @param type Label, see the `type` field.
    #' @param resize See the `resize` field.
    #' @param class See the `class` field.
    initialize = function(plot, svg = list(), type = "Plot", resize = TRUE, class = NULL) {
      if (!inherits(plot, "ggplot") && !inherits(plot, "patchwork")) {
        cli::cli_abort("{.arg plot} must be a ggplot object, not {.obj_type_friendly {plot}}.")
      }
      if (!is.list(svg)) {
        cli::cli_abort("{.arg svg} must be a list of rendering options.")
      }
      check_string(type)
      self$plot <- plot
      self$svg_options <- modifyList(nightowl_option("svg"), svg)
      self$type <- type
      self$resize <- isTRUE(resize)
      self$class <- class
      private$renderer <- memoise::memoise(render_svg)
      invisible(self)
    },

    #' @description Render to SVG markup.
    #' @param ... Rendering options overriding `svg_options` for this call.
    #' @return An [htmltools::HTML()] string.
    svg = function(...) {
      opts <- modifyList(self$svg_options, list(...))
      if (!self$resize) {
        opts$element_width <- opts$element_width %||% paste0(self$width(), "px")
        opts$element_height <- opts$element_height %||% paste0(self$height(), "px")
      }
      rlang::exec(private$renderer, plot = self$plot, !!!opts)
    },

    #' @description Render to an HTML block with the nightowl dependency
    #'   attached.
    #' @param resize Overrides the `resize` field.
    #' @param ... Passed to `$svg()`.
    #' @return A browsable [htmltools::tag].
    html = function(resize = self$resize, ...) {
      style <- if (!resize) glue::glue("width:{self$width()}px;height:{self$height()}px;") else NULL
      tag <- htmltools::div(
        class = paste(c("nightowl", "nightowl-plot", if (!resize) "nightowl-plot--fixed", self$class), collapse = " "),
        style = style,
        self$svg(...)
      )
      htmltools::browsable(htmltools::attachDependencies(tag, nightowl_dependency()))
    },

    #' @description Width of the rendered SVG in CSS pixels (72 per inch).
    width = function() {
      self$svg_options$width * 72
    },

    #' @description Height of the rendered SVG in CSS pixels (72 per inch).
    height = function() {
      self$svg_options$height * 72
    },

    #' @description Print: opens the HTML rendering in the viewer or browser.
    #' @param ... Ignored.
    print = function(...) {
      print(self$html())
      invisible(self)
    },

    #' @description One-line description used inside tables.
    #' @param ... Ignored.
    format = function(...) {
      glue::glue("<{self$type}>")
    },

    #' @description The SVG markup as a plain string.
    #' @param ... Passed to `$svg()`.
    as.character = function(...) {
      as.character(self$svg(...))
    }
  ),
  private = list(
    renderer = NULL
  )
)

#' Is an object a `Plot`?
#'
#' @param x Any object.
#' @return `TRUE` or `FALSE`.
#' @examples
#' is_Plot(1)
#' @export
is_Plot <- function(x) {
  inherits(x, "Plot") && inherits(x, "R6")
}
