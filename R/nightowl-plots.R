#' A vector of plots
#'
#' @description
#' `NightowlPlots` is a [vctrs] vector whose elements are [Plot] objects. It
#' behaves like any other column: it can live in a tibble, survive
#' `dplyr::mutate()`, `filter()` and `bind_rows()`, and be concatenated with
#' `c()`. [render_reactable()] and [render_kable()] recognise such columns and
#' render each element as an inline SVG.
#'
#' @param ... [Plot] objects, or a single list of them.
#' @param x An object.
#' @return `new_NightowlPlots()` returns a `NightowlPlots` vector.
#'   `is_NightowlPlots()` returns `TRUE` or `FALSE`. `as_ggplot()` returns a
#'   list of ggplot objects. `as_html()` returns a list of browsable HTML
#'   blocks.
#' @examples
#' gg <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
#' plots <- new_NightowlPlots(Plot$new(gg, svg = list(width = 2, height = 1)))
#' tibble::tibble(car = "all", trend = plots)
#' @name NightowlPlots
#' @aliases NightowlPlots
NULL

#' @rdname NightowlPlots
#' @export
new_NightowlPlots <- function(...) {
  x <- list(...)
  if (length(x) == 1 && is.list(x[[1]]) && !is_Plot(x[[1]])) {
    x <- x[[1]]
  }
  if (inherits(x, "NightowlPlots")) {
    return(x)
  }
  ok <- vapply(x, is_Plot, logical(1))
  if (!all(ok)) {
    cli::cli_abort("All elements must be {.cls Plot} objects; {cli::qty(sum(!ok))}element{?s} {which(!ok)} {?is/are} not.")
  }
  vctrs::new_vctr(unname(as.list(x)), class = "NightowlPlots")
}

#' @rdname NightowlPlots
#' @export
is_NightowlPlots <- function(x) {
  inherits(x, "NightowlPlots")
}

#' @rdname NightowlPlots
#' @export
as_ggplot <- function(x) {
  UseMethod("as_ggplot")
}

#' @export
as_ggplot.NightowlPlots <- function(x) {
  lapply(vctrs::vec_data(x), function(p) p$plot)
}

#' @export
as_ggplot.Plot <- function(x) {
  x$plot
}

#' @rdname NightowlPlots
#' @export
as_html <- function(x) {
  UseMethod("as_html")
}

#' @export
as_html.NightowlPlots <- function(x) {
  lapply(vctrs::vec_data(x), function(p) p$html())
}

#' @export
as_html.Plot <- function(x) {
  x$html()
}

#' @export
format.NightowlPlots <- function(x, ...) {
  vapply(vctrs::vec_data(x), function(p) p$format(), character(1))
}

#' @export
as.character.NightowlPlots <- function(x, ...) {
  vapply(vctrs::vec_data(x), function(p) as.character(p$svg()), character(1))
}

#' @export
print.NightowlPlots <- function(x, ...) {
  cat(glue::glue("<NightowlPlots[{length(x)}]>"), "\n")
  if (length(x) > 0) cat(format(x), sep = "\n")
  invisible(x)
}

#' @importFrom vctrs vec_ptype_abbr
#' @export
vec_ptype_abbr.NightowlPlots <- function(x, ...) {
  "NghtwlPl"
}

#' @importFrom vctrs vec_ptype2
#' @export
vec_ptype2.NightowlPlots.NightowlPlots <- function(x, y, ...) {
  new_NightowlPlots()
}

#' @importFrom vctrs vec_cast
#' @export
vec_cast.NightowlPlots.NightowlPlots <- function(x, to, ...) {
  x
}

#' Largest width/height (px) of the plots in a vector
#' @noRd
plots_width <- function(x) {
  max(vapply(vctrs::vec_data(x), function(p) p$width(), numeric(1)), na.rm = TRUE)
}

plots_height <- function(x) {
  max(vapply(vctrs::vec_data(x), function(p) p$height(), numeric(1)), na.rm = TRUE)
}
