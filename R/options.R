#' Package options
#'
#' nightowl keeps its tunable defaults in base R [options()] under the
#' `nightowl.` prefix. `nightowl_options()` reads or sets them without the
#' prefix; `nightowl_option()` reads a single value.
#'
#' Available options:
#'
#' * `palette`: name of the default discrete palette, see [nightowl_palettes()].
#' * `missing_colour`: colour used for the `"(Missing)"` level everywhere.
#' * `header_width`: width at which table headers are wrapped.
#' * `font_family`: CSS font stack used in rendered SVG and HTML.
#' * `download_button`: whether [render_svg()] adds a download button.
#' * `web_fonts`: CSS font import written into SVG output, or `NULL`.
#' * `svg`: list of `width`, `height` (inches) and `scaling` for SVG rendering.
#'
#' @param ... Either nothing (return all options), unnamed character
#'   strings (return those options as a list), or `name = value` pairs to set.
#' @param name A single option name without the `nightowl.` prefix.
#' @return `nightowl_options()` returns a named list of the requested options
#'   (all of them when called without arguments); when setting, it returns the
#'   previous values invisibly so they can be restored. `nightowl_option()`
#'   returns one value.
#' @examples
#' nightowl_option("palette")
#' old <- nightowl_options(palette = "dusk")
#' nightowl_option("palette")
#' options(old)
#' @export
nightowl_options <- function(...) {
  args <- list(...)
  defaults <- nightowl_default_options()
  if (length(args) == 0) {
    return(setNames(lapply(names(defaults), function(k) getOption(k)), strip_prefix(names(defaults))))
  }
  if (is.null(names(args))) {
    keys <- add_prefix(unlist(args))
    check_option_names(keys)
    return(setNames(lapply(keys, getOption), strip_prefix(keys)))
  }
  keys <- add_prefix(names(args))
  check_option_names(keys)
  invisible(options(setNames(args, keys)))
}

#' @rdname nightowl_options
#' @export
nightowl_option <- function(name) {
  key <- add_prefix(name)
  check_option_names(key)
  getOption(key, default = nightowl_default_options()[[key]])
}

nightowl_default_options <- function() {
  list(
    nightowl.palette = "owl",
    nightowl.missing_colour = "#B3B3B3",
    nightowl.header_width = 20,
    nightowl.font_family = "Lato, Helvetica, Arial, sans-serif",
    nightowl.download_button = TRUE,
    nightowl.web_fonts = "https://fonts.googleapis.com/css2?family=Lato:wght@400;700&display=swap",
    nightowl.svg = list(width = 8, height = 8, scaling = 1)
  )
}

add_prefix <- function(x) ifelse(startsWith(x, "nightowl."), x, paste0("nightowl.", x))
strip_prefix <- function(x) sub("^nightowl\\.", "", x)

check_option_names <- function(keys) {
  known <- names(nightowl_default_options())
  bad <- setdiff(keys, known)
  if (length(bad) > 0) {
    cli::cli_abort(c(
      "Unknown nightowl option{?s}: {.val {strip_prefix(bad)}}.",
      i = "Available options: {.val {strip_prefix(known)}}."
    ))
  }
  invisible(TRUE)
}
