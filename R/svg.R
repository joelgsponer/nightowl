#' Render a ggplot to an SVG string
#'
#' Renders with [svglite::svgstring()] and post-processes the markup so it
#' scales to its container, uses the nightowl font stack, and optionally
#' carries a download button. All nightowl plot output goes through here.
#'
#' @param plot A ggplot object.
#' @param width,height Device size in inches. These define the aspect ratio and
#'   the `viewBox`; the on-page size is controlled by CSS.
#' @param scaling Scaling factor for text and lines, see [svglite::svgstring()].
#' @param bg Background colour.
#' @param font_family CSS font stack written into the SVG.
#' @param web_fonts Passed to [svglite::svgstring()]; a URL of a CSS font
#'   import, or `NULL`.
#' @param download_button Add a "Save SVG" button. Requires the markup to be
#'   embedded in an HTML page with [nightowl_dependency()] attached, which the
#'   package renderers do.
#' @param element_width,element_height Values written to the root `width` and
#'   `height` attributes. Defaults let the SVG fill its container.
#' @param filename File name suggested by the download button.
#' @param ... Further arguments to [svglite::svgstring()].
#' @return An [htmltools::HTML()] string (class `html`). Use `as.character()`
#'   to get the plain markup.
#' @examples
#' p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
#' svg <- render_svg(p, width = 4, height = 3, download_button = FALSE)
#' substr(as.character(svg), 1, 40)
#' @export
render_svg <- function(plot,
                       width = nightowl_option("svg")$width,
                       height = nightowl_option("svg")$height,
                       scaling = nightowl_option("svg")$scaling,
                       bg = "transparent",
                       font_family = nightowl_option("font_family"),
                       web_fonts = nightowl_option("web_fonts"),
                       download_button = nightowl_option("download_button"),
                       element_width = "100%",
                       element_height = "100%",
                       filename = "plot.svg",
                       ...) {
  if (!inherits(plot, "ggplot")) {
    cli::cli_abort("{.arg plot} must be a ggplot object, not {.obj_type_friendly {plot}}.")
  }
  n_dev <- length(dev.list())
  on.exit(while (length(dev.list()) > n_dev) dev.off(), add = TRUE)
  string <- svglite::svgstring(
    width = width, height = height, scaling = scaling, bg = bg,
    standalone = FALSE, web_fonts = web_fonts, id = "nightowl", ...
  )
  print(plot)
  dev.off()
  svg <- string()
  svg <- set_svg_root_attr(svg, "width", element_width)
  svg <- set_svg_root_attr(svg, "height", element_height)
  if (!is.null(font_family)) {
    # style attributes are single-quoted; quoted family names must use double quotes
    font_family <- gsub("'", "\"", font_family, fixed = TRUE)
    svg <- stringr::str_replace_all(svg, "font-family: [^;]*;", paste0("font-family: ", font_family, ";"))
  }
  if (isTRUE(download_button)) {
    svg <- paste0(download_button_html(filename), svg)
  }
  htmltools::HTML(svg)
}

#' Set an attribute on the root `<svg>` element
#' @noRd
set_svg_root_attr <- function(svg, attr, value) {
  pattern <- paste0("(<svg\\b[^>]*?\\s)", attr, "='[^']*'")
  if (stringr::str_detect(svg, pattern)) {
    stringr::str_replace(svg, pattern, paste0("\\1", attr, "='", value, "'"))
  } else {
    stringr::str_replace(svg, "<svg\\b", paste0("<svg ", attr, "='", value, "'"))
  }
}

download_button_html <- function(filename = "plot.svg") {
  as.character(htmltools::tags$button(
    class = "nightowl-download",
    type = "button",
    onclick = glue::glue("nightowlDownloadSvg(this, '{filename}')"),
    htmltools::HTML("&#8615; SVG")
  ))
}

#' Read the viewBox of an SVG string
#' @noRd
svg_viewbox <- function(svg) {
  m <- stringr::str_match(as.character(svg), "viewBox='([^']+)'")[, 2]
  if (is.na(m)) {
    return(c(NA_real_, NA_real_, NA_real_, NA_real_))
  }
  as.numeric(strsplit(m, " ")[[1]])
}
