# ===============================================================================
#' Create SVG string
#' @param g ggplot object
#' @export
render_svg <- function(g, ...) {
  tryCatch(
    {
      svg <- svglite::svgstring(...)
      print(g)
      svg <- waRRior::regex_replace_element_parameter(svg(), "width", "100%") %>%
        waRRior::regex_replace_element_parameter("height", "100%")
      dev.off()
      return(svg)
    },
    error = function(e) {
      dev.off()
      stop(e)
    }
  )
}
# ===============================================================================
#' peek
#' @export
peek <- function(g, ...) {
  nightowl::render_svg(g) %>%
    htmltools::browsable()
}
# ===============================================================================
