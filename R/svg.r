# ===============================================================================
#' Create SVG string
#' @param g ggplot object
#' @export
render_svg <- function(g, ...) {
  tryCatch(
    {
      svg <- svglite::svgstring(web_fonts = "https://fonts.googleapis.com/css2?family=Lato:wght@100;300;400;700;900&display=swap", ...)
      print(g)
      svg <- waRRior::regex_replace_element_parameter(svg(), "width", "100%") %>%
        waRRior::regex_replace_element_parameter("height", "100%")
      try(dev.off())
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
