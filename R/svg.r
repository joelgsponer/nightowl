# ===============================================================================
#' Create SVG string
#' @param g ggplot object
#' @export
render_svg <- function(g, height = 8, width = 8, scaling = 1) {
  fix_font <- function(html, param = "font-family", value = "Lato", old_value = "[^;]*") {
    str <- as.character(html)
    pattern <- glue::glue("{param}: {old_value}")
    print(pattern)
    replace <- glue::glue("{param}: {value}")
    new_str <- stringr::str_replace_all(str, pattern, replace)
    return(shiny::HTML(new_str))
  }
  tryCatch(
    {
      svg <- svglite::svgstring(
        height = height, width = width, scaling = scaling,
        web_fonts = "https://fonts.googleapis.com/css2?family=Lato:wght@100;300;400;700;900&display=swap"
      )
      print(g)
      svg <- waRRior::regex_replace_element_parameter(svg(), "width", "100%") %>%
        waRRior::regex_replace_element_parameter("height", "100%") %>%
        fix_font() %>%
        htmltools::browsable()
      try(dev.off())
      return(svg)
    },
    error = function(e) {
      try(dev.off())
      stop(e)
    }
  )
}
# ===============================================================================
#' peek
#' @export
peek <- function(g, ...) {
  nightowl::render_svg(g, ...) %>%
    htmltools::browsable()
}

# ===============================================================================
# =================================================
