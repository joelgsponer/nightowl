# ===============================================================================
#' Create SVG string
#' @param g ggplot object
#' @export
render_svg <- function(g, height = 8, width = 8, scaling = 1, add_download_button = T, standalone = F, ...) {
  fix_font <- function(html, param = "font-family", value = "Lato", old_value = "[^;]*") {
    str <- as.character(html)
    pattern <- glue::glue("{param}: {old_value}")
    replace <- glue::glue("{param}: {value}")
    new_str <- stringr::str_replace_all(str, pattern, replace)
    return(shiny::HTML(new_str))
  }
  tryCatch(
    {
      svg <- svglite::svgstring(
        height = height, width = width, scaling = scaling,
        web_fonts = "https://fonts.googleapis.com/css2?family=Lato:wght@100;300;400;700;900&display=swap",
        standalone = F,
        ...
      )
      print(g)
      svg <- waRRior::regex_replace_element_parameter(svg(), "width", "100%") %>%
        waRRior::regex_replace_element_parameter("height", "100%") %>%
        fix_font() %>%
        htmltools::browsable()
      try(dev.off())
      if (add_download_button) {
        svg <- nightowl::add_download_button(svg)
      }
      class(svg) <- c("nightowl_svg", class(svg))
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
# =================================================
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
add_download_button <- function(x) {
  shiny::div(
    AceOfSpades::useAceOfSpadesJS(),
    shiny::HTML("
      <div
       class = 'nightowl-svg-download-button'
       onclick='AOS_download_svg(this)'
       style = 'text-align:right; font-weight:900; font-family:sans-serif;cursor:pointer;'
      >&#10515; Save Plot</div>
    "),
    x
  ) %>%
    htmltools::browsable()
}
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
new_svg <- function(x) {
  vctrs::new_vctr(x, class = "nightowl_svg")
}
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
is_nightowl_svg <- function(x) {
  inherits(x, "nightowl_svg")
}
# =================================================
#' @export
vec_ptype2.nightowl_svg.nightowl_svg <- function(x, y, ...) {
  x
}
# =================================================
