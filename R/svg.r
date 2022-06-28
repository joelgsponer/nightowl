# ===============================================================================
#' Create SVG string
#' @param g ggplot object
#' @export
render_svg <- function(g,
                       height = 8,
                       width = 8,
                       scaling = 1,
                       add_download_button = T,
                       standalone = F,
                       font_family = "Lato, sans-serif",
                       ...) {
  fix_font <- function(html, param = "font-family", value = font_family, old_value = "[^;]*") {
    str <- as.character(html)
    pattern <- glue::glue("{param}: {old_value}")
    replace <- glue::glue("{param}: {value}")
    new_str <- stringr::str_replace_all(str, pattern, replace)
    return(htmltools::HTML(new_str))
  }
  n_dev <- length(dev.list())
  tryCatch(
    {
      cli::cli_progress_step("Opening SVG device")
      svg <- svglite::svgstring(
        height = height, width = width, scaling = scaling,
        web_fonts = "https://fonts.googleapis.com/css2?family=Lato:wght@100;300;400;700;900&display=swap",
        standalone = F,
        ...
      )
      print(g)
      svg <- waRRior::regex_replace_element_parameter(svg(), "width", "100%") %>%
        waRRior::regex_replace_element_parameter("height", "100%") %>%
        fix_font()
      if (add_download_button) {
        svg <- nightowl::add_download_button(svg)
      }
      svg <- as.character(svg) %>%
        htmltools::HTML()
      svg <- htmltools::browsable(svg)
      return(svg)
    },
    error = function(e) {
      stop(e)
    },
    finally = {
      while (length(dev.list()) > n_dev) {
        cli::cli_progress_step("Closing device")
        dev.off()
      }
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
    htmltools::HTML("
      <div
       class = 'nightowl-svg-download-button'
       onclick='AOS_download_svg(this)'
       style = 'text-align:right; font-weight:900; font-family:sans-serif;cursor:pointer;'
      >&#10515; Save Plot</div>
    "),
    x
  )
}
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
new_NightowlPlots <- function(...) {
  x <- list(...)
  vctrs::new_vctr(x, class = "NightowlPlots")
}
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
is_NightowlPlots <- function(x) {
  inherits(x, "NightowlPlots") || inherits(x, "NghtwlPl")
}
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
format.NightowlPlots <- function(x) {
  purrr::map(x, ~ .x$format())
}
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
as_html.NightowlPlots <- function(x) {
  purrr::map(x, ~ .x$html())
}
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
as_ggplot <- function(x) {
  UseMethod("as_ggplot")
}
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
as_ggplot.NightowlPlots <- function(x) {
  purrr::map(x, ~ .x$ggplot())
}
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
print.NightowlPlots <- function(x, browser = T) {
  purrr::map(x, ~ .x$print())
}
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
vec_ptype_abbr.NightowlPlots <- function(x) {
  "NghtwlPl"
}
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
as.character.NightowlPlots <- function(x) {
  purrr::map_chr(x, ~ .x$as.character())
}
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
vec_ptype2.NightowlPlots.NightowlPlots <- function(x, y, ...) {
  x
}
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
vec_cast.NightowlPlots.NightowlPlots <- function(x, to, ...) {
  x
}
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
as_R6 <- function(x) {
  UseMethod("as_R6")
}
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
as_R6.NightowlPlots <- function(x) {
  purrr::map(x, ~.x)
}
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
width <- function(x) {
  UseMethod("width")
}
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
width.NightowlPlots <- function(x) {
  purrr::map_dbl(x, ~ .x$get_width()) %>%
    max(na.rm = T)
}
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
height <- function(x) {
  UseMethod("height")
}
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
height.NightowlPlots <- function(x) {
  purrr::map_dbl(x, ~ .x$get_height()) %>%
    max(na.rm = T)
}
# =================================================
