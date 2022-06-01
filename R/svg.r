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
      # class(svg) <- c("nightowl_svg", class(svg))
      svg <- as.character(svg) %>%
       htmltools::HTML()
      svg <- vctrs::new_vctr(svg, class = "nightowl_svg")
      attributes(svg)$g <- g
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
  inherits(x, "nightowl_svg") || inherits(x, "nghtwl_s")
}
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
as_html <- function(x) {
  UseMethod("as_html")
}
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
as_html.nightowl_svg <- function(x) {
  vctrs::vec_data(x) %>%
    as.character() %>%
    htmltools::HTML()
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
as_ggplot.nightowl_svg <- function(x) {
  attributes(x)$g
}
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
print.nightowl_svg <- function(x, browser = T) {
  if (browser) {
    print(htmltools::browsable(nightowl::as_html(x)))
  } else {
    print(nightowl::as_html(x))
  }
}
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
format.nightowl_svg <- function(x, ...) {
  x
}
#=================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
vec_ptype_abbr.nightowl_svg <- function(x) {
  "nghtwl_svg"
}
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
as.character.nightowl_svg <- function(x) {
  nightowl::as_html(x) %>%
    as.character()
}
#=================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
vec_ptype2.nightowl_svg.nightowl_svg <- function(x, y, ...) {
  x
}
#=================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
vec_cast.nightowl_svg.nightowl_svg <- function(x, to, ...) {
  x
}
