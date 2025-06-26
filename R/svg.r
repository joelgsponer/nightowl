# ===============================================================================
#' Create SVG string
#' @param g ggplot object
#' @export
render_svg <- function(g,
                       height = 8,
                       width = 8,
                       element_width = "75vw",
                       element_height = "75vh",
                       scaling = 1,
                       add_download_button = T,
                       standalone = F,
                       bg = "transparent",
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
      nightowl_progress_step("Opening SVG device")
      svg <- svglite::svgstring(
        height = height, width = width, scaling = scaling,
        web_fonts = "https://fonts.googleapis.com/css2?family=Lato:wght@100;300;400;700;900&display=swap",
        standalone = standalone,
        bg = bg,
        ...
      )
      if (inherits(g, "call")) {
        # Security fix: prevent arbitrary code execution
        throw_security_error(
          "Call objects are not supported for security reasons",
          operation = "render_svg"
        )
      } else {
        print(g)
      }
      svg <- nightowl_regex_replace_element_parameter(svg(), "width", element_width) %>%
        nightowl_regex_replace_element_parameter("height", element_height) %>%
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
        nightowl_progress_step("Closing device")
        dev.off()
      }
    }
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
add_download_button <- function(x) {
  shiny::div(
    AceOfSpades::useAceOfSpadesJS(),
    htmltools::HTML('
      <script>
        function nightowl_download_svg(e) {
          var svg = e.parentElement.querySelector("svg");

          var svgXml = (new XMLSerializer).serializeToString(svg);
          var blob = new Blob([svgXml], {type: "image/svg+xml"});
          var url = URL.createObjectURL(blob);

          var link = document.createElement("a");
          link.href = url;
          link.download = "image.svg";
          link.style.display = "none";
          document.body.appendChild(link);

          // your code goes here
          link.click();
          document.body.removeChild(link);
        };
      </script>
    '),
    htmltools::HTML("
      <div
       class = 'nightowl-svg-download-button'
       onclick='nightowl_download_svg(this)'
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
  x <- unclass(x)
  nightowl_new_vctr(x, class = "NightowlPlots")
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
asJSON.NightowlPlots <- function(x) {
  purrr::map(x, ~ jsonlite:::asJSON(.x$html()))
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
  purrr::map(x, ~ .x$plot)
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
as.character.NightowlPlots <- function(x) {
  res <- purrr::map_chr(x, function(.x) {
    .x$html() %>%
      as.character()
  })
  return(res)
}
# =================================================
