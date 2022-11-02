# =================================================
#' @title
#' Convert to girafe object
#' @export
ggplot_to_girafe <- function(ggobj,
                             width = 10,
                             height = 20,
                             color = "black") {
  ggiraph::girafe(
    ggobj = ggobj,
    width_svg = width,
    height_svg = height,
    options = list(
      ggiraph::opts_sizing(rescale = TRUE, width = 0.8),
      ggiraph::opts_tooltip(
        use_fill = TRUE,
        opacity = .8,
        css = glue::glue("color:{color};padding:2px;border-radius:2px;font-family:'Lato';margin-left:10px;"),
      ),
      ggiraph::opts_hover(css = "fill:red;stroke:black;cursor:pointer;"),
      ggiraph::opts_hover_inv(css = "opacity:0.3;"),
      ggiraph::opts_zoom(max = 5)
    )
  )
}
# =================================================
#' Use lowRider
#' Includes required javascript and css files.
#' @return
#' @export
useNightowl <- function() {
  scripts <- c("d3-bundle.min.js", "d3-lasso.min.js", "flatbush.min.js", "ggiraphjs.min.js", "save-svg-as-png.min.js")
  htmltools::tagList(
    htmltools::htmlDependency(
      name = "ggiraph-assets", version = packageVersion("nightowl"),
      package = "nightowl",
      src = "assets",
      script = scripts
    )
  )
}
# =================================================
