# =================================================
#' @title
#' Convert to girafe object
#' @description
#'
#' @detail
#'
#' @param
#' @return
#' @export
ggplot_to_girafe <- function(ggobj,
                             width = 10,
                             height = 20) {
  ggiraph::girafe(
    ggobj = ggobj,
    width_svg = width,
    height_svg = height,
    options = list(
      ggiraph::opts_sizing(rescale = TRUE, width = 0.8),
      ggiraph::opts_tooltip(
        use_fill = TRUE,
        opacity = .8,
        css = "color:black;padding:2px;border-radius:2px;font-family:'Lato';margin-left:10px;",
      ),
      ggiraph::opts_hover(css = "fill:red;stroke:black;cursor:pointer;"),
      ggiraph::opts_hover_inv(css = "opacity:0.3;"),
      ggiraph::opts_zoom(max = 5)
    )
  )
}
# =================================================
