#' Nightowl Theme for ggplot2
#'
#' @description
#' A clean, professional theme for ggplot2 plots, replacing picasso::theme_picasso()
#'
#' @param base_size Base font size (default: 11)
#' @param base_family Base font family (default: "")
#' @param base_line_size Base size for line elements (default: base_size/22)
#' @param base_rect_size Base size for rect elements (default: base_size/22)
#' @return A ggplot2 theme object
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   theme_nightowl()
theme_nightowl <- function(base_size = 11, 
                          base_family = "",
                          base_line_size = base_size / 22,
                          base_rect_size = base_size / 22) {
  
  # Start with theme_minimal as base
  ggplot2::theme_minimal(
    base_size = base_size, 
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) +
  ggplot2::theme(
    # Grid lines
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_line(color = "grey90", size = 0.25),
    
    # Axes
    axis.ticks = ggplot2::element_line(color = "grey20", size = 0.25),
    axis.text = ggplot2::element_text(color = "grey20", size = base_size * 0.8),
    axis.title = ggplot2::element_text(color = "grey20", size = base_size * 0.9),
    axis.line = ggplot2::element_line(color = "grey20", size = 0.25),
    
    # Legend
    legend.position = "top",
    legend.title = ggplot2::element_text(size = base_size * 0.9),
    legend.text = ggplot2::element_text(size = base_size * 0.8),
    legend.key.size = ggplot2::unit(1.2, "lines"),
    
    # Plot title and subtitle
    plot.title = ggplot2::element_text(
      size = base_size * 1.2,
      hjust = 0,
      vjust = 1,
      margin = ggplot2::margin(b = base_size / 2)
    ),
    plot.subtitle = ggplot2::element_text(
      size = base_size * 0.9,
      hjust = 0,
      vjust = 1,
      margin = ggplot2::margin(b = base_size / 2)
    ),
    
    # Facets
    strip.background = ggplot2::element_rect(fill = "grey95", color = NA),
    strip.text = ggplot2::element_text(
      color = "grey20",
      size = base_size * 0.8,
      margin = ggplot2::margin(base_size / 4, base_size / 4, base_size / 4, base_size / 4)
    ),
    
    # Overall plot
    plot.background = ggplot2::element_rect(color = NA),
    panel.background = ggplot2::element_rect(color = NA),
    plot.margin = ggplot2::margin(base_size / 2, base_size / 2, base_size / 2, base_size / 2)
  )
}

#' Nightowl Void Theme
#'
#' @description
#' A completely minimal theme with no elements, replacing picasso::theme_void()
#'
#' @return A ggplot2 theme object
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   theme_nightowl_void()
theme_nightowl_void <- function() {
  ggplot2::theme_void()
}