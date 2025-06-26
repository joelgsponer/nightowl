#' Nightowl Plot Theming Utilities
#'
#' Internal theming functions to replace external picasso dependency
#'
#' @name theming
NULL

#' Hide legend from ggplot
#'
#' @return ggplot2 theme element
#' @export
hide_legend <- function() {
  ggplot2::theme(legend.position = "none")
}

#' Hide plot title (compatible with picasso::hide_title)
#'
#' @return ggplot2 theme element
#' @export
hide_title <- function() {
  ggplot2::theme(plot.title = ggplot2::element_blank())
}

#' Hide x-axis from ggplot (compatible with picasso::hide_x_axis)
#'
#' @return ggplot2 theme element
#' @export
hide_x_axis <- function() {
  ggplot2::theme(
    axis.text.x = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.title.x = ggplot2::element_blank()
  )
}

#' Hide y-axis from ggplot (compatible with picasso::hide_y_axis)
#'
#' @return ggplot2 theme element
#' @export
hide_y_axis <- function() {
  ggplot2::theme(
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank()
  )
}

#' Nightowl ggplot2 theme
#'
#' @param base_size Base font size
#' @param base_family Base font family
#' @return ggplot2 theme object
#' @export
theme_nightowl <- function(base_size = 11, base_family = "") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = base_size * 1.2, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = base_size * 0.9, color = "grey50"),
      strip.background = ggplot2::element_rect(fill = "grey90", color = NA),
      strip.text = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom"
    )
}

#' Check if color is dark
#'
#' @param color Color hex code or name
#' @return Logical indicating if color is dark
#' @export
is_dark <- function(color) {
  # Convert to RGB
  rgb_vals <- col2rgb(color)
  
  # Calculate luminance (simplified)
  luminance <- (0.299 * rgb_vals[1] + 0.587 * rgb_vals[2] + 0.114 * rgb_vals[3]) / 255
  
  return(luminance < 0.5)
}