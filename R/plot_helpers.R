#' Hide Plot Legend
#'
#' @description
#' Remove the legend from a ggplot2 plot, replacing picasso::hide_legend()
#'
#' @return A ggplot2 theme element
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
#'   geom_point() +
#'   hide_legend()
hide_legend <- function() {
  ggplot2::theme(legend.position = "none")
}

#' Hide Plot Title
#'
#' @description
#' Remove the title from a ggplot2 plot, replacing picasso::hide_title()
#'
#' @return A ggplot2 theme element
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   ggtitle("My Plot") +
#'   hide_title()
hide_title <- function() {
  ggplot2::theme(
    plot.title = ggplot2::element_blank(),
    plot.subtitle = ggplot2::element_blank()
  )
}

#' Hide X-Axis
#'
#' @description
#' Remove all x-axis elements from a ggplot2 plot, replacing picasso::hide_x_axis()
#'
#' @return A ggplot2 theme element
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   hide_x_axis()
hide_x_axis <- function() {
  ggplot2::theme(
    axis.text.x = ggplot2::element_blank(),
    axis.title.x = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.line.x = ggplot2::element_blank()
  )
}

#' Hide Y-Axis
#'
#' @description
#' Remove all y-axis elements from a ggplot2 plot, replacing picasso::hide_y_axis()
#'
#' @return A ggplot2 theme element
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   hide_y_axis()
hide_y_axis <- function() {
  ggplot2::theme(
    axis.text.y = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    axis.line.y = ggplot2::element_blank()
  )
}

#' Show X-Axis
#'
#' @description
#' Ensure x-axis elements are visible in a ggplot2 plot, replacing picasso::add_x_axis()
#'
#' @param title Include axis title (default: TRUE)
#' @param text Include axis text/labels (default: TRUE)
#' @param ticks Include axis ticks (default: TRUE)
#' @param line Include axis line (default: TRUE)
#' @return A ggplot2 theme element
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   hide_x_axis() +
#'   add_x_axis()
add_x_axis <- function(title = TRUE, text = TRUE, ticks = TRUE, line = TRUE) {
  theme_elements <- list()
  
  if (title) {
    theme_elements$axis.title.x <- ggplot2::element_text()
  }
  if (text) {
    theme_elements$axis.text.x <- ggplot2::element_text()
  }
  if (ticks) {
    theme_elements$axis.ticks.x <- ggplot2::element_line()
  }
  if (line) {
    theme_elements$axis.line.x <- ggplot2::element_line()
  }
  
  do.call(ggplot2::theme, theme_elements)
}

#' Show Y-Axis
#'
#' @description
#' Ensure y-axis elements are visible in a ggplot2 plot
#'
#' @param title Include axis title (default: TRUE)
#' @param text Include axis text/labels (default: TRUE)
#' @param ticks Include axis ticks (default: TRUE)
#' @param line Include axis line (default: TRUE)
#' @return A ggplot2 theme element
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   hide_y_axis() +
#'   add_y_axis()
add_y_axis <- function(title = TRUE, text = TRUE, ticks = TRUE, line = TRUE) {
  theme_elements <- list()
  
  if (title) {
    theme_elements$axis.title.y <- ggplot2::element_text()
  }
  if (text) {
    theme_elements$axis.text.y <- ggplot2::element_text()
  }
  if (ticks) {
    theme_elements$axis.ticks.y <- ggplot2::element_line()
  }
  if (line) {
    theme_elements$axis.line.y <- ggplot2::element_line()
  }
  
  do.call(ggplot2::theme, theme_elements)
}