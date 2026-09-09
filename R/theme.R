#' nightowl ggplot2 theme
#'
#' A quiet theme for publication figures: light horizontal grid, ink-grey text,
#' legend on top, left-aligned title. It is the default theme of
#' [DeclarativePlot].
#'
#' @param base_size Base font size in points.
#' @param base_family Font family. Empty uses the device default; SVG output
#'   sets the `font_family` option as CSS instead.
#' @param grid Which major grid lines to draw: `"y"`, `"xy"`, or `"none"`.
#' @param legend_position Passed to [ggplot2::theme()].
#' @return A ggplot2 theme object.
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   theme_nightowl()
#' @export
theme_nightowl <- function(base_size = 11, base_family = "", grid = c("y", "xy", "none"),
                           legend_position = "top") {
  grid <- rlang::arg_match(grid)
  ink <- nightowl_colours("ink")
  muted <- nightowl_colours("muted")
  grid_line <- ggplot2::element_line(colour = "#E4E4E4", linewidth = 0.3)
  th <- ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      text = ggplot2::element_text(colour = ink),
      plot.title = ggplot2::element_text(face = "bold", hjust = 0, size = ggplot2::rel(1.15),
                                         margin = ggplot2::margin(b = base_size * 0.4)),
      plot.subtitle = ggplot2::element_text(colour = muted, hjust = 0, margin = ggplot2::margin(b = base_size * 0.6)),
      plot.caption = ggplot2::element_text(colour = muted, hjust = 0, size = ggplot2::rel(0.8),
                                           margin = ggplot2::margin(t = base_size * 0.6)),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      axis.title = ggplot2::element_text(colour = muted, size = ggplot2::rel(0.9)),
      axis.text = ggplot2::element_text(colour = ink, size = ggplot2::rel(0.85)),
      axis.ticks = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_line(colour = "#BDBDBD", linewidth = 0.4),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = if (grid == "xy") grid_line else ggplot2::element_blank(),
      panel.grid.major.y = if (grid %in% c("y", "xy")) grid_line else ggplot2::element_blank(),
      legend.position = legend_position,
      legend.title = ggplot2::element_text(colour = muted, size = ggplot2::rel(0.9)),
      legend.text = ggplot2::element_text(size = ggplot2::rel(0.85)),
      legend.key.size = ggplot2::unit(base_size, "pt"),
      strip.text = ggplot2::element_text(face = "bold", colour = ink, hjust = 0, size = ggplot2::rel(0.9),
                                         margin = ggplot2::margin(b = base_size * 0.4)),
      strip.background = ggplot2::element_blank(),
      panel.spacing = ggplot2::unit(base_size, "pt"),
      plot.margin = ggplot2::margin(base_size, base_size, base_size * 0.6, base_size * 0.6),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA)
    )
  th
}

#' Theme for inline plots: nothing but the data
#' @noRd
theme_inline <- function(margin = c(0, 15, 0, 15)) {
  ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "none",
      plot.margin = ggplot2::margin(margin[1], margin[2], margin[3], margin[4], unit = "pt"),
      plot.background = ggplot2::element_rect(fill = "transparent", colour = NA)
    )
}

#' Theme elements that hide an axis
#' @noRd
theme_hide_axis <- function(axis = c("x", "y")) {
  axis <- match.arg(axis)
  args <- setNames(
    list(ggplot2::element_blank(), ggplot2::element_blank(), ggplot2::element_blank(), ggplot2::element_blank()),
    paste0(c("axis.text.", "axis.title.", "axis.ticks.", "axis.line."), axis)
  )
  do.call(ggplot2::theme, args)
}

#' Theme for the scale row under a column of inline plots
#' @noRd
theme_scale_row <- function(text_size = 0.9, line_size = 0.6) {
  ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = "none",
      axis.line.x = ggplot2::element_line(colour = nightowl_colours("ink"), linewidth = ggplot2::rel(line_size)),
      axis.line.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = ggplot2::rel(text_size)),
      axis.title = ggplot2::element_blank(),
      plot.title = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(0, 15, 0, 15, unit = "pt"),
      plot.background = ggplot2::element_rect(fill = "transparent", colour = NA)
    )
}
