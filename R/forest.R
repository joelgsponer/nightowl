# =================================================
#' @title Create Forest Plot
#' @description 
#' Create a forest plot visualization showing point estimates with confidence intervals.
#' Commonly used in meta-analysis and clinical research to display effect sizes.
#' @param x Numeric vector of point estimates
#' @param xmin Numeric vector of lower confidence interval bounds
#' @param xmax Numeric vector of upper confidence interval bounds  
#' @param xlim Optional numeric vector of length 2 specifying x-axis limits
#' @param xintercept Numeric value for vertical reference line (e.g., null effect)
#' @param height Numeric value specifying plot height (default: 0.5)
#' @param xlab Character string for x-axis label
#' @param ylab Character string for y-axis label
#' @param hide_y_axis Logical indicating whether to hide y-axis (default: TRUE)
#' @param hide_x_axis Logical indicating whether to hide x-axis (default: TRUE)
#' @param hide_legend Logical indicating whether to hide legend (default: TRUE)
#' @param shape Numeric value specifying point shape (default: 15)
#' @param theme ggplot2 theme function to apply (default: theme_void)
#' @return An SVG forest plot rendered with nightowl styling
#' @export
forestplot <- function(x,
                       xmin,
                       xmax,
                       xlim = NULL,
                       xintercept = NULL,
                       height = 0.5,
                       xlab = NULL,
                       ylab = NULL,
                       hide_y_axis = TRUE,
                       hide_x_axis = TRUE,
                       hide_legend = TRUE,
                       shape = 15,
                       theme = ggplot2::theme_void) {
  .data <- tibble::tibble(x = x, xmin = xmin, xmax = xmax)
  .p <- ggplot2::ggplot(NULL, ggplot2::aes(
    y = 0,
    x = x,
    xmin = xmin,
    xmax = xmax
  )) +
    ggplot2::geom_vline(xintercept = xintercept, color = picasso::roche_colors("red"), linetype = "solid", size = 1) +
    ggplot2::geom_errorbarh() +
    ggplot2::geom_point(cex = 8, shape = shape, color = picasso::roche_colors("blue")) +
    theme() +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab)


  if (hide_legend) {
    .p <- .p + picasso::hide_legend()
  }
  if (hide_x_axis) {
    .p <- .p + picasso::hide_x_axis()
  }
  if (hide_y_axis) {
    .p <- .p + picasso::hide_y_axis()
  }


  if (!is.null(xlim)) {
    .p <- .p + ggplot2::xlim(xlim[1], xlim[2])

    if (xmin < xlim[1]) {
      .p <- .p + ggplot2::geom_text(
        mapping = ggplot2::aes(x = xlim[1], label = "<<<"),
        color = picasso::roche_colors("black"),
        hjust = 0.3,
        size = 9
      ) +
        ggplot2::geom_point(mapping = ggplot2::aes(x = xmax), cex = 8, shape = 108, color = picasso::roche_colors("black"))
    }

    if (xmax > xlim[2]) {
      .p <- .p + ggplot2::geom_text(
        mapping = ggplot2::aes(x = xlim[2], label = ">>>"),
        color = picasso::roche_colors("black"),
        size = 9
      ) +
        ggplot2::geom_point(mapping = ggplot2::aes(x = xmin), cex = 8, shape = 108, color = picasso::roche_colors("black"))
    }

    if (x < xlim[1]) {
      .p <- .p + ggplot2::geom_text(
        mapping = ggplot2::aes(x = xlim[1], label = "<<<"),
        color = picasso::roche_colors("blue"),
        hjust = 0.3,
        size = 9
      )
    }


    if (x > xlim[2]) {
      .p <- .p + ggplot2::geom_text(
        mapping = ggplot2::aes(x = xlim[2], label = ">>>"),
        color = picasso::roche_colors("blue"),
        size = 9
      )
    }
  }
  res <- nightowl::render_svg(.p, height = height, add_download_button = FALSE)
  return(res)
}
# =================================================
