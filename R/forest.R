# =================================================
#' @title Create forest plot with confidence intervals
#' @description
#' Creates a forest plot visualization showing point estimates with confidence intervals,
#' optional reference lines, and out-of-bounds indicators for values exceeding plot limits.
#' @param x Numeric. Point estimate value(s) for the main effect
#' @param xmin Numeric. Lower confidence limit(s)
#' @param xmax Numeric. Upper confidence limit(s)
#' @param xlim Numeric vector of length 2. Plot x-axis limits. If NULL, uses data range
#' @param xintercept Numeric. Value for vertical reference line (e.g., null effect). Default NULL
#' @param height Numeric. Plot height for SVG rendering. Default 0.5
#' @param xlab String. X-axis label. Default NULL
#' @param ylab String. Y-axis label. Default NULL
#' @param hide_y_axis Logical. Whether to hide y-axis. Default TRUE
#' @param hide_x_axis Logical. Whether to hide x-axis. Default TRUE
#' @param hide_legend Logical. Whether to hide legend. Default TRUE
#' @param shape Integer. Point shape for main estimates. Default 15 (square)
#' @param theme ggplot2 theme function. Default ggplot2::theme_void
#' @return An SVG string containing the rendered forest plot
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
  # Input validation
  x <- validate_numeric(x, param_name = "x")
  xmin <- validate_numeric(xmin, param_name = "xmin")
  xmax <- validate_numeric(xmax, param_name = "xmax")
  height <- validate_numeric(height, min_value = 0, param_name = "height")
  
  if (!is.null(xlim)) {
    xlim <- validate_numeric(xlim, param_name = "xlim")
    if (length(xlim) != 2) {
      throw_validation_error("xlim must be a numeric vector of length 2", "xlim", xlim)
    }
  }
  if (!is.null(xintercept)) {
    xintercept <- validate_numeric(xintercept, param_name = "xintercept")
  }
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
