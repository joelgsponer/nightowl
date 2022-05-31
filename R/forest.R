# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
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
    ggplot2::geom_point(cex = 8, shape = 18, color = picasso::roche_colors("blue")) +
    theme() +
    ggplot2::theme(
      legend.position = "none"
    ) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab)

  if (hide_y_axis) {
    .p <- .p + ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    )
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
  res <- nightowl::render_svg(.p, height = height, add_download_button = FALSE) %>%
    shiny::HTML()
  class(res) <- c("nightowl_svg", class(res))
  return(res)
}
# =================================================
