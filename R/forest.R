# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
forestplot <- function(x, xmin, xmax, xlim = NULL, xintercept = NULL) {
  if (is.null(xlim)) {
    xlim <- c(xmin, xmax) + c(xmin, xmax) / 100 * 10
  }
  .data <- tibble::as_tibble(x = x, xmin = xmin, xmax = xmax)
  if (is.null(xlim)) {
  }
  .p <- ggplot2::ggplot(.data, ggplot2::aes(
    y = 0,
    x = x,
    xmin = xmin,
    xmax = xmax
  )) +
    ggplot2::geom_vline(xintercept = xintercept, color = picasso::roche_colors("red"), linetype = "solid", size = 1) +
    ggplot2::geom_errorbarh() +
    ggplot2::geom_point(cex = 8, shape = 18, color = picasso::roche_colors("blue")) +
    ggplot2::xlim(xlim) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "none"
    )
  nightowl::render_svg(.p, height = 0.3, add_download_button = FALSE) %>%
    shiny::HTML()
}
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
add_forestplot <- function(data, x, xmin, xmax, xintercept = NULL) {
  .range <- c(min(data[[xmin]]), max(data[[xmax]]))
  data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Forest = nightowl::forestplot(!!rlang::sym(x), !!rlang::sym(xmin), !!rlang::sym(xmax), xlim = .range, xintercept = xintercept))
}
# =================================================
