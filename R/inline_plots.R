# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
add_barplot <- function(x) {
  counts <- base::table(x) / length(x) * 100
  .p <- tibble::tibble(fill = names(counts), y = counts) %>%
    dplyr::mutate(fill = forcats::fct_inorder(fill)) %>%
    dplyr::mutate(fill = forcats::fct_rev(fill)) %>%
    ggplot2::ggplot(ggplot2::aes(x = 1, y = y, fill = fill)) +
    ggplot2::geom_col() +
    ggplot2::theme_void() +
    ggplot2::coord_flip() +
    ggplot2:::scale_fill_viridis_d(drop = FALSE) +
    ggplot2:::scale_x_discrete(expand = ggplot2::expansion(0)) +
    ggplot2:::scale_y_continuous(expand = ggplot2::expansion(0)) +
    picasso::theme_void()
  nightowl::render_svg(.p, height = 0.8, add_download_button = FALSE) %>%
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
add_forestplot <- function(x,
                           fun_data = ggplot2::mean_cl_boot,
                           xintercept = NULL,
                           xlim = NULL) {
  vals <- fun_data(x)
  nightowl::forestplot(
    x = vals[[1]],
    xmin = vals[[2]],
    xmax = vals[[3]],
    xintercept = xintercept,
    xlim = xlim
  )
}
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
add_violin <- function(x,
                       ylim = NULL,
                       height = 0.8,
                       theme = ggplot2::theme_void) {
  .data <- tibble::tibble(x = x)
  .p <- ggplot2::ggplot(.data,
    mapping = ggplot2::aes(y = x, x = 0)
  ) +
    ggplot2::geom_violin(fill = picasso::roche_colors("lightblue")) +
    ggplot2::stat_summary(size = 2) +
    ggplot2::coord_flip() +
    ggplot2:::scale_x_discrete(expand = ggplot2::expansion(0)) +
    ggplot2:::scale_y_continuous(expand = ggplot2::expansion(0))
  if (!is.null(ylim)) {
    .p <- .p + ggplot2::ylim(ylim)
  }
  .p <- .p + theme()
  nightowl::render_svg(.p, height = height, add_download_button = FALSE) %>%
    shiny::HTML()
}
# ===============================================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
add_density <- function(x,
                        ylim = NULL,
                        height = 0.8,
                        theme = ggplot2::theme_void) {
  .data <- tibble::tibble(x = x)
  .p <- ggplot2::ggplot(.data,
    mapping = ggplot2::aes(x)
  ) +
    ggplot2::geom_density()

  if (!is.null(ylim)) {
    .p <- .p + ggplot2::ylim(ylim)
  }
  .p <- .p + theme()
  nightowl::render_svg(.p, height = height, add_download_button = FALSE) %>%
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
add_histogram <- function(x,
                          height = 0.8,
                          binwidth = NULL,
                          xlim = NULL) {
  .data <- tibble::tibble(x = x)
  .p <- ggplot2::ggplot(.data,
    mapping = ggplot2::aes(x = x)
  ) +
    ggplot2::geom_histogram(fill = picasso::roche_colors("lightblue"), binwidth = binwidth, color = "black") +
    ggplot2::theme_void() +
    ggplot2:::scale_x_discrete(expand = ggplot2::expansion(0)) +
    ggplot2:::scale_y_continuous(expand = ggplot2::expansion(0))
  nightowl::render_svg(.p, height = height, add_download_button = FALSE) %>%
    shiny::HTML()
}
# =================================================
add_scale <- function(obj,
                      column,
                      height = 0.5,
                      theme = ggplot2::theme_classic) {
  browser()
  stopifnot(inherits(obj[[column]], "nightowl_svg"))
  .attr <- attributes(obj)$parameters[[column]]
  stopifnot(!is.null(.attr$xlim))

  scale <- ggplot2::ggplot() +
    ggplot2::scale_x_continuous(limits = .attr$xlim) +
    theme() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank()
    )
  new_scale <- list()
  new_scale[[column]] <- nightowl::render_svg(scale, height = height, add_download_button = F)
  dplyr::bind_rows(obj, new_scale)
}
