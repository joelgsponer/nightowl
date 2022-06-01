# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
add_barplot <- function(x) {
  x <- factor(x) %>%
    forcats::fct_explicit_na()
  counts <- base::table(x) / length(x) * 100
  .p <- tibble::tibble(fill = names(counts), y = counts) %>%
    dplyr::mutate(fill = forcats::fct_inorder(fill)) %>%
    dplyr::mutate(fill = forcats::fct_rev(fill)) %>%
    ggplot2::ggplot(ggplot2::aes(y = 1, x = y, fill = fill)) +
    ggplot2::geom_col(orientation = "y") +
    ggplot2:::scale_fill_viridis_d(drop = FALSE) +
    ggplot2:::scale_y_discrete(expand = ggplot2::expansion(0)) +
    ggplot2::scale_x_continuous(limits = c(0, 100)) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "none",
      plot.margin = ggplot2::margin(0, 0, 0, 0, "cm")
    )
  nightowl::render_svg(.p, height = 0.8, add_download_button = FALSE)
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
    ggplot2:::scale_y_continuous(expand = ggplot2::expansion(0))
  if (!is.null(ylim)) {
    .p <- .p + ggplot2::ylim(ylim)
  }
  .p <- .p + theme()
  nightowl::render_svg(.p, height = height, add_download_button = FALSE)
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
  nightowl::render_svg(.p, height = height, add_download_button = FALSE)
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
    ggplot2:::scale_y_continuous(expand = ggplot2::expansion(0))
  nightowl::render_svg(.p, height = height, add_download_button = FALSE)
}
# =================================================
add_scale <- function(obj,
                      height = 1,
                      scaling = 3.5,
                      legend_position = "none") {
  columns <- purrr::imap(obj, ~ if (nightowl::is_nightowl_svg(.x)) .y else NULL) %>%
    purrr::compact()


  ggs <- purrr::map(columns, function(.column) {
    .gg <- nightowl::as_ggplot(obj[[.column]])
    .gg$layers <- NULL
    .gg +
      ggplot2::theme_classic() +
      ggplot2::theme(
        legend.position = legend_position,
        axis.line.x = ggplot2::element_line(colour = "black", size = ggplot2::rel(1)),
        axis.line.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(size = ggplot2::rel(1)),
        axis.ticks.y = ggplot2::element_blank(),
        plot.margin = ggplot2::margin(0, 0, 0, 0, "cm"),
        axis.title.y = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank()
      )
  }) %>%
    purrr::set_names(columns)

  .scales <- purrr::map(ggs, ~ nightowl::render_svg(.x, height = height, scaling = scaling, add_download_button = FALSE))
  dplyr::bind_rows(obj, .scales)
}
