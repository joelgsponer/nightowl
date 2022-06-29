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
    MetBrewer::scale_fill_met_d("Demuth", drop = F) +
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
add_inline_plot <- function(x,
                            y = rep(0, length(x)),
                            mapping = list(x = "x", y = "y"),
                            style = NULL,
                            xlim = NULL,
                            ylim = NULL,
                            height = 0.8,
                            width = 8,
                            scaling = 1,
                            theme = ggplot2::theme_void,
                            coord_flip = FALSE,
                            hide_title = T,
                            hide_legend = T,
                            hide_x_axis = T,
                            hide_y_axis = T,
                            fill = picasso::roche_colors("lightblue"),
                            expansion_y = 0,
                            add_download_button = FALSE,
                            fun.data = ggplot2::mean_cl_boot,
                            ...) {
  # Prepare paramters
  ## DATA
  if (!inherits(x, "data.frame")) {
    DATA <- tibble::tibble(x = x, y = y)
  } else {
    DATA <- x
  }
  ## Paramters added via ...
  if (!is.null(style)) {
    .style <- nightowl::load_style(style)
  } else {
    .ellipsis <- list(...)
    if (!is.null(.ellipsis$svg)) {
      cli::cli_alter_warning("For inline plots svg paramters should be passed directeley. Ignoring additional svg paramters in plot call.")
      .ellipsis$svg <- NULL
    }
    .style <- .ellipsis
  }
  ## Putting them together
  .args <- c(
    list(DATA = DATA),
    list(mapping = mapping),
    .style
  )
  .p <- do.call(nightowl::plot, .args)

  # Coord flip
  if (coord_flip) {
    .p <- .p + ggplot2::coord_flip()
  }

  # Axis limits
  if (!is.null(xlim) && is.numeric(.p$data[[mapping$x]])) {
    .p <- .p + ggplot2::scale_x_continuous(limits = xlim)
  }
  if (!is.null(ylim) && is.numeric(.p$data[[mapping$y]])) {
    .p <- .p + ggplot2::scale_y_continuous(limits = ylim)
  }
  .p <- .p + theme()

  # Hiding stuff
  if (hide_title) {
    .p <- .p + picasso::hide_title()
  }
  if (hide_legend) {
    .p <- .p + picasso::hide_legend()
  }
  if (hide_x_axis) {
    .p <- .p + picasso::hide_x_axis()
  }
  if (hide_y_axis) {
    .p <- .p + picasso::hide_y_axis()
  }

  # Create SVG
  nightowl::render_svg(.p,
    height = height,
    width = width,
    scaling = scaling,
    add_download_button = add_download_button
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
                       height = 0.7,
                       width = 3.5,
                       scaling = 1,
                       theme = ggplot2::theme_void,
                       hide_legend = T,
                       hide_x_axis = T,
                       hide_y_axis = T,
                       fill = picasso::roche_colors("lightblue"),
                       expansion_y = 0,
                       add_download_button = FALSE,
                       fun.data = ggplot2::mean_cl_boot) {
  .data <- tibble::tibble(x = x)
  .p <- ggplot2::ggplot(.data,
    mapping = ggplot2::aes(y = x, x = 0)
  ) +
    ggplot2::geom_violin(fill = fill) +
    ggplot2::stat_summary(fun.data = fun.data, size = 2) +
    ggplot2::coord_flip() +
    ggplot2:::scale_y_continuous(expand = ggplot2::expansion(expansion_y))
  if (!is.null(ylim)) {
    .p <- .p + ggplot2::ylim(ylim)
  }
  .p <- .p + theme()
  if (hide_legend) {
    .p <- .p + picasso::hide_legend()
  }
  if (hide_x_axis) {
    .p <- .p + picasso::hide_x_axis()
  }
  if (hide_y_axis) {
    .p <- .p + picasso::hide_y_axis()
  }
  nightowl::Plot$new(
    plot = .p,
    type = "NightowlViolin",
    resize = FALSE,
    options_svg = list(height = height, width = width, scaling = scaling, add_download_button = FALSE)
  ) %>%
    nightowl::new_NightowlPlots()
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
                        width = 8,
                        scaling = 1,
                        theme = ggplot2::theme_void,
                        hide_x_axis = T,
                        hide_y_axis = T,
                        fill = picasso_colors("lightblue"),
                        expansion_y = 0,
                        add_download_button = FALSE) {
  .data <- tibble::tibble(x = x)
  .p <- ggplot2::ggplot(.data,
    mapping = ggplot2::aes(y = x, x = 0)
  ) +
    ggplot2::geom_violin(fill = fill) +
    ggplot2::stat_summary(size = 2) +
    ggplot2::coord_flip() +
    ggplot2:::scale_y_continuous(expand = ggplot2::expansion(expansion_y))
  if (!is.null(ylim)) {
    .p <- .p + ggplot2::ylim(ylim)
  }
  .p <- .p + theme()
  if (!add_scale) {
    .p <- .p + picasso::hide_y_axis()
    .p <- .p + picasso::hide_x_axis()
  }
  nightowl::render_svg(.p,
    height = height,
    width = width,
    scaling = scaling,
    add_download_button = add_download_button
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
add_histogram <- function(x,
                          height = 0.8,
                          binwidth = NULL,
                          xlim = NULL,
                          add_scale = F) {
  .data <- tibble::tibble(x = x)
  .p <- ggplot2::ggplot(.data,
    mapping = ggplot2::aes(x = x)
  ) +
    ggplot2::geom_histogram(fill = picasso::roche_colors("lightblue"), binwidth = binwidth, color = "black") +
    ggplot2::theme_void() +
    ggplot2:::scale_y_continuous(expand = ggplot2::expansion(0))
  if (add_scale) {
    .p <- .p + picasso::add_x_axis()
  }
  nightowl::render_svg(.p, height = height, add_download_button = FALSE)
}
# =================================================
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
add_scale <- function(obj,
                      text_size = 1.5,
                      line_size = 1,
                      legend_position = "none") {
  obj <- dplyr::ungroup(obj)
  columns <- purrr::imap(obj, ~ if (nightowl::is_NightowlPlots(.x)) .y else NULL) %>%
    purrr::compact()
  options_svg <- NULL
  ggs <- purrr::map(columns, function(.column) {
    .obj <- obj[[.column]]
    .options_svg <- .obj[[1]]$options_svg
    .options_svg$height <- 0.3
    options_svg <<- .options_svg
    .gg <- nightowl::as_ggplot(.obj)[[1]]
    .gg$layers <- NULL
    .gg +
      ggplot2::theme_classic() +
      ggplot2::theme(
        legend.position = legend_position,
        axis.line.x = ggplot2::element_line(colour = "black", size = ggplot2::rel(line_size)),
        axis.line.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(size = ggplot2::rel(text_size)),
        axis.ticks.y = ggplot2::element_blank(),
        plot.margin = ggplot2::margin(0, 0, 0, 0, "cm"),
        axis.title.y = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank()
      )
  }) %>%
    purrr::set_names(columns) %>%
    purrr::map(~ nightowl::Plot$new(
      plot = .x,
      type = "NightowlScale",
      resize = FALSE,
      options_svg = options_svg
    ))
  .scales <- purrr::map(ggs, ~ nightowl::new_NightowlPlots(.x))
  dplyr::bind_rows(obj, .scales) %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    dplyr::mutate_if(is.character, function(x) tidyr::replace_na(x, ""))
}
