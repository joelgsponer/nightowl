# =================================================
#' @title
#' MISSING_TITLE
#' @export
add_inline_forestplot <- function(x,
                                  xmin,
                                  xmax,
                                  xlim = NULL,
                                  xintercept = NULL,
                                  xlab = NULL,
                                  ylab = NULL,
                                  hide_y_axis = TRUE,
                                  hide_x_axis = TRUE,
                                  hide_legend = TRUE,
                                  height = 0.3,
                                  width = 3,
                                  scaling = 0.8,
                                  shape = 15,
                                  size = 4.5,
                                  alpha = 0.8,
                                  breaks = seq(-10, 10, 0.5),
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
    ggplot2::geom_point(size = size, shape = shape, color = picasso::roche_colors("blue", alpha = alpha)) +
    ggplot2::geom_point(size = 3, shape = 8, color = "black") + # picasso::roche_colors("black", alpha = alpha)) +
    theme() +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab)

  .p <- .p + ggplot2::theme(plot.margin = ggplot2::margin(t = 0, r = 15, b = 0, l = 15, unit = "pt"))


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
    .p <- .p + ggplot2:::scale_x_continuous(limits = c(xlim[1], xlim[2]))
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
  } else {
    .p <- .p + ggplot2:::scale_x_continuous(breaks)
  }

  nightowl::Plot$new(
    plot = .p,
    type = "ForestPlot",
    resize = FALSE,
    options_svg = list(height = height, width = width, scaling = scaling, add_download_button = FALSE)
  ) %>%
    nightowl::new_NightowlPlots()
}
# =================================================
#' @title
#' MISSING_TITLE
#' @export
add_barplot <- function(x,
                        height = 0.3,
                        width = 2.5,
                        scaling = 1,
                        colors = NightowlOptions$get_colors) {
  if (!is.factor(x)) {
    x <- factor(x) %>%
      forcats::fct_explicit_na()
  }
  counts <- base::table(x) / length(x) * 100
  colors <- colors(length(counts), missing = "(Missing)" %in% names(counts)) %>%
    rev()
  .p <- tibble::tibble(fill = names(counts), y = counts) %>%
    dplyr::mutate(fill = forcats::fct_inorder(fill)) %>%
    dplyr::mutate(fill = forcats::fct_rev(fill)) %>%
    ggplot2::ggplot(ggplot2::aes(y = 1, x = y, fill = fill)) +
    ggplot2::geom_col(orientation = "y") +
    ggplot2::scale_fill_manual(values = colors, drop = F) +
    ggplot2:::scale_y_discrete(expand = ggplot2::expansion(0)) +
    ggplot2::scale_x_continuous(limits = c(0, 100.1)) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "none",
      plot.margin = ggplot2::margin(t = 0, r = 15, b = 0, l = 15, unit = "pt")
    )
  nightowl::Plot$new(
    plot = .p,
    type = "Barplot",
    resize = FALSE,
    options_svg = list(height = height, width = width, scaling = scaling, add_download_button = FALSE)
  ) %>%
    nightowl::new_NightowlPlots()
}
# =================================================
#' @title
#' MISSING_TITLE
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
#' @export
add_inline_plot <- function(x,
                            y = NULL,
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
                            ...) {
  # Prepare paramters
  ## DATA
  if (!inherits(x, "data.frame")) {
    if (is.null(y)) y <- rep(0, length(x))
    DATA <- tibble::tibble(x = x, y = y)
  } else {
    DATA <- x
    if (is.null(mapping$y)) DATA$padding <- rep(0, nrow(x))
    mapping$y <- "padding"
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
    list(data = DATA),
    list(mapping = mapping),
    .style
  )
  .p <- do.call(nightowl::plot, .args)$plot

  # Coord flip
  if (coord_flip) {
    .p <- .p + ggplot2::coord_flip()
  }
  # Axis limits
  if (!is.null(xlim) && is.numeric(.p$data[[mapping$x]])) {
    .p <- .p + ggplot2::scale_x_continuous(limits = xlim, expand = ggplot2::expansion(0.1))
  } else {
    .p <- .p + ggplot2::scale_x_continuous(expand = ggplot2::expansion(0.1))
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

  .p <- .p + ggplot2::theme(plot.margin = ggplot2::margin(t = 0, r = 15, b = 0, l = 15, unit = "pt"))

  nightowl::Plot$new(
    plot = .p,
    type = style,
    resize = FALSE,
    options_svg = list(height = height, width = width, scaling = scaling, add_download_button = FALSE)
  ) %>%
    nightowl::new_NightowlPlots()
}
# =================================================
#' @title
#' MISSING_TITLE
#' @export
add_inline_histogram <- function(x,
                                 mapping = list(x = "x", y = NULL),
                                 ...) {
  nightowl::add_inline_plot(x,
    mapping = mapping,
    style = "Inline-Histogram",
    ...
  )
}
# =================================================
#' @title
#' MISSING_TITLE
#' @export
add_inline_pointrange <- function(x,
                                  fun_data = NULL,
                                  mapping = list(x = "y", xmin = "ymin", xmax = "ymax", y = NULL),
                                  ...) {
  if (!is.null(fun_data)) {
    x <- fun_data(x)
  }
  nightowl::add_inline_plot(x,
    mapping = mapping,
    style = "Inline-Pointrange",
    ...
  )
}
# =================================================
#' @title
#' MISSING_TITLE
#' @export
add_violin <- function(x,
                       ylim = NULL,
                       height = 0.3,
                       width = 2.5,
                       scaling = 1,
                       theme = ggplot2::theme_void,
                       hide_legend = T,
                       hide_x_axis = T,
                       hide_y_axis = T,
                       fill = "#B9B9B8", # picasso::roche_colors("lightblue"),
                       expansion_y = 10,
                       add_download_button = FALSE,
                       fun.data = ggplot2::mean_cl_boot) {
  .data <- tibble::tibble(x = x)
  .p <- ggplot2::ggplot(.data,
    mapping = ggplot2::aes(y = x, x = 0)
  ) +
    ggplot2::geom_violin(fill = fill) +
    ggplot2::stat_summary(fun.data = fun.data, size = 1) +
    ggplot2::coord_flip() +
    ggplot2:::scale_y_continuous(expand = ggplot2::expansion(expansion_y))
  if (!is.null(ylim)) {
    .p <- .p + ggplot2::ylim(ylim)
  }
  .p <- .p + theme()
  if (hide_legend) {
    .p <- .p + picasso::hide_legend()
    .p <- .p + ggplot2::theme(plot.margin = ggplot2::margin(t = 0, r = 15, b = 0, l = 15, unit = "pt"))
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
    ggplot2:::scale_x_continuous(expand = ggplot2::expansion(0.1)) +
    ggplot2:::scale_y_continuous(expand = ggplot2::expansion(0.1))
  if (add_scale) {
    .p <- .p + picasso::add_x_axis()
  }
  nightowl::render_svg(.p, height = height, add_download_button = FALSE)
}
# =================================================
#' @title
#' MISSING_TITLE
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
        plot.margin = ggplot2::margin(t = 0, r = 15, b = 0, l = 15, unit = "pt"),
        plot.title = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank()
      )
  })
  ggs <- ggs %>%
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
# =================================================
#' @export
make_scale <- function(obj,
                       height = 0.3,
                       text_size = 1.5,
                       line_size = 1,
                       legend_position = "none") {
  obj <- obj[[1]]
  .options_svg <- obj$options_svg
  .options_svg$height <- height
  .gg <- obj$plot
  .gg$layers <- NULL
  p <- .gg +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = legend_position,
      axis.line.x = ggplot2::element_line(colour = "black", size = ggplot2::rel(line_size)),
      axis.line.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = ggplot2::rel(text_size)),
      axis.ticks.y = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(t = 0, r = 15, b = 0, l = 15, unit = "pt"),
      plot.title = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank()
    )
  nightowl::Plot$new(
    plot = p,
    type = "NightowlScale",
    resize = FALSE,
    options_svg = .options_svg
  )
}
