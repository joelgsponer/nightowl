#' Inline plots for table cells
#'
#' @description
#' Small plots meant to sit inside a table column. Each function takes a
#' vector (typically one group's values, as passed by [calc_summary()]) and
#' returns a length-one [NightowlPlots] vector, so it can be used directly as
#' a calculation in a summary template.
#'
#' * `add_inline_plot()` draws any style or layer list with a minimal theme.
#' * `add_inline_histogram()` and `add_inline_pointrange()` are shortcuts for
#'   the `Inline-Histogram` and `Inline-Pointrange` styles.
#' * `add_inline_forestplot()` draws an estimate with an interval against a
#'   reference line, clipped to `xlim` with arrows where the interval leaves
#'   the range.
#' * `add_inline_barplot()` draws the frequencies of a categorical vector as
#'   one stacked bar.
#' * `add_inline_violin()` draws a horizontal violin with the mean and its
#'   confidence interval.
#'
#' All inline plots in one column should share `xlim` (or `ylim`) so they are
#' comparable; the summary templates take care of that.
#'
#' @param x A numeric vector, or for `add_inline_plot()` also a data frame.
#' @param y Optional y values when `x` is a vector.
#' @param mapping Named list of column names, see [DeclarativePlot].
#' @param style A style name or file, see [styles].
#' @param layers Layer specifications used when `style` is `NULL`.
#' @param xlim,ylim Axis limits shared across a column.
#' @param height,width,scaling SVG size in inches and text scaling. Inline plots
#'   render at their native pixel size (72 px per inch) inside tables.
#' @param coord_flip Flip the axes.
#' @param type Label used when the plot is printed.
#' @param fun_data For `add_inline_pointrange()` and `add_inline_forestplot()`,
#'   a function of `x` returning a data frame with columns `y`, `ymin`,
#'   `ymax` (such as [mean_ci()]).
#' @param xmin,xmax Interval bounds for `add_inline_forestplot()`; computed
#'   with `fun_data` when `NULL`.
#' @param xintercept Reference line for `add_inline_forestplot()`.
#' @param shape,size,alpha Point appearance.
#' @param palette Palette name, see [nightowl_palettes()].
#' @param fill Fill colour.
#' @param ... Passed to `add_inline_plot()`.
#' @return A [NightowlPlots] vector of length one.
#' @examples
#' add_inline_histogram(mtcars$mpg, xlim = range(mtcars$mpg))
#' add_inline_forestplot(1.2, 0.8, 1.7, xlim = c(0.5, 2), xintercept = 1)
#' add_inline_barplot(mtcars$cyl)
#' @name inline_plots
NULL

#' @rdname inline_plots
#' @export
add_inline_plot <- function(x, y = NULL, mapping = list(x = "x", y = "y"), style = NULL, layers = NULL,
                            xlim = NULL, ylim = NULL, height = 0.45, width = 3, scaling = 1,
                            coord_flip = FALSE, type = style %||% "InlinePlot", ...) {
  if (is.data.frame(x)) {
    data <- tibble::as_tibble(x)
    if (is.null(mapping$y)) {
      data$.padding <- 0
      mapping$y <- ".padding"
    }
  } else {
    data <- tibble::tibble(x = x, y = y %||% rep(0, length(x)))
    mapping <- modifyList(list(x = "x", y = "y"), mapping)
  }
  if (!is.null(style)) {
    layers <- load_style(style)$layers
  }
  if (is.null(layers)) {
    cli::cli_abort("Provide a {.arg style} or a list of {.arg layers}.")
  }
  gg <- DeclarativePlot$new(
    data = data, mapping = mapping, layers = layers,
    theming = list(theme = "ggplot2::theme_void"),
    annotation = list(title = FALSE),
    ...
  )$plot
  # limits go through the coordinate system so out-of-range data is clipped,
  # not dropped (which would distort histograms and summaries)
  if (!is.null(xlim)) xlim <- xlim + c(-1, 1) * 0.05 * diff(range(xlim))
  if (is.numeric(data[[mapping$x]]) && is.null(xlim)) {
    gg <- gg + ggplot2::scale_x_continuous(expand = ggplot2::expansion(0.1))
  }
  gg <- gg + if (coord_flip) {
    ggplot2::coord_flip(xlim = xlim, ylim = ylim)
  } else {
    ggplot2::coord_cartesian(xlim = xlim, ylim = ylim)
  }
  gg <- gg + theme_inline()
  as_inline(gg, type = type, height = height, width = width, scaling = scaling)
}

#' @rdname inline_plots
#' @export
add_inline_histogram <- function(x, xlim = NULL, ...) {
  add_inline_plot(x, mapping = list(x = "x", y = NULL), style = "Inline-Histogram", xlim = xlim, ...)
}

#' @rdname inline_plots
#' @export
add_inline_pointrange <- function(x, fun_data = mean_ci, xlim = NULL, ...) {
  est <- fun_data(x)
  add_inline_plot(
    est,
    mapping = list(x = "y", xmin = "ymin", xmax = "ymax", y = NULL),
    style = "Inline-Pointrange", xlim = xlim, ...
  )
}

#' @rdname inline_plots
#' @export
add_inline_forestplot <- function(x, xmin = NULL, xmax = NULL, fun_data = NULL, xlim = NULL,
                                  xintercept = NULL, height = 0.3, width = 3, scaling = 0.8,
                                  shape = 15, size = 4, alpha = 0.9) {
  if (is.null(xmin) || is.null(xmax)) {
    if (is.null(fun_data)) {
      cli::cli_abort("Provide {.arg xmin} and {.arg xmax}, or a {.arg fun_data} to compute them from {.arg x}.")
    }
    est <- fun_data(x)
    x <- est$y
    xmin <- est$ymin
    xmax <- est$ymax
  }
  if (length(x) != 1 || length(xmin) != 1 || length(xmax) != 1) {
    cli::cli_abort("{.arg x}, {.arg xmin} and {.arg xmax} must each have length one.")
  }
  accent <- unname(nightowl_colours("accent"))
  reference <- unname(nightowl_colours("reference"))
  ink <- unname(nightowl_colours("ink"))
  data <- tibble::tibble(x = x, xmin = xmin, xmax = xmax, y = 0)
  gg <- ggplot2::ggplot(data, ggplot2::aes(x = .data$x, y = .data$y))
  if (!is.null(xintercept)) {
    gg <- gg + ggplot2::geom_vline(xintercept = xintercept, colour = reference, linewidth = 0.8)
  }
  if (!is.null(xlim)) {
    lo <- max(xmin, xlim[1], na.rm = TRUE)
    hi <- min(xmax, xlim[2], na.rm = TRUE)
    gg <- gg + ggplot2::annotate("segment", x = lo, xend = hi, y = 0, yend = 0, colour = ink, linewidth = 0.6)
    arrow <- grid::arrow(length = grid::unit(0.18, "cm"), type = "closed")
    if (isTRUE(xmin < xlim[1])) {
      gg <- gg + ggplot2::annotate("segment", x = lo + diff(xlim) * 0.06, xend = xlim[1], y = 0, yend = 0,
                                   colour = ink, linewidth = 0.6, arrow = arrow)
    }
    if (isTRUE(xmax > xlim[2])) {
      gg <- gg + ggplot2::annotate("segment", x = hi - diff(xlim) * 0.06, xend = xlim[2], y = 0, yend = 0,
                                   colour = ink, linewidth = 0.6, arrow = arrow)
    }
    if (isTRUE(x >= xlim[1] && x <= xlim[2])) {
      gg <- gg + ggplot2::geom_point(shape = shape, size = size, colour = accent, alpha = alpha)
    }
    gg <- gg + ggplot2::scale_x_continuous(limits = xlim, expand = ggplot2::expansion(0.02))
  } else {
    gg <- gg +
      ggplot2::geom_errorbar(ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax), width = 0, orientation = "y", colour = ink, linewidth = 0.6) +
      ggplot2::geom_point(shape = shape, size = size, colour = accent, alpha = alpha)
  }
  gg <- gg + ggplot2::coord_cartesian(ylim = c(-1, 1)) + theme_inline()
  as_inline(gg, type = "ForestPlot", height = height, width = width, scaling = scaling)
}

#' @rdname inline_plots
#' @export
add_inline_barplot <- function(x, height = 0.3, width = 2.5, scaling = 1, palette = nightowl_option("palette")) {
  x <- explicit_missing(x)
  counts <- table(x)
  data <- tibble::tibble(level = factor(names(counts), levels = rev(names(counts))), pct = as.numeric(counts) / length(x) * 100)
  cols <- level_colours(names(counts), name = palette)
  gg <- ggplot2::ggplot(data, ggplot2::aes(x = .data$pct, y = 1, fill = .data$level)) +
    ggplot2::geom_col(orientation = "y", width = 0.9) +
    ggplot2::scale_fill_manual(values = cols, drop = FALSE) +
    ggplot2::scale_x_continuous(limits = c(0, 100.1), expand = ggplot2::expansion(0)) +
    theme_inline()
  as_inline(gg, type = "Barplot", height = height, width = width, scaling = scaling)
}

#' @rdname inline_plots
#' @export
add_inline_violin <- function(x, ylim = NULL, height = 0.3, width = 2.5, scaling = 1,
                              fill = nightowl_colours("fill"), fun_data = mean_ci) {
  data <- tibble::tibble(x = x[!is.na(x)])
  gg <- ggplot2::ggplot(data, ggplot2::aes(y = .data$x, x = 0)) +
    ggplot2::geom_violin(fill = unname(fill), colour = NA) +
    ggplot2::stat_summary(fun.data = fun_data, colour = unname(nightowl_colours("ink")), linewidth = 0.5, size = 0.4) +
    ggplot2::coord_flip(ylim = ylim) +
    theme_inline()
  as_inline(gg, type = "Violin", height = height, width = width, scaling = scaling)
}

as_inline <- function(gg, type, height, width, scaling) {
  new_NightowlPlots(Plot$new(
    plot = gg, type = type, resize = FALSE,
    svg = list(height = height, width = width, scaling = scaling, download_button = FALSE)
  ))
}
