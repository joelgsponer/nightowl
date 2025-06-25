# =================================================
#' @title Create inline forest plots for embedding in tables
#' @description
#' Creates compact forest plots suitable for embedding within table cells,
#' with customizable confidence intervals and out-of-bounds indicators.
#' @param x Numeric. Point estimate value(s) for the main effect
#' @param xmin Numeric. Lower confidence limit(s)
#' @param xmax Numeric. Upper confidence limit(s)
#' @param xlim Numeric vector of length 2. Plot x-axis limits. If NULL, uses data range
#' @param xintercept Numeric. Value for vertical reference line. Default NULL
#' @param xlab String. X-axis label. Default NULL
#' @param ylab String. Y-axis label. Default NULL
#' @param hide_y_axis Logical. Whether to hide y-axis. Default TRUE
#' @param hide_x_axis Logical. Whether to hide x-axis. Default TRUE
#' @param hide_legend Logical. Whether to hide legend. Default TRUE
#' @param height Numeric. Plot height for inline display. Default 0.3
#' @param width Numeric. Plot width for inline display. Default 3
#' @param scaling Numeric. SVG scaling factor. Default 0.8
#' @param shape Integer. Point shape for estimates. Default 15
#' @param size Numeric. Point size. Default 4
#' @param alpha Numeric. Point transparency. Default 0.8
#' @param breaks Numeric vector. X-axis breaks. Default seq(-10, 10, 2)
#' @param theme ggplot2 theme function. Default ggplot2::theme_void
#' @return A NightowlPlots object containing the inline forest plot
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
                                  size = 4,
                                  alpha = 0.8,
                                  breaks = seq(-10, 10, 2),
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
    .p <- .p + ggplot2:::scale_x_continuous(limits = c(xlim[1], xlim[2]), breaks = breaks)

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
#' @title Create inline stacked bar plots for categorical data
#' @description
#' Creates compact horizontal stacked bar plots showing percentage distributions
#' of categorical variables, suitable for embedding in summary tables.
#' @param x A vector (factor or character) containing categorical data
#' @param height Numeric. Plot height for inline display. Default 0.3
#' @param width Numeric. Plot width for inline display. Default 2.5
#' @param scaling Numeric. SVG scaling factor. Default 1
#' @param colors Function. Color palette function for categories
#' @return A NightowlPlots object containing the inline stacked bar plot
#' @export
add_barplot <- function(x,
                        height = 0.3,
                        width = 2.5,
                        scaling = 1,
                        colors = get_nightowl_options()$get_colors) {
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
#' @title Create forest plot from raw data with confidence intervals
#' @description
#' Generates a forest plot by applying statistical functions to raw data
#' to calculate point estimates and confidence intervals automatically.
#' @param x Numeric vector. Raw data for forest plot calculation
#' @param fun_data Function. Statistical function to calculate estimates and CI. Default ggplot2::mean_cl_boot
#' @param xintercept Numeric. Value for vertical reference line. Default NULL
#' @param xlim Numeric vector of length 2. Plot x-axis limits. If NULL, uses data range
#' @return An SVG string containing the rendered forest plot
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
#' @title Create flexible inline plots with customizable styling
#' @description
#' Creates versatile inline plots that can adapt to different data types and styling
#' configurations, supporting both data frames and vectors with extensive customization.
#' @param x Numeric vector or data frame. Data for plotting
#' @param y Numeric vector. Y values if x is a vector. Default NULL
#' @param mapping Named list. Aesthetic mappings. Default list(x = "x", y = "y")
#' @param style String. Predefined style name to load. Default NULL
#' @param xlim Numeric vector. X-axis limits. Default NULL
#' @param ylim Numeric vector. Y-axis limits. Default NULL
#' @param height Numeric. Plot height. Default 0.8
#' @param width Numeric. Plot width. Default 8
#' @param scaling Numeric. SVG scaling factor. Default 1
#' @param theme ggplot2 theme function. Default ggplot2::theme_void
#' @param coord_flip Logical. Whether to flip coordinates. Default FALSE
#' @param hide_title Logical. Whether to hide plot title. Default TRUE
#' @param hide_legend Logical. Whether to hide legend. Default TRUE
#' @param hide_x_axis Logical. Whether to hide x-axis. Default TRUE
#' @param hide_y_axis Logical. Whether to hide y-axis. Default TRUE
#' @param fill String. Fill color for plot elements
#' @param expansion_y Numeric. Y-axis expansion factor. Default 0
#' @param add_download_button Logical. Whether to add download functionality. Default FALSE
#' @param ... Additional arguments passed to plot styling
#' @return A NightowlPlots object containing the customized inline plot
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
#' @title Create inline histogram plots for distribution visualization
#' @description
#' Creates compact histogram plots for showing data distributions within tables,
#' using predefined styling optimized for inline display.
#' @param x Numeric vector. Data for histogram creation
#' @param mapping Named list. Aesthetic mappings. Default list(x = "x", y = NULL)
#' @param ... Additional arguments passed to add_inline_plot()
#' @return A NightowlPlots object containing the inline histogram
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
#' @title Create inline point range plots with confidence intervals
#' @description
#' Creates compact point range plots showing point estimates with confidence intervals,
#' suitable for embedding in tables to display statistical summaries.
#' @param x Numeric vector or data frame. Data for point range calculation
#' @param fun_data Function. Statistical function for calculating point and range. Default NULL
#' @param mapping Named list. Aesthetic mappings. Default list(x = "y", xmin = "ymin", xmax = "ymax", y = NULL)
#' @param ... Additional arguments passed to add_inline_plot()
#' @return A NightowlPlots object containing the inline point range plot
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
#' @title Create violin plots with summary statistics overlay
#' @description
#' Creates violin plots showing data distribution density with overlaid
#' summary statistics, suitable for compact visualization of numeric distributions.
#' @param x Numeric vector. Data for violin plot creation
#' @param ylim Numeric vector. Y-axis limits. Default NULL
#' @param height Numeric. Plot height. Default 0.3
#' @param width Numeric. Plot width. Default 2.5
#' @param scaling Numeric. SVG scaling factor. Default 1
#' @param theme ggplot2 theme function. Default ggplot2::theme_void
#' @param hide_legend Logical. Whether to hide legend. Default TRUE
#' @param hide_x_axis Logical. Whether to hide x-axis. Default TRUE
#' @param hide_y_axis Logical. Whether to hide y-axis. Default TRUE
#' @param fill String. Fill color for violin. Default "#B9B9B8"
#' @param expansion_y Numeric. Y-axis expansion factor. Default 10
#' @param add_download_button Logical. Whether to add download functionality. Default FALSE
#' @param fun.data Function. Summary statistic function. Default ggplot2::mean_cl_boot
#' @return A NightowlPlots object containing the violin plot
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
#' @title Create density plots for continuous data visualization
#' @description
#' Creates density plots showing the probability density of continuous data,
#' with customizable styling and summary statistics overlay.
#' @param x Numeric vector. Data for density plot creation
#' @param ylim Numeric vector. Y-axis limits. Default NULL
#' @param height Numeric. Plot height. Default 0.8
#' @param width Numeric. Plot width. Default 8
#' @param scaling Numeric. SVG scaling factor. Default 1
#' @param theme ggplot2 theme function. Default ggplot2::theme_void
#' @param hide_x_axis Logical. Whether to hide x-axis. Default TRUE
#' @param hide_y_axis Logical. Whether to hide y-axis. Default TRUE
#' @param fill String. Fill color for density curve
#' @param expansion_y Numeric. Y-axis expansion factor. Default 0
#' @param add_download_button Logical. Whether to add download functionality. Default FALSE
#' @return An SVG string containing the rendered density plot
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
#' @title Create basic histogram plots for data distribution
#' @description
#' Creates simple histogram plots showing the frequency distribution of numeric data
#' with customizable binning and optional axis scaling.
#' @param x Numeric vector. Data for histogram creation
#' @param height Numeric. Plot height. Default 0.8
#' @param binwidth Numeric. Width of histogram bins. Default NULL (automatic)
#' @param xlim Numeric vector. X-axis limits. Default NULL
#' @param add_scale Logical. Whether to show axis scales. Default FALSE
#' @return An SVG string containing the rendered histogram
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
#' @title Add axis scales to NightowlPlots objects
#' @description
#' Adds axis scale information as a separate row to tables containing NightowlPlots,
#' providing reference scales for embedded visualizations.
#' @param obj A data frame containing NightowlPlots objects in columns
#' @param text_size Numeric. Relative size of axis text. Default 1.5
#' @param line_size Numeric. Relative thickness of axis lines. Default 1
#' @param legend_position String. Position of legend. Default "none"
#' @return A data frame with an additional scale row appended
#' @export
add_scale <- function(obj,
                      text_size = 1.5,
                      line_size = 1,
                      legend_position = "none") {
  obj <- dplyr::ungroup(obj)
  columns <- purrr::imap(obj, ~ if (nightowl::is_NightowlPlots(.x)) .y else NULL) %>%
    purrr::compact()
  # Get options_svg from first valid column
  options_svg <- if (length(columns) > 0) {
    .obj <- obj[[columns[1]]]
    .options_svg <- .obj[[1]]$options_svg
    .options_svg$height <- 0.3
    .options_svg
  } else {
    NULL
  }
  
  ggs <- purrr::map(columns, function(.column) {
    .obj <- obj[[.column]]
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
