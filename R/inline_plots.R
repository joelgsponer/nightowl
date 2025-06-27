# =================================================
#' @title Create Inline Forest Plot for Table Embedding
#' @description
#' Creates a compact forest plot suitable for embedding within tables or reports.
#' Displays point estimates with confidence intervals, optional reference lines,
#' and automatic truncation indicators for values outside specified limits.
#' @param x Numeric value for the point estimate
#' @param xmin Numeric value for the lower confidence interval bound
#' @param xmax Numeric value for the upper confidence interval bound
#' @param xlim Numeric vector of length 2 specifying x-axis limits (default: NULL)
#' @param xintercept Numeric value for vertical reference line position (default: NULL)
#' @param xlab Character string for x-axis label (default: NULL)
#' @param ylab Character string for y-axis label (default: NULL)
#' @param hide_y_axis Logical indicating whether to hide y-axis (default: TRUE)
#' @param hide_x_axis Logical indicating whether to hide x-axis (default: TRUE)
#' @param hide_legend Logical indicating whether to hide legend (default: TRUE)
#' @param height Numeric value for plot height in inches (default: 0.3)
#' @param width Numeric value for plot width in inches (default: 3)
#' @param scaling Numeric scaling factor for plot elements (default: 0.8)
#' @param shape Numeric shape code for point markers (default: 15)
#' @param size Numeric size for point markers (default: 4.5)
#' @param alpha Numeric transparency value (default: 0.8)
#' @param breaks Numeric vector of axis break positions (default: seq(-10, 10, 0.5))
#' @param theme ggplot2 theme function (default: ggplot2::theme_void)
#' @return A NightowlPlots object containing the rendered inline forest plot
#' @export
#' @examples
#' # Basic inline forest plot
#' add_inline_forestplot(x = 1.2, xmin = 0.8, xmax = 1.6)
#' 
#' # Forest plot with reference line and custom limits
#' add_inline_forestplot(x = 0.9, xmin = 0.6, xmax = 1.3, 
#'                      xintercept = 1.0, xlim = c(0.5, 2.0))
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
#' @title Create Inline Stacked Bar Plot for Categorical Data
#' @description
#' Creates a compact horizontal stacked bar plot showing the proportional distribution
#' of categorical variables. Automatically handles factor conversion and missing value
#' representation, making it ideal for embedding in summary tables.
#' @param x Vector of categorical data to visualize
#' @param height Numeric value for plot height in inches (default: 0.3)
#' @param width Numeric value for plot width in inches (default: 2.5)
#' @param scaling Numeric scaling factor for plot elements (default: 1)
#' @param colors Function to generate color palette (default: NightowlOptions$get_colors)
#' @return A NightowlPlots object containing the rendered inline bar plot
#' @export
#' @examples
#' # Basic categorical bar plot
#' add_barplot(c("A", "B", "A", "C", "B", "A"))
#' 
#' # Bar plot with missing values
#' data_with_na <- c("Treatment", "Control", NA, "Treatment", "Control")
#' add_barplot(data_with_na)
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
#' @title Create Forest Plot from Raw Data with Statistical Summary
#' @description
#' Creates a forest plot by first applying a statistical summary function to raw data,
#' then rendering the result as an inline forest plot. Commonly used for displaying
#' confidence intervals derived from bootstrap or other statistical methods.
#' @param x Numeric vector of raw data values
#' @param fun_data Function to compute statistical summary (default: ggplot2::mean_cl_boot)
#' @param xintercept Numeric value for vertical reference line position (default: NULL)
#' @param xlim Numeric vector of length 2 specifying x-axis limits (default: NULL)
#' @return A forest plot object with statistical summary applied to the input data
#' @export
#' @examples
#' # Forest plot with bootstrap confidence intervals
#' add_forestplot(rnorm(100, mean = 1.2, sd = 0.3))
#' 
#' # Custom statistical summary function
#' add_forestplot(rnorm(50), fun_data = ggplot2::mean_cl_normal, xintercept = 0)
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
#' @title Create General Purpose Inline Plot with Flexible Styling
#' @description
#' Creates a flexible inline plot that can be customized using YAML style templates or
#' direct parameter specification. Supports various plot types and is the foundation
#' for other specialized inline plotting functions in the package.
#' @param x Data frame or numeric vector containing plot data
#' @param y Numeric vector for y-axis data when x is numeric (default: NULL)
#' @param mapping Named list specifying aesthetic mappings (default: list(x = "x", y = "y"))
#' @param style Character string specifying style template name (default: NULL)
#' @param xlim Numeric vector of length 2 for x-axis limits (default: NULL)
#' @param ylim Numeric vector of length 2 for y-axis limits (default: NULL)
#' @param height Numeric value for plot height in inches (default: 0.8)
#' @param width Numeric value for plot width in inches (default: 8)
#' @param scaling Numeric scaling factor for plot elements (default: 1)
#' @param theme ggplot2 theme function (default: ggplot2::theme_void)
#' @param coord_flip Logical indicating whether to flip coordinates (default: FALSE)
#' @param hide_title Logical indicating whether to hide plot title (default: TRUE)
#' @param hide_legend Logical indicating whether to hide legend (default: TRUE)
#' @param hide_x_axis Logical indicating whether to hide x-axis (default: TRUE)
#' @param hide_y_axis Logical indicating whether to hide y-axis (default: TRUE)
#' @param fill Character string or color for fill aesthetic (default: picasso::roche_colors("lightblue"))
#' @param expansion_y Numeric value for y-axis expansion (default: 0)
#' @param add_download_button Logical indicating whether to add download button (default: FALSE)
#' @param ... Additional arguments passed to style configuration
#' @return A NightowlPlots object containing the rendered inline plot
#' @export
#' @examples
#' # Basic scatter plot
#' add_inline_plot(data.frame(x = 1:10, y = rnorm(10)))
#' 
#' # Histogram using style template
#' add_inline_plot(rnorm(100), style = "Inline-Histogram")
#' 
#' # Custom mapping and styling
#' df <- data.frame(values = rnorm(50), groups = rep(c("A", "B"), 25))
#' add_inline_plot(df, mapping = list(x = "values", fill = "groups"))
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
  .p <- nightowl::DeclarativePlot$new(
    data = DATA,
    mapping = mapping,
    layers = .style$layers
  )$plot
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
#' @title Create Inline Histogram for Distribution Visualization
#' @description
#' Creates a compact histogram optimized for inline display within tables or reports.
#' Uses predefined styling optimized for small-scale visualization of data distributions.
#' @param x Numeric vector of data values to plot
#' @param mapping Named list specifying aesthetic mappings (default: list(x = "x", y = NULL))
#' @param ... Additional arguments passed to add_inline_plot function
#' @return A NightowlPlots object containing the rendered inline histogram
#' @export
#' @examples
#' # Basic inline histogram
#' add_inline_histogram(rnorm(100))
#' 
#' # Histogram with custom width and height
#' add_inline_histogram(rexp(200), width = 4, height = 0.5)
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
#' @title Create Inline Point Range Plot with Confidence Intervals
#' @description
#' Creates a compact point range plot showing central tendency with confidence intervals.
#' Can accept either raw data (with statistical summary function) or pre-computed
#' summary statistics for inline display in tables.
#' @param x Numeric vector of data or data frame with pre-computed statistics
#' @param fun_data Function to compute statistical summary when x is raw data (default: NULL)
#' @param mapping Named list specifying aesthetic mappings (default: list(x = "y", xmin = "ymin", xmax = "ymax", y = NULL))
#' @param ... Additional arguments passed to add_inline_plot function
#' @return A NightowlPlots object containing the rendered inline point range plot
#' @export
#' @examples
#' # Point range from raw data
#' add_inline_pointrange(rnorm(50), fun_data = ggplot2::mean_cl_boot)
#' 
#' # Point range from pre-computed statistics
#' stats_df <- data.frame(y = 1.2, ymin = 0.8, ymax = 1.6)
#' add_inline_pointrange(stats_df)
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
#' @title Create Inline Violin Plot for Distribution Visualization
#' @description
#' Creates a compact violin plot showing the distribution shape of numeric data
#' with overlaid summary statistics. Optimized for inline display with minimal
#' visual elements while preserving distributional information.
#' @param x Numeric vector of data values to plot
#' @param ylim Numeric vector of length 2 for y-axis limits (default: NULL)
#' @param height Numeric value for plot height in inches (default: 0.3)
#' @param width Numeric value for plot width in inches (default: 2.5)
#' @param scaling Numeric scaling factor for plot elements (default: 1)
#' @param theme ggplot2 theme function (default: ggplot2::theme_void)
#' @param hide_legend Logical indicating whether to hide legend (default: TRUE)
#' @param hide_x_axis Logical indicating whether to hide x-axis (default: TRUE)
#' @param hide_y_axis Logical indicating whether to hide y-axis (default: TRUE)
#' @param fill Character string or color for violin fill (default: "#B9B9B8")
#' @param expansion_y Numeric value for y-axis expansion factor (default: 10)
#' @param add_download_button Logical indicating whether to add download button (default: FALSE)
#' @param fun.data Function for summary statistics overlay (default: ggplot2::mean_cl_boot)
#' @return A NightowlPlots object containing the rendered inline violin plot
#' @export
#' @examples
#' # Basic inline violin plot
#' add_violin(rnorm(100))
#' 
#' # Violin plot with custom appearance
#' add_violin(rexp(200), fill = "lightblue", fun.data = ggplot2::mean_cl_normal)
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
#' @title Create Inline Density Plot for Distribution Visualization
#' @description
#' Creates a compact density plot using violin geometry to show the distribution
#' of numeric data. Includes summary statistics overlay and is optimized for
#' inline display within tables or reports.
#' @param x Numeric vector of data values to plot
#' @param ylim Numeric vector of length 2 for y-axis limits (default: NULL)
#' @param height Numeric value for plot height in inches (default: 0.8)
#' @param width Numeric value for plot width in inches (default: 8)
#' @param scaling Numeric scaling factor for plot elements (default: 1)
#' @param theme ggplot2 theme function (default: ggplot2::theme_void)
#' @param hide_x_axis Logical indicating whether to hide x-axis (default: TRUE)
#' @param hide_y_axis Logical indicating whether to hide y-axis (default: TRUE)
#' @param fill Character string or color for density fill (default: picasso_colors("lightblue"))
#' @param expansion_y Numeric value for y-axis expansion factor (default: 0)
#' @param add_download_button Logical indicating whether to add download button (default: FALSE)
#' @return HTML object containing the rendered inline density plot
#' @export
#' @examples
#' # Basic inline density plot
#' add_density(rnorm(100))
#' 
#' # Density plot with custom styling
#' add_density(rgamma(200, shape = 2), fill = "coral", width = 6)
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
#' @title Create Inline Histogram with Customizable Binning
#' @description
#' Creates a compact histogram for displaying data distributions with customizable
#' bin width and optional axis scaling. Designed for inline embedding in tables
#' and reports with minimal visual footprint.
#' @param x Numeric vector of data values to plot
#' @param height Numeric value for plot height in inches (default: 0.8)
#' @param binwidth Numeric value for histogram bin width (default: NULL for automatic)
#' @param xlim Numeric vector of length 2 for x-axis limits (default: NULL)
#' @param add_scale Logical indicating whether to display x-axis scale (default: FALSE)
#' @return HTML object containing the rendered inline histogram
#' @export
#' @examples
#' # Basic inline histogram
#' add_histogram(rnorm(100))
#' 
#' # Histogram with custom binning and scale
#' add_histogram(rexp(200), binwidth = 0.5, add_scale = TRUE)
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
#' @title Add Axis Scales to Inline Plot Columns
#' @description
#' Extracts axis information from NightowlPlots objects within data frame columns
#' and creates corresponding scale rows showing axis labels and tick marks.
#' This function is used to add interpretable scales below inline plots in tables.
#' @param obj Data frame containing columns with NightowlPlots objects
#' @param text_size Numeric scaling factor for axis text size (default: 1.5)
#' @param line_size Numeric scaling factor for axis line thickness (default: 1)
#' @param legend_position Character string for legend position (default: "none")
#' @return Data frame with original data plus additional scale rows for plot columns
#' @export
#' @examples
#' # Create data frame with inline plots
#' df <- data.frame(
#'   group = c("A", "B"),
#'   plot_col = c(add_histogram(rnorm(50)), add_histogram(rnorm(50)))
#' )
#' 
#' # Add scales below plots
#' add_scale(df, text_size = 2, line_size = 1.5)
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
