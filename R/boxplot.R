# ===============================================================================
#' Boxplot
#' @importFrom ggplot2 aes mean_cl_boot mean_cl_normal mean_se mean_sdl
#' @export
plot <- function(DATA,
                 processing = list(
                   remove_missing = T
                 ),
                 mapping = list(
                   x = NULL,
                   y = NULL,
                   group = NULL,
                   fill = NULL,
                   color = NULL,
                   size = NULL,
                   shape = NULL,
                   id = NULL
                 ),
                 facetting = NULL,
                 annotation = NULL,
                 axis = NULL,
                 theming = NULL,
                 points = NULL,
                 dotplot = NULL,
                 lines = NULL,
                 boxplot = NULL,
                 violin = NULL,
                 smooth = NULL,
                 summary = NULL,
                 traces = NULL,
                 facets = NULL,
                 dodge = NULL,
                 ...) {
  #*******************************************************************************
  # Dodge override
  if (!is.null(dodge)) {
    if (!is.null(violin)) violin$dodge <- dodge
    if (!is.null(boxplot)) boxplot$dodge <- dodge
    if (!is.null(dotplot)) dotplot$dodge <- dodge
    if (!is.null(summary)) {
      summary <- summary %>%
        purrr::map(function(.x) {
          .x["dodge"] <- dodge
          .x
        })
    }
  }
  # Drop columns that are not needed
  DATA <- DATA %>%
    dplyr::select_at(unlist(unname(mapping)))
  if (any(dim(DATA) == 0)) rlang::abort("No data, check mapping")
  #*******************************************************************************
  # Drop missing values
  DATA <- do.call(nightowl::prepare_data_for_plotting, c(list(DATA = DATA), processing))
  # Data preparation
  DATA <- do.call(nightowl::add_text_wraping, c(list(DATA = DATA), annotation))
  #*******************************************************************************
  # Setup Plot
  g <- nightowl:::ggplot(DATA, mapping) +
    ggplot2::geom_blank(mapping = ggplot2::aes(x = forcats::as_factor(.data[[mapping$x]])))
  #*******************************************************************************
  # Add Violin
  if (!is.null(violin)) {
    g <- do.call(nightowl::add_violin, c(list(g = g, mapping = mapping), violin))
  }
  #*******************************************************************************
  # Add Boxplot
  if (!is.null(boxplot)) {
    g <- do.call(nightowl::add_boxplot, c(list(g = g, mapping = mapping), boxplot))
  }
  #*******************************************************************************
  # Add points
  if (!is.null(dotplot)) {
    g <- do.call(nightowl::add_dotplot, c(list(g = g, mapping = mapping), dotplot))
  }
  #*******************************************************************************
  # Add points r
  if (!is.null(points)) {
    g <- do.call(nightowl::add_points, c(list(g = g, mapping = mapping), points))
  }
  # #*******************************************************************************
  # Add Summary
  if (!is.null(summary)) {
    g <- summary %>%
      purrr::imap(~ {
        c(.x, list(geom = .y))
      }) %>%
      purrr::reduce(function(.out, .in) {
        do.call(nightowl::add_summary, c(list(g = .out, mapping = mapping), .in))
      }, .init = g)
  }
  #*******************************************************************************
  # Add Smooth
  if (!is.null(smooth)) {
    g <- smooth %>%
      purrr::imap(~ {
        c(.x, list(method = .y))
      }) %>%
      purrr::reduce(function(.out, .in) {
        do.call(nightowl::add_smooth, c(list(g = .out, mapping = mapping), .in))
      }, .init = g)
  }
  #*******************************************************************************
  # Add facetting
  if (!is.null(facets)) {
    g <- do.call(nightowl::add_facets, c(list(g = g), facets))
  }

  #*******************************************************************************
  # Add Traces
  if (!is.null(traces)) {
    g <- traces %>%
      purrr::imap(~ {
        c(.x, list(geom = .y))
      }) %>%
      purrr::reduce(function(.out, .in) {
        do.call(nightowl::add_traces, c(list(g = .out, mapping = mapping), .in))
      }, .init = g)
  }

  # #*******************************************************************************
  # Colors and theming
  g <- do.call(nightowl::add_colors, c(list(g = g, DATA = DATA, mapping = mapping)))
  # # Add Theme
  # g <- nightowl::apply_theme(g, theme)
  # #*******************************************************************************
  # # Annotation
  # ## Labels
  # g <- nightowl::apply_annotation(
  #   g,
  #   x,
  #   y,
  #   title,
  #   xlab,
  #   ylab,
  #   axis_text_x,
  #   axis_text_x_angle,
  #   axis_text_x_hjust,
  #   axis_text_x_vjust,
  #   legend_position
  # )
  # #*******************************************************************************
  # # Axis
  # g <- nightowl::apply_axis(g, log_x, log_y, xlim, ylim)
  #*******************************************************************************
  # Finishing up
  return(g)
}
# ===============================================================================
