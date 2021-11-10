# ===============================================================================
#' Boxplot
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
                   shape = NULL
                 ),
                 facetting = list(
                   facet_col = NULL,
                   facet_row = NULL,
                   scales = "free_y"
                 ),
                 annotation = list(
                   title = NULL,
                   xlab = NULL,
                   ylab = NULL,
                   xlim = NULL,
                   ylim = NULL,
                   label_width = 20,
                   axis_text_x_angle = 45,
                   axis_text_x_hjust = 1,
                   axis_text_x_vjust = 1,
                   legend_position = "bottom"
                 ),
                 axis = list(
                   log_x = F,
                   log_y = F
                 ),
                 theming = list(
                   theme = picasso::theme_picasso,
                   palette_discrete = picasso::roche_palette_discrete(1)
                 ),
                 points = list(
                   color = picasso::roche_colors("black"),
                   alpha = 0.5,
                   position = "identity",
                   size = 0.5,
                   stroke = 1,
                 ),
                 dotplot = list(
                   color = picasso::roche_colors("black"),
                   alpha = 0.5,
                   position = "identity",
                   size = 0.5,
                   stroke = 1,
                 ),
                 lines = list(
                   alpha = 0.5,
                   size = 0.5,
                 ),
                 boxplot = list(
                   dodge = 0.2
                 ),
                 violin = list(
                   width = 0.3,
                   alpha = 0.5
                 ),
                 smooth = list(
                   method = "lm",
                   color = "black",
                   se = T
                 ),
                 summary = list(
                   method = "mean",
                   ribbon = FALSE,
                   whiskers = FALSE,
                 ),
                 ...) {
  #*******************************************************************************
  # Drop columns that are not needed
  DATA <- DATA %>%
    dplyr::select_at(unlist(unname(mapping)))
  if (any(dim(DATA) == 0)) rlang::abort("No data, check mapping")
  #*******************************************************************************
  # Drop missing values
  DATA <- do.call(nightowl::prepare_data_for_plotting, c(list(DATA = DATA), processing))
  # Data preparation
  DATA <- nightowl::add_text_wraping(DATA, width = annotation$label_width)
  #*******************************************************************************
  # Setup Plot
  g <- nightowl:::ggplot(DATA, mapping)
  #*******************************************************************************
  # Add Violin
  browser()
  if (!is.null(violin)) {
    g <- do.call(nightowl::add_violin, c(list(DATA, g = g, x = mapping$x), violin))
  }
  #*******************************************************************************
  # Add Boxplot
  if (!is.null(violin)) {
    g <- do.call(nightowl::add_boxplot, c(list(DATA, g = g, x = mapping$x), boxplot))
  }
 # #*******************************************************************************
  # # Add points
  # if (add_points) {
  #   g <- g + ggplot2::geom_dotplot(
  #     binaxis = "y",
  #     stackdir = "center",
  #     color = points_color,
  #     alpha = points_alpha,
  #     stroke = points_stroke,
  #     dotsize = points_size,
  #   )
  # }
  # #*******************************************************************************
  # ## Smooth
  # if (!is.null(add_smooth)) {
  #   attributes(g)$caption <- c(
  #     attributes(g)$caption,
  #     glue::glue("Method for trendline: '{add_smooth}'")
  #   )
  #   # Fix colors
  #   # if (!is.null(group) && group %in% c(facet_row, facet_col)) {
  #   #   line_mapping <- ggplot2::aes()
  #   # } else {
  #   #   line_mapping <- ggplot2::aes_(lty = rlang::sym(.aes$group))
  #   # }
  #   # Add smooth
  #   if (add_smooth == "mean") {
  #     g <- g + nightowl::geom_bootstrap_mean(
  #       color = color_smooth,
  #       # mapping = line_mapping,
  #       add_ribbon = add_ribbon,
  #       add_whiskers = add_whiskers,
  #       add_points = add_whiskers,
  #       position = ggplot2::position_dodge(width = dodge, preserve = "total")
  #     )
  #   } else if (add_smooth == "median") {
  #     g <-
  #       g + nightowl::geom_hillow_median(
  #         color = color_smooth,
  #         mapping = line_mapping,
  #         add_ribbon = add_ribbon,
  #         add_whiskers = add_whiskers,
  #         add_points = add_whiskers,
  #         position = ggplot2::position_dodge(width = dodge, preserve = "total")
  #       )
  #   } else {
  #     g <-
  #       g + ggplot2::geom_smooth(
  #         mapping = line_mapping,
  #         color = color_smooth,
  #         method = add_smooth,
  #         se = show_se,
  #         position = ggplot2::position_dodge(width = dodge)
  #       )
  #   }
  # }
  # #*******************************************************************************
  # # Facetting ----
  # g <- nightowl::apply_facetting(g, facet_col, facet_row, scales)
  # #*******************************************************************************
  # # Colors and theming
  # g <- nightowl::apply_colors(g, DATA, fill, color)
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
