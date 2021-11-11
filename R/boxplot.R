# ===============================================================================
#' Boxplot
#' @export
boxplot <- function(DATA,
                    x,
                    y,
                    id,
                    group = NULL,
                    fill = NULL,
                    color = NULL,
                    size = NULL,
                    shape = NULL,
                    facet_col = NULL,
                    facet_row = NULL,
                    scales = "free_y",
                    title = NULL,
                    xlab = NULL,
                    ylab = NULL,
                    xlim = NULL,
                    ylim = NULL,
                    label_width = 20,
                    axis_text_x_angle = 45,
                    axis_text_x_hjust = 1,
                    axis_text_x_vjust = 1,
                    legend_position = "bottom",
                    log_x = F,
                    log_y = F,
                    theme = picasso::theme_picasso,
                    palette_discrete = picasso::roche_palette_discrete(1),
                    remove_missing = T,
                    plot_height = NULL, # Drity hack
                    plot_width = NULL, # Dirty hack
                    # Specific
                    add_boxplot = F,
                    add_violin = T,
                    add_points = F,
                    points_color = picasso::roche_colors("black"),
                    points_alpha = 0.5,
                    points_position = "identity",
                    points_size = 0.5,
                    points_stroke = 1,
                    dodge = 0.2,
                    add_lines = T,
                    lines_alpha = 0.5,
                    lines_size = 1,
                    add_smooth = NULL,
                    color_smooth = "black",
                    add_ribbon = F,
                    add_whiskers = T,
                    show_se = T,
                    ...) {
  #*******************************************************************************
  # Parameters
  if (is.null(group)) group <- x
  if (is.null(fill)) fill <- group
  if (is.null(color)) color <- fill
  # Drop columns that are not needed
  DATA <- DATA %>%
    dplyr::select_at(c(x, y, color, fill, facet_row, facet_col))
  #*******************************************************************************
  # Drop missing values 
  DATA <- nightowl::prepare_data_for_plotting(DATA, remove_missing = remove_missing)
  # Data preparation
  DATA <- nightowl::add_text_wraping(DATA, width = label_width)
  #*******************************************************************************
  # Setup Plot
  .aes <- list(
    x = x,
    y = y,
    fill = fill,
    color = color,
    group = fill
  )
  g <- nightowl:::ggplot(DATA, .aes)
  #*******************************************************************************
  # Add Violin
  if (add_violin) {
    g <- g + ggplot2::geom_violin(
      position = ggplot2::position_dodge(dodge, preserve = "total"),
      width = 1, alpha = 0.5
    )
  }
  #*******************************************************************************
  # Add Boxplot
  if (add_boxplot) {
    if (add_violin) {
      g <- g + ggplot2::geom_boxplot(
        position = ggplot2::position_dodge(dodge, preserve = "total"),
        outlier.shape = NA,
        coef = 0,
        alpha = 1,
        width = 0.3,
        color = "black"
      )
    } else {
      g <- g + ggplot2::geom_boxplot(
        outlier.shape = NA,
        position = ggplot2::position_dodge(dodge, preserve = "total"),
        width = 0.3,
        color = "black"
      )
    }
  }
  #*******************************************************************************
  # Add points
  if (add_points) {
    g <- g + ggplot2::geom_dotplot(
      binaxis = "y",
      stackdir = "center",
      color = points_color,
      alpha = points_alpha,
      stroke = points_stroke,
      dotsize = points_size,
      position = ggplot2::position_dodge(dodge, preserve = "total"),
    )
  }
  #*******************************************************************************
  ## Smooth
  if (!is.null(add_smooth)) {
    attributes(g)$caption <- c(
      attributes(g)$caption,
      glue::glue("Method for trendline: '{add_smooth}'")
    )
    # Fix colors
    if (!is.null(fill) && fill %in% c(facet_row, facet_col)) {
      line_mapping <- ggplot2::aes()
    } else {
      line_mapping <- ggplot2::aes_(lty = rlang::sym(.aes$fill))
    }
    # Add smooth
    if (add_smooth == "mean") {
      g <- g + nightowl::geom_bootstrap_mean(
        color = color_smooth,
        mapping = line_mapping,
        add_ribbon = add_ribbon,
        add_whiskers = add_whiskers,
        add_points = add_whiskers,
        position = ggplot2::position_dodge(width = dodge, preserve = "total")
      )
    } else if (add_smooth == "median") {
      g <-
        g + nightowl::geom_hillow_median(
          color = color_smooth,
          mapping = line_mapping,
          add_ribbon = add_ribbon,
          add_whiskers = add_whiskers,
          add_points = add_whiskers,
          position = ggplot2::position_dodge(width = dodge, preserve = "total")
        )
    } else {
      g <-
        g + ggplot2::geom_smooth(
          mapping = line_mapping,
          color = color_smooth,
          method = add_smooth,
          se = show_se,
          position = ggplot2::position_dodge(width = dodge)
        )
    }
  }
  #*******************************************************************************
  # Facetting ----
  g <- nightowl::apply_facetting(g, facet_col, facet_row, scales)
  #*******************************************************************************
  # Colors and theming
  g <- nightowl::apply_colors(g, DATA, fill, color)
  # Add Theme
  g <- nightowl::apply_theme(g, theme)
  #*******************************************************************************
  # Annotation
  ## Labels
  g <- nightowl::apply_annotation(
    g,
    x,
    y,
    title,
    xlab,
    ylab,
    axis_text_x,
    axis_text_x_angle,
    axis_text_x_hjust,
    axis_text_x_vjust,
    legend_position
  )
  #*******************************************************************************
  # Axis
  g <- nightowl::apply_axis(g, log_x, log_y, xlim, ylim)
  #*******************************************************************************
  # Finishing up
  return(g)
}
# ===============================================================================
