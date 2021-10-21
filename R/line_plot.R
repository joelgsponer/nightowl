#' Line plot
#' @export
line_plot <- function(DATA,
                      x,
                      y,
                      fill = NULL,
                      color = NULL,
                      id = "USUBJID",
                      add_points = F,
                      facet_row = NULL,
                      title = NULL,
                      xlab = NULL,
                      ylab = NULL,
                      add_lines = T,
                      lines_alpha = 0.5,
                      lines_size = 1,
                      facet_col = NULL,
                      label_width = 20,
                      method_smooth = NULL,
                      color_smooth = "black",
                      xlim = NULL,
                      ylim = NULL,
                      axis.text.x.angle = 45,
                      axis.text.x.hjust = 1,
                      axis.text.x.vjust = 1,
                      legend.position = "bottom",
                      show_se = T,
                      log_y = F,
                      add_ribbon = F,
                      add_whiskers = T,
                      theme = picasso::theme_picasso,
                      palette_discrete = picasso::roche_palette_discrete(1),
                      dodge = 0.2,
                      points_position = "identity",
                      summarise_y = NULL,
                      remove_missing = T,
                      scales = "free_y",
                      plot_height = NULL, # Drity hack
                      plot_width = NULL, # Dirty hack
                      ...) {
  #*******************************************************************************
  # Parameters
  if (is.null(fill)) fill <- id
  if (!is.null(fill) && is.null(color)) color <- fill
  # Drop columns that are not needed
  DATA <- DATA %>%
    dplyr::select_at(c(x, y, color, fill, facet_row, facet_col, id))
  #*******************************************************************************
  # Drop missing values
  DATA <- nightowl::prepare_data_for_plotting(DATA, remove_missing = remove_missing)
  # Data preparation
  DATA <- nightowl::add_text_wraping(DATA, width = label_width)
  # Aggregate values if there are multiple present per x
  # DATA <- nightowl::aggregate_y_values(DATA, summarise_y, x, y)
  #*******************************************************************************
  # Setup ggplot
  .aes <- list(
    x = x,
    y = y,
    fill = fill,
    color = color,
    group = fill,
    id = id
  )
  g <- nightowl::ggplot(DATA, .aes)
  #*******************************************************************************
  # Layers ----
  ## Lines
  if (add_lines) {
    if (is.null(summarise_y)) {
      g <- g + ggplot2::geom_line(
        ggplot2::aes_(group = rlang::sym(.aes$id)),
        alpha = lines_alpha,
        size = lines_size
      )
    } else {
      g <- g + ggplot2::stat_summary(
        ggplot2::aes_(group = rlang::sym(.aes$id)),
        fun = summarise_y,
        geom = "line",
        alpha = lines_alpha,
        size = lines_size
      )
    }
  }
  ## Points
  if (add_points) {
    g <- g + ggplot2::geom_point(
      position = ggplot2::position_dodge(width = dodge, preserve = "total"),
    )
  }
  ## Smooth
  if (!is.null(method_smooth)) {
    # Fix colors
    if (!is.null(fill) && fill %in% c(facet_row, facet_col)) {
      line_mapping <- ggplot2::aes()
    } else {
      line_mapping <- ggplot2::aes_(lty = rlang::sym(.aes$fill))
    }
    # Add smooth
    if (method_smooth == "mean") {
      g <- g + nightowl::geom_bootstrap_mean(
        color = color_smooth,
        mapping = line_mapping,
        add_ribbon = add_ribbon,
        add_whiskers = add_whiskers,
        add_points = add_whiskers,
        position = ggplot2::position_dodge(width = dodge, preserve = "total")
      )
    } else if (method_smooth == "median") {
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
          method = method_smooth,
          se = show_se,
          position = ggplot2::position_dodge(width = dodge)
        )
    }
  }
  #*******************************************************************************
  # Facetting ----
  if (!is.null(facet_col) | !is.null(facet_row)) {
    if (is.null(facet_col)) facet_col <- "."
    if (is.null(facet_row)) facet_row <- "."
    g <- g + ggplot2::facet_grid(
      as.formula(
        paste(
          paste0("`", facet_row, "`", collapse = "+"),
          "~",
          paste0("`", facet_col, "`", collapse = "+")
        )
      ),
      scales = scales,
      labeller = ggplot2::label_both
    )
  }
  #*******************************************************************************
  # Colors and theming
  if (is.factor(DATA[[fill]])) {
    if (length(unique(DATA[[fill]])) <= 10) {
      g <- g + ggplot2::discrete_scale("fill", "roche", palette_discrete)
      g <- g + ggplot2::discrete_scale("color", "roche", palette_discrete)
    } else {
      legend.position <- "none"
    }
  }
  if (!is.null(color)) {
    if (length(unique(DATA[[color]])) <= 10) {
      g <-
        g + ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(
          size = 2, color = palette_discrete(length(unique(DATA[[color]])))
        )))
    }
  }
  # Add Theme
  if (is.character(theme)) {
    g <- g + eval(parse(text = paste0(theme, "()")))
  } else if (is.function(theme)) {
    g <- g + theme()
  } else {
    rlang::abort("theme has to be either a function name as a string or a function iself")
  }
  # Adjust margins etc,
  g <- g + ggplot2::theme(
    legend.key.width = ggplot2::unit(2, "cm"),
    plot.margin = ggplot2::margin(1, 1, 1, 1, "cm")
  )
  #*******************************************************************************
  # Annotation
  ## Labels
  g <- g + ggplot2::theme(
    axis.text.x = ggplot2::element_text(
      angle = axis.text.x.angle,
      hjust = axis.text.x.hjust,
      vjust = axis.text.x.vjust,
    ),
    legend.position = legend.position
  ) +
    ggplot2::scale_y_continuous(n.breaks = 20)

  ## X label
  if (!is.null(xlab) && xlab != "auto") {
    g <- g + ggplot2::xlab(xlab)
  }
  ## Y label
  if (!is.null(ylab) && ylab != "auto") {
    g <- g + ggplot2::ylab(ylab)
  }
  ## Title
  if (!is.null(title)) {
    if (title == "auto") {
      auto_title <- glue::glue("{x} vs. {y}")
      g <- g + ggplot2::ggtitle(auto_title)
    } else {
      g <- g + ggplot2::ggtitle(title)
    }
  }
  #*******************************************************************************
  # Axis
  if (log_y) g <- g + ggplot2::scale_y_log10()
  if (!is.null(ylim)) {
    g <- g + ggplot2::ylim(ylim[1], ylim[2])
  }
  if (!is.null(xlim)) {
    g <- g + ggplot2::xlim(xlim[1], xlim[2])
  }
  #*******************************************************************************
  # Finishing up
  return(g)
}
