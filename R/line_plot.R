#' Line plot
#' @export
line_plot <- function(DATA, x, y, fill, color,
                      id = "USUBJID",
                      y_unit = "AVALU",
                      title_column = "PARAM",
                      add_lines = T,
                      lines_alpha = 0.5,
                      lines_size = 1,
                      add_points = F,
                      facet_row = NULL,
                      facet_col = NULL,
                      method_smooth = NULL,
                      color_smooth = "black",
                      xlim = NULL,
                      ylim = NULL,
                      title = NULL,
                      ylab = NULL,
                      show_se = T,
                      log_y = F,
                      add_theme = T,
                      add_ribbon = F,
                      add_whiskers = T,
                      theme = ggplot2::theme_light,
                      dodge = 0.2,
                      points_position = "identity",
                      fun.y = NULL,
                      scales = "free_y") {
  # Genrate titles and axis label
  y_unit <- unique(DATA[[y_unit]])
  auto_title <- unique(DATA[[title_column]])
  # Add text wraping for facets
  DATA <- DATA %>%
    dplyr::mutate_at(c(facet_row, facet_col), function(x) stringr::str_wrap(x, width = 20))
  # Drop missing values
  DATA <- DATA %>%
    dplyr::filter_at(c(x, y), function(x) !is.na(x)) %>%
    droplevels() %>%
    tibble::rowid_to_column("INDEX")
  # Aggregate values if there are multiple present per x
  if (!is.null(fun.y)) {
    DATA %>%
      dplyr::group_by_at(c(x, id)) %>%
      dplyr::group_split() %>%
      purrr::map_df(function(.data) {
        .value <- do.call(fun.y, list(.data[[y]]))
        .data %>%
          dplyr::filter_at(y, function(x) x == .value) %>%
          head(1) ->
        .res
        if (nrow(.res) < 1) {
          .data %>%
            head(1) %>%
            dplyr::mutate(AVAL = .value) ->
          .res
        }
        return(.res)
      }) ->
    DATA
  }
  # Setup ggplot
  g <- DATA %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = !!rlang::sym(x), # as.numeric(!!rlang::sym(x)),
        y = !!rlang::sym(y),
        fill = !!rlang::sym(fill),
        color = !!rlang::sym(color),
        group = !!rlang::sym(fill),
      )
    )
  # Layers ----
  ## Lines
  if (add_lines) {
    if (is.null(fun.y)) {
      g <- g + ggplot2::geom_line(
        ggplot2::aes(group = !!rlang::sym(id)),
        alpha = lines_alpha,
        size = lines_size
      )
    } else {
      g <- g + ggplot2::stat_summary(
        ggplot2::aes(group = !!rlang::sym(id)),
        fun.y = fun.y,
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
    if (fill %in% c(facet_row, facet_col)) {
      line_mapping <- ggplot2::aes()
    } else {
      line_mapping <- ggplot2::aes(lty = !!rlang::sym(fill))
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
        g + waRRior::geom_hillow_median(
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
  # Facetting ----
  if (!is.null(facet_col) | !is.null(facet_row)) {
    if (is.null(facet_col)) facet_col <- "."
    if (is.null(facet_row)) facet_row <- "."
    g <- g + ggplot2::facet_grid(
      as.formula(
        paste(
          paste(facet_row, collapse = "+"),
          "~",
          paste(facet_col, collapse = "+")
        )
      ),
      scales = scales,
      labeller = ggplot2::label_both
    )
  }
  # AXIS ----
  if (is.factor(DATA[[x]]) | is.character(DATA[[x]])) {
    g <- g + ggplot2::scale_x_discrete(expand = ggplot2::expand_scale(add = 0.2))
  } else {
    g <- g + ggplot2::scale_x_continuous(expand = ggplot2::expand_scale(add = 0.2))
  }
  if (log_y) {
    g <- g + ggplot2::scale_y_log10()
  }
  if (!is.null(ylim)) {
    g <- g + ggplot2::ylim(ylim[1], ylim[2])
  }
  if (!is.null(xlim)) {
    g <- g + ggplot2::xlim(xlim[1], xlim[2])
  }
  # Theming and colors ----
  # if (!is.null(COLORS)) {
  #  g <- g + scale_color_manual(values = get_COLORS(color, COLORS))
  #  g <- g + scale_fill_manual(values = get_COLORS(fill, COLORS))
  #  g <-
  #    g + guides(colour = guide_legend(override.aes = list(
  #      size = 2, color = get_COLORS(color, COLORS)[1:length(unique(DATA[[color]]))]
  #    )))
  # }
  if (add_theme) {
    g <- g + theme()
    g <- g + ggplot2::theme(legend.position = "top", legend.key.width = ggplot2::unit(2, "cm"))
  }
  if (is.factor(DATA[[x]])) {
    g <- g + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
      xlab("")
  } else {
    g <- g + ggplot2::xlab(x)
  }
  if (!is.null(title)) {
    if (ylab == "auto") {
      g <- g + ggplot2::ggtitle(auto_title)
    } else {
      g <- g + ggplot2::ggtitle(title)
    }
  }
  if (!is.null(ylab)) {
    if (ylab == "auto") {
      g <- g + ggplot2::ylab(y_unit)
    } else {
      g <- g + ggplot2::ylab(ylab)
    }
  }
  # Finishing up
  return(list(
    plot = g,
    data = DATA
  ))
}
