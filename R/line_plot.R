#' Line plot
#' @export
line_plot <- function(DATA,
                      x,
                      y,
                      fill = NULL,
                      color = NULL,
                      id = "USUBJID",
                      add_lines = T,
                      lines_alpha = 0.5,
                      lines_size = 1,
                      add_points = F,
                      facet_row = NULL,
                      facet_col = NULL,
                      facet_label_width = 20,
                      method_smooth = NULL,
                      color_smooth = "black",
                      xlim = NULL,
                      ylim = NULL,
                      title = NULL,
                      ylab = NULL,
                      axis.text.x.angle = 45,
                      show_se = T,
                      log_y = F,
                      add_ribbon = F,
                      add_whiskers = T,
                      theme = picasso::theme_picasso,
                      palette_discrete = picasso::roche_palette_discrete(1),
                      dodge = 0.2,
                      points_position = "identity",
                      summarise_y = NULL,
                      scales = "free_y",
                      plot_height = NULL, # Drity hack
                      plot_width = NULL, # Dirty hack
                      ...) {
  if (!is.null(fill) && is.null(color)) color <- fill
  # Add text wraping for facets
  DATA <- DATA %>%
    dplyr::mutate_at(
      c(facet_row, facet_col),
      function(x) stringr::str_wrap(x, width = facet_label_width)
    )
  # Drop missing values
  DATA <- DATA %>%
    dplyr::filter_at(c(x, y), function(x) !is.na(x)) %>%
    droplevels() %>%
    tibble::rowid_to_column("INDEX")
  # Convert Characters to factors
  DATA <- DATA %>%
    dplyr::mutate_if(is.character, factor)
  # Aggregate values if there are multiple present per x
  if (!is.null(summarise_y)) {
    DATA %>%
      dplyr::group_by_at(c(x, id, facet_col, facet_row)) %>%
      dplyr::group_split() %>%
      purrr::map_df(function(.data) {
        .value <- do.call(summarise_y, list(.data[[y]]))
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
  # This was difficult, fist store parameters in list,
  # Convert to symbols
  # drop the onses which are null, call aes_ function (CAVE: ecex)
  # also think of other places where params is used, e.g. params$id
  params <- list(
    x = x,
    y = y,
    fill = fill,
    color = color,
    group = fill,
    id = id
  ) %>%
    purrr::compact() %>%
    purrr::map(~ rlang::sym(.x))
  f <- ggplot2::aes_
  .aes <- rlang::exec(.fn = "f", !!!params)
  g <- DATA %>%
    ggplot2::ggplot(.aes)
  # Layers ----
  ## Lines
  if (add_lines) {
    if (is.null(summarise_y)) {
      g <- g + ggplot2::geom_line(
        ggplot2::aes_(group = params$id),
        alpha = lines_alpha,
        size = lines_size
      )
    } else {
      g <- g + ggplot2::stat_summary(
        ggplot2::aes_(group = params$id),
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
      line_mapping <- ggplot2::aes_(lty = params$fill)
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
  # AXIS ----
  # if (is.factor(DATA[[x]]) | is.character(DATA[[x]])) {
  #   g <- g + ggplot2::scale_x_discrete(expand = ggplot2::expansion(add = 0.2))
  # } else {
  #   g <- g + ggplot2::scale_x_continuous(expand = ggplot2::expansion(add = 0.2))
  # }
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
  g <- g + ggplot2::discrete_scale("fill", "roche", palette_discrete, ...)
  g <- g + ggplot2::discrete_scale("color", "roche", palette_discrete, ...)
  if (!is.null(color)) {
    g <-
      g + ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(
        size = 2, color = palette_discrete(length(unique(DATA[[color]])))
      )))
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
    legend.position = "top",
    legend.key.width = ggplot2::unit(2, "cm"),
    plot.margin = ggplot2::margin(1, 1, 1, 1, "cm")
  )
  if (is.factor(DATA[[x]])) {
    g <- g + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = axis.text.x.angle)) +
      ggplot2::xlab("")
  } else {
    g <- g + ggplot2::xlab(x)
  }
  # Title
  if (!is.null(title)) {
    if (title == "auto") {
      auto_title <- glue::glue("{x} vs. {y}")
      g <- g + ggplot2::ggtitle(auto_title)
    } else {
      g <- g + ggplot2::ggtitle(title)
    }
  }
  if (!is.null(ylab)) {
    g <- g + ggplot2::ylab(ylab)
  }
  # Finishing up
  return(g)
}
