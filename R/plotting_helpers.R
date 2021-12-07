# ===============================================================================
#' Wrap text label at a certain width
#' @param width
#' @export
text_wraping <- function(DATA, cols = NULL, width = 20, ...) {
  if (is.null(cols)) cols <- names(DATA)[purrr::map_lgl(DATA, ~ is.factor(.x))]
  purrr::reduce(cols, function(.out, .col) {
    .x <- .out[[.col]]
    levels(.x) <- stringr::str_wrap(levels(.x), width)
    .out[[.col]] <- .x
    .out
  }, .init = DATA)
}
# ===============================================================================
#' Handle missing values
#' @export
prepare_data_for_plotting <- function(DATA, cols = NULL, remove_missing = T, to_factor = TRUE) {
  DATA <- tibble::as_tibble(DATA)
  if (is.null(cols)) cols <- names(DATA)
  if (remove_missing) {
    DATA <- DATA %>%
      dplyr::filter_at(c(cols), function(x) !is.na(x))
  }
  if (to_factor) {
    # Make missing factors explicit
    # Convert Characters to factors
    DATA <- DATA %>%
      dplyr::mutate_if(is.character, factor) %>%
      dplyr::mutate_if(is.factor, forcats::fct_explicit_na) %>%
      droplevels()
  }
  return(DATA)
}
# ===============================================================================
#' Create aes from list
#' Setup ggplot
#' This was difficult, fist store parameters in list,
#' Convert to symbols
#' drop the onses which are null, call aes_ function (CAVE: ecex)
#' also think of other places where params is used, e.g. params$id
#' @export
aes <- function(aes) {
  aes <- aes %>%
    purrr::compact() %>%
    purrr::map(~ rlang::sym(.x))
  f <- ggplot2::aes_
  rlang::exec(.fn = "f", !!!aes)
}
# ===============================================================================
#' Define colors
#' @export
colors <- function(g, DATA, mapping) {
  fill <- mapping$fill
  color <- mapping$color
  if (!is.null(fill) && is.factor(DATA[[fill]])) {
    if (length(unique(DATA[[fill]])) <= 10) {
      g <- g + ggplot2::discrete_scale("fill", "roche", picasso::roche_palette_discrete(1))
    } else {
      g <- g + ggplot2::guides(fill = "none")
      attributes(g)$caption <- c(
        attributes(g)$caption,
        glue::glue("Legend for {fill} not shown (to many values)")
      )
    }
  }
  if (!is.null(color) && is.factor(DATA[[color]])) {
    if (length(unique(DATA[[color]])) <= 10) {
      g <- g + ggplot2::discrete_scale("color", "roche", picasso::roche_palette_discrete(1))
      g <- g + ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(
        size = 2, color = picasso::roche_palette_discrete()(length(unique(DATA[[color]])))
      )))
    } else {
      g <- g + ggplot2::guides(color = "none")
      attributes(g)$caption <- c(
        attributes(g)$caption,
        glue::glue("Legend for {color} not shown (to many values)")
      )
    }
  }
  return(g)
}
# ===============================================================================
#' Apply theme
#' @export
theme <- function(g, theme = "picasso::theme_picasso", ...) {
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
  return(g)
}
# ===============================================================================
#' Apply annotation
#' @export
annotation <- function(g,
                           x,
                           y,
                           title = NULL,
                           xlab = NULL,
                           ylab = NULL,
                           axis_text_x_angle = 45,
                           axis_text_x_hjust = 1,
                           axis_text_x_vjust = 1,
                           legend_position = "bottom",
                           ...) {
  g <- g + ggplot2::theme(
    axis.text.x = ggplot2::element_text(
      angle = axis_text_x_angle,
      hjust = axis_text_x_hjust,
      vjust = axis_text_x_vjust,
    ),
    legend.position = legend_position
  )
  ## X label
  if (!is.null(xlab)) {
    g <- g + ggplot2::xlab(xlab)
  }
  ## Y label
  if (!is.null(ylab)) {
    g <- g + ggplot2::ylab(ylab)
  }
  ## Title
  if (!is.null(title)) {
    g <- g + ggplot2::ggtitle(title)
  } else {
    auto_title <- glue::glue("{x} vs. {y}")
    g <- g + ggplot2::ggtitle(auto_title)
  }
  if (!is.null(attributes(g)$caption)) {
    g <- g + ggplot2::labs(
      caption = paste(unique(attributes(g)$caption), collapse = "\n")
    )
  }
  return(g)
}
# ===============================================================================
#' Apply axis
#' @export
axis <- function(g,
                 log_x = F,
                 log_y = F,
                 xlim = NULL,
                 ylim = NULL,
                 units_x = NULL,
                 units_y = NULL,
                 ...) {
  if (!is.null(units_x)) {
    g$labels$x <- paste0(g$labels$x, "(", units_x, ")")
  }
  if (!is.null(units_y)) {
    g$labels$y <- paste0(g$labels$y, "(", units_y, ")")
  }
  if (log_x) {
    g <- g + ggplot2::scale_x_log10()
    g$labels$x <- glue::glue("log10({g$labels$x})")
  }
  if (log_y) {
    g <- g + ggplot2::scale_y_log10()
    g$labels$y <- glue::glue("log10({g$labels$y})")
  }
  if (!is.null(xlim)) {
    g <- g + ggplot2::coord_cartesian(xlim = xlim)
    attributes(g)$caption <- c(
      attributes(g)$caption,
      glue::glue("Zoom on X Axis: {paste0(xlim, units_x, collapse = ' - ')}")
    )
  }
  if (!is.null(ylim)) {
    g <- g + ggplot2::coord_cartesian(ylim = ylim)
    attributes(g)$caption <- c(
      attributes(g)$caption,
      glue::glue("Zoom on Y Axis: {paste0(ylim, units_y, collapse = ' - ')}")
    )
  }
  return(g)
}
# ===============================================================================
#' Spread data
#' @export
spread_data <- function(DATA, key, value = NULL) {
  if (is.null(value)) {
    return(DATA)
  } else {
    tryCatch({
      DATA %>%
        dplyr::ungroup() %>%
        tidyr::pivot_wider(names_from = key, values_from = value)
    })
  }
}
# ===============================================================================
