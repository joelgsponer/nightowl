# ===============================================================================
#' Wrap text label at a certain width
#' @param width 
#' @export
add_text_wraping <- function(DATA, cols = NULL, width) {
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
  if (is.null(cols)) cols <- names(DATA)
  if (remove_missing) {
    DATA <- DATA %>%
      dplyr::filter_at(c(cols), function(x) !is.na(x)) %>%
      dplyr::mutate_if(is.character, factor) %>%
      droplevels()
  } else if (to_factor) {
    # Make missing factors explicit
    # Convert Characters to factors
    DATA <- DATA %>%
      dplyr::mutate_if(is.character, factor) %>%
      dplyr::mutate_if(is.factor, forcats::fct_explicit_na)
  }
  return(DATA)
}
# ===============================================================================
#' Aggregate y values
#' @export
aggregate_y_values <- function(DATA, summarise_y, x, y, cols = NULL) {
  if (is.null(cols)) cols <- waRRior::pop(names(DATA), y)
  if (!is.null(summarise_y)) {
    DATA %>%
      dplyr::group_by_at(cols) %>%
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
      })
  } else {
    return(DATA)
  }
}
# ===============================================================================
#' Create ggplot form list
#' Setup ggplot
#' This was difficult, fist store parameters in list,
#' Convert to symbols
#' drop the onses which are null, call aes_ function (CAVE: ecex)
#' also think of other places where params is used, e.g. params$id
ggplot <- function(DATA, aes, only_aes = F, ...) {
  aes <- aes %>%
    purrr::compact() %>%
    purrr::map(~ rlang::sym(.x))
  f <- ggplot2::aes_
  .aes <- rlang::exec(.fn = "f", !!!aes)
  if(only_aes) return(.aes)
  ggplot2::ggplot(data = DATA, .aes)
}
# ===============================================================================
#' Define colors
#' @export
apply_colors <- function(g, DATA, fill, color) {
  if (is.factor(DATA[[fill]])) {
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
  if (is.factor(DATA[[color]])) {
    if (length(unique(DATA[[fill]])) <= 10) {
      g <- g + ggplot2::discrete_scale("color", "roche", picasso::roche_palette_discrete(1))
    } else {
      g <- g + ggplot2::guides(color = "none")
      attributes(g)$caption <- c(
        attributes(g)$caption,
        glue::glue("Legend for {color} not shown (to many values)")
      )
    }
  }
  if (!is.null(color)) {
    if (length(unique(DATA[[color]])) <= 10) {
      g <-
        g + ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(
          size = 2, color = picasso::roche_palette_discrete()(length(unique(DATA[[color]])))
        )))
    }
  }
  return(g)
}
# ===============================================================================
#' Apply facetting
#' @export
apply_facetting <- function(g, facet_col, facet_row, scales) {
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
  return(g)
}
# ===============================================================================
#' Apply theme
#' @export
apply_theme <- function(g, theme) {
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
apply_annotation <- function(g,
                             x,
                             y,
                             title,
                             xlab,
                             ylab,
                             axis_text_x,
                             axis_text_x_angle,
                             axis_text_x_hjust,
                             axis_text_x_vjust,
                             legend_position,
                             n_breaks_y = 20) {
  g <- g + ggplot2::theme(
    axis.text.x = ggplot2::element_text(
      angle = axis_text_x_angle,
      hjust = axis_text_x_hjust,
      vjust = axis_text_x_vjust,
    ),
    legend.position = legend_position
  ) +
    ggplot2::scale_y_continuous(n.breaks = n_breaks_y)
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
apply_axis <- function(g, log_x, log_y, xlim, ylim) {
  if (log_x) g <- g + ggplot2::scale_x_log10()
  if (log_y) g <- g + ggplot2::scale_y_log10()
  if (!is.null(xlim)) {
    g <- g + ggplot2::coord_cartesian(xlim = xlim)
  }
  if (!is.null(ylim)) {
    g <- g + ggplot2::coord_cartesian(ylim = ylim)
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
#===============================================================================
