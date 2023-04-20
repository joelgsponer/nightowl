# =================================================
#' @title
#'  Plot stacked percentage barplot
#' @export
plot_stacked_percentages <- function(DATA,
                                     x,
                                     y,
                                     fill = y,
                                     title = "Missing title",
                                     fill_colors = NULL,
                                     debug = F,
                                     position = "stack",
                                     facet_col = NULL,
                                     facet_row = NULL,
                                     facet_wrap = NULL,
                                     add_labels = T,
                                     add_theme = T,
                                     .theme = ggplot2::theme_minimal,
                                     legend_title = NULL,
                                     dodge = 0,
                                     x_angle = NULL,
                                     strip_x_angle = NULL,
                                     strip_y_angle = NULL,
                                     reverse = F,
                                     data_id_prefix = stringi::stri_reverse(AceOfSpades::random_id()),
                                     onclick = "console.log($(\"[class^=\"hover_svg\"]\"))",
                                     explicit_na = T) {
  # For debugging
  if (debug) browser()
  # Number of Levels
  n_group <- length(unique(DATA[[y]]))
  # Fill colors
  # Filter DATA and calculate
  DATA %>%
    dplyr::mutate_at(y, as.factor) %>%
    dplyr::group_by_at(c(facet_col, facet_row, facet_wrap)) %>%
    dplyr::group_split(.keep = T) %>%
    purrr::map_df(function(.DATA) {
      .DATA %>%
        dplyr::mutate(!!rlang::sym(y) := as.factor(!!rlang::sym(y))) %>%
        {
          if (explicit_na) {
            dplyr::mutate_at(., y, function(x) forcats::fct_explicit_na(x))
          } else {
            dplyr::filter(., !is.na(!!rlang::sym(y)))
          }
        } %>%
        dplyr::group_by_at(c(x, y, facet_col, facet_row, facet_wrap)) %>%
        dplyr::summarise(n = dplyr::n()) %>%
        # mutate(freq = n / sum(n) * 100) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(!!rlang::sym(x)) %>%
        dplyr::group_split() %>%
        purrr::map(.f = function(x) {
          total <- sum(x$n)
          x <- dplyr::mutate(x, p = n / total * 100)
          x <- dplyr::mutate(x, t = cumsum(p))
          x <- dplyr::mutate(x, m = t - (p / 2))
          return(x)
        }) %>%
        dplyr::bind_rows()
    }) ->
  DATA

  if (reverse) {
    DATA <- DATA %>%
      dplyr::mutate_at(y, forcats::fct_rev) %>%
      dplyr::mutate(
        m = 100 - m
      )
  }

  if (all(is.na(DATA[y])) | all(is.na(DATA[x]))) {
    warning("Only missing variables in data.")
    invisible()
  }

  if (length(unique(DATA[[y]])) > length(fill_colors)) fill_colors <- viridis::viridis(length(unique(DATA[[y]])))

  if (reverse) {
    fill_colors <- rev(fill_colors[1:length(unique(DATA[[y]]))])
  }

  if (is.null(legend_title)) legend_title <- y

  DATA <- DATA %>%
    dplyr::mutate(tooltip = paste(.data[[y]], paste0(round(p), "% (", n, ")"))) %>%
    dplyr::mutate(data_id = paste0(data_id_prefix, .data[[y]])) %>%
    dplyr::mutate(onclick = onclick)

  g <- ggplot2::ggplot(DATA, ggplot2::aes_string(
    fill = fill,
    y = "p",
    x = x,
    tooltip = "tooltip",
    data_id = "data_id",
    onclick = "onclick"
  )) +
    ggiraph::geom_col_interactive(color = "black", lwd = 0.2, position = position) +
    ggplot2::scale_fill_manual(name = legend_title, values = fill_colors) +
    ggplot2::ggtitle(title) +
    ggplot2::ylab("%") +
    ggplot2::xlab(x)

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
      )
      # labeller = label_both
    )
  }
  if (!is.null(facet_wrap)) {
    g <- g + ggplot2::facet_wrap(
      as.formula(
        paste(
          "~",
          paste(facet_wrap, collapse = "+")
        )
      ),
      drop = FALSE
    )
  }
  if (add_theme) {
    g <- g +
      .theme() +
      ggplot2::theme(legend.position = "top")
  }
  if (length(unique(DATA[[x]])) > 5) {
    g <- g + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = ifelse(is.null(x_angle), 90, x_angle)))
  } else {
    g <- g + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = ifelse(is.null(x_angle), 0, x_angle)))
  }
  if (!is.null(strip_x_angle)) {
    g <- g + ggplot2::theme(strip.text.x = ggplot2::element_text(angle = strip_x_angle))
  }
  if (!is.null(strip_y_angle)) {
    g <- g + ggplot2::theme(strip.text.y = ggplot2::element_text(angle = strip_y_angle))
  }
  if (add_labels && position != "dodge") {
    g <- g + ggplot2::geom_label(ggplot2::aes(label = paste0(round(p), "% (", n, ")"), y = 100 - m),
      alpha = 0.5,
      position = ggplot2::position_dodge(dodge)
    )
  } else if (add_labels) {
    g <- g + ggtext::geom_richtext(
      ggplot2::aes(
        label = paste0(round(p), "% (", n, ")"),
        fill = NULL,
        color = NULL
      ),
      size = 3,
      fill = NA,
      label.color = NA,
      alpha = 1,
      angle = 90,
      hjust = 0
    )
  }
  return(g)
}
# =================================================
