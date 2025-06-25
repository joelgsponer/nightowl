# ===============================================================================
#' Function for the lower (diagnonal) triangle of the correlation matrix
#' @export
lowerFn <- function(data, mapping, method = "lm", ...) {
  if (!is.null(mapping$group)) {
    alpha_points <- 0.3
    alpha_lines <- 0.8
  } else {
    alpha_points <- 1
    alpha_lines <- 1
  }
  ggplot2::ggplot(data = data, mapping = mapping) +
    ggplot2::geom_point(alpha = alpha_points) +
    ggplot2::geom_smooth(method = method, alpha = alpha_lines, se = FALSE, ...) +
    picasso::theme_picasso()
}
# ===============================================================================
#' Function for the upper (diagnonal) triangle of the correlation matrix
#' @export
upperFn <- function(data, mapping, method = "lm", ...) {
  res <- dplyr::select(data, !!mapping$x, !!mapping$y) %>% cor(use = "complete.obs")
  res <- res[1, 2]
  if (!is.null(mapping$group)) {
    show_se <- FALSE
    alpha <- 0
    size <- 0
    l <- waRRior::length_unique(dplyr::pull(data, !!mapping$group))
    vstep <- 1 / l
  } else {
    show_se <- TRUE
    alpha <- log10(abs(res * 10))
    size <- 0.1
    vstep <- 1
  }
  if (is.na(res)) {
    cli::cli_abort("Computation failed.")
    msg <- "Computation failed"
    return(ggplot2::ggplot() +
      ggplot2::geom_text(ggplot2::aes(
        x = 1,
        y = 1,
        label = msg
      ), col = "red", dodge = ggplot2::position_dodge()) +
      ggplot2::theme_void())
  }
  if (res == 0) col <- picasso::roche_color("black", alpha = 0.1)
  if (res < 0) col <- picasso::roche_color("red", alpha = 1)
  if (res > 0) col <- picasso::roche_color("green", alpha = 1)
  p <- ggplot2::ggplot(data = data, mapping = mapping) +
    ggplot2::geom_smooth(
      method = method,
      size = size,
      fill = col,
      alpha = alpha,
      se = show_se,
      ...
    ) +
    ggpmisc::stat_correlation(vstep = vstep) +
    ggplot2::theme_void()
  p
}
# ===============================================================================
#' Function for the diagnonal correlation matrix
#' @export
diagFn <- function(data, mapping, method = "lm", ...) {
  if (!is.null(mapping$group)) {
    fill <- NA
  } else {
    fill <- picasso::roche_color("blue")
  }
  p <- ggplot2::ggplot(data = data, mapping = mapping) +
    ggplot2::geom_density(fill = fill, alpha = 0.4) +
    picasso::theme_picasso()
  p
}
# ===============================================================================
#' ggpairs theme
#' @export
ggpairs_theme <- function(g) {
  g + ggplot2::theme(
    plot.title = ggplot2::element_text(size = 20, face = "bold", hjust = 0, vjust = 2),
    plot.subtitle = ggplot2::element_text(size = 14, hjust = 0, vjust = 2),
    panel.background = ggplot2::element_rect(fill = "white"),
    plot.background = ggplot2::element_rect(fill = "white"),
    panel.border = ggplot2::element_rect(color = "#231F20FF", fill = NA),
    strip.background = ggplot2::element_rect(fill = "white"),
    legend.position = "bottom",
    legend.background = ggplot2::element_rect(fill = "white", colour = "white"),
    legend.key = ggplot2::element_rect(fill = "white"),
    axis.text = ggplot2::element_text(colour = "#231F20FF"),
    strip.text.x = ggplot2::element_text(color = "#231F20FF"),
    strip.text.y = ggplot2::element_text(color = "#231F20FF"),
  )
}
# ===============================================================================
#' Boxplot
#' @export
ggpairs <- function(DATA,
                    mapping = list(
                      key = NULL,
                      y = NULL,
                      group = NULL,
                      fill = NULL,
                      color = NULL,
                      size = NULL,
                      shape = NULL,
                      lty = NULL,
                      id = NULL
                    ),
                    processing = NULL,
                    transform = NULL,
                    annotation = NULL,
                    facets = NULL,
                    label_width = 10,
                    svg = NULL,
                    diagFn = nightowl::diagFn,
                    ...) {
  #*******************************************************************************
  # Parameters
  if (length(mapping$key) > 1) {
    if (!is.null(mapping$value)) {
      rlang::abort("Key/Value spreading can only be used with a single Key argmuent")
    }
  }
  if (!is.null(mapping$value)) {
    if (is.null(mapping$id)) {
      mapping$id <- mapping$key
    }
  }
  if (is.null(mapping$group) && !is.null(mapping$color)) mapping$group <- mapping$color
  if (is.null(mapping$fill) && !is.null(mapping$color)) mapping$fill <- mapping$color
  #*******************************************************************************
  # Drop columns that are not needed
  DATA <- DATA %>%
    dplyr::ungroup() %>%
    dplyr::select_at(unique(unlist(unname(mapping))))
  #*******************************************************************************
  # Transformations
  if (!is.null(transform)) {
    DATA <- purrr::reduce(names(transform), function(DATA, .var_name) {
      .f <- match.fun(transform[[.var_name]])
      .var <- mapping[[.var_name]]
      if (!is.null(.var)) {
        DATA <- DATA %>%
          dplyr::mutate(!!rlang::sym(.var) := .f(!!rlang::sym(.var)))
      }
      return(DATA)
    }, .init = DATA)
  }
  #*******************************************************************************
  # Spread data
  DATA <-
    nightowl::spread_data(DATA, mapping$key, mapping$value)
  # Data preparation
  DATA <- nightowl::text_wraping(DATA, width = label_width)
  #****************************************************************************
  # Setup Plot
  .aes <- mapping[waRRior::pop(names(mapping), c("key", "value", "id"))] %>%
    nightowl:::aes()
  if (!is.null(mapping$color) || !is.null(mapping$fill)) {
    legend <- 1
  } else {
    legend <- NULL
  }
  g <- GGally::ggpairs(
    mapping = .aes,
    data = DATA,
    columns = which(!names(DATA) %in% c(mapping$id, mapping$fill, mapping$color, mapping$group)),
    lower = list(continuous = GGally::wrap(nightowl::lowerFn, method = "lm")),
    diag = list(continuous = GGally::wrap(diagFn)),
    upper = list(continuous = GGally::wrap(nightowl::upperFn)),
    legend = legend
  )
  g <- nightowl::ggpairs_theme(g)
  #*******************************************************************************
  # Create SVG
  if (!is.null(svg)) {
    g <- do.call(nightowl::render_svg, c(list(g = g), svg))
  }
  #*******************************************************************************
  # Finishing up
  return(g)
}
# ===============================================================================
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
this_f <- function(data, mapping, method = "lm", ...) {
  .mapping <- purrr::map(mapping, function(.x) {
    stringr::str_replace_all(as.character(.x)[2], "`", "")
  })

  p <- do.call(nightowl::styled_plot, c(list(DATA = data, style = "Histogram-simple", y = NULL), .mapping))
  as_ggplot(p)
}
# =================================================
