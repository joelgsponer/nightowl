# ===============================================================================
#' Function for the lower (diagnonal) triangle of the correlation matrix
#' @export
lowerFn <- function(data, mapping, method = "lm", ...) {
  ggplot2::ggplot(data = data, mapping = mapping) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = method, color = picasso::roche_color("blue"), ...) +
    picasso::theme_picasso()
}
# ===============================================================================
#' Function for the upper (diagnonal) triangle of the correlation matrix
#' @export
upperFn <- function(data, mapping, method = "lm", ...) {
  res <- dplyr::select(data, !!!mapping) %>% cor(use = "complete.obs")
  res <- res[1, 2]
  if (is.na(res)) {
    peacock::error("Computation failed.")
    print(res)
    msg <- "Computation failed"
    return(ggplot2::ggplot() +
      ggplot2::geom_text(ggplot2::aes(
        x = 1,
        y = 1, label = msg
      ), col = "red") +
      ggplot2::theme_void())
  }
  if (res == 0) col <- picasso::roche_color("black", alpha = 0.1)
  if (res < 0) col <- picasso::roche_color("red", alpha = 1)
  if (res > 0) col <- picasso::roche_color("green", alpha = 1)
  p <- ggplot2::ggplot(data = data, mapping = mapping) +
    ggplot2::geom_smooth(
      method = method,
      color = "black",
      size = 0.1,
      fill = col,
      alpha = log10(abs(res * 10)),
      ...
    ) +
    ggpmisc::stat_correlation(color = picasso::roche_color("black")) +
    ggplot2::theme_void()
  p
}
# ===============================================================================
#' Function for the diagnonal correlation matrix
#' @export
diagFn <- function(data, mapping, method = "lm", ...) {
  p <- ggplot2::ggplot(data = data, mapping = mapping) +
    ggplot2::geom_density(fill = picasso::roche_color("blue"), alpha = 0.4) +
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
  #*******************************************************************************
  # Drop columns that are not needed
  DATA <- DATA %>%
    dplyr::ungroup() %>%
    dplyr::select_at(unique(unlist(unname(mapping))))
  #*******************************************************************************
  # Transformations
  if (!is.null(transform)) {
    purrr::iwalk(transform, function(.f, .var) {
      .f <- match.fun(.f)
      .var <- mapping[[.var]]
      DATA <<- DATA %>%
        dplyr::mutate(!!rlang::sym(.var) := .f(!!rlang::sym(.var)))
    })
  }
  #*******************************************************************************
  # Spread data
  DATA <- nightowl::spread_data(DATA, mapping$key, mapping$value)
  # Data preparation
  DATA <- nightowl::text_wraping(DATA, width = label_width)
  #*******************************************************************************
  # Setup Plot
  .aes <- mapping[waRRior::pop(names(mapping), c("key", "value", "id"))] %>%
    nightowl:::aes()
  g <- GGally::ggpairs(
    mapping = .aes,
    data = DATA,
    columns = which(!names(DATA) %in% c(mapping$id, mapping$fill, mapping$color, mapping$group)),
    lower = list(continuous = GGally::wrap(nightowl::lowerFn, method = "lm")),
    diag = list(continuous = GGally::wrap(nightowl::diagFn)),
    upper = list(continuous = GGally::wrap(nightowl::upperFn))
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
