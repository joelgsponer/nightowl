#===============================================================================
#' Function for the lower (diagnonal) triangle of the correlation matrix
#' @export
lowerFn <- function(data, mapping, method = "lm", ...) {
  ggplot2::ggplot(data = data, mapping = mapping) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = method, color = picasso::roche_color("blue"), ...) +
    picasso::theme_picasso()
}
#===============================================================================
#' Function for the upper (diagnonal) triangle of the correlation matrix
#' @export
upperFn <- function(data, mapping, method = "lm", ...) {
  res <- dplyr::select(data, !!!mapping) %>% cor(., use = "complete.obs")
  res <- res[1, 2]
  if(is.na(res)) {
    peacock::error("Computation failed.")
    print(res)
    msg <- "Computation failed"
    return(ggplot2::ggplot() + ggplot2::geom_text(ggplot2::aes(x = 1,
        y = 1, label = msg), col = "red") + ggplot2::theme_void())
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
      alpha = abs(res) * 0.8,
      ...
    ) +
    ggpmisc::stat_correlation(color = picasso::roche_color("black")) +
    ggplot2::theme_void()
  p
}
#===============================================================================
#' Function for the diagnonal correlation matrix
#' @export
diagFn <- function(data, mapping, method = "lm", ...) {
  p <- ggplot2::ggplot(data = data, mapping = mapping) +
    ggplot2::geom_density(fill = picasso::roche_color("blue"), alpha = 0.4) +
    picasso::theme_picasso()
  p
}
#===============================================================================
#' ggpairs theme
#' @export
ggpairs_theme <- function(g){
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
                    key,
                    value = NULL,
                    id = NULL,
                    group = NULL,
                    fill = NULL,
                    color = NULL,
                    size = NULL,
                    shape = NULL,
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
                    remove_missing = T,
                    ...) {
  #*******************************************************************************
  # Parameters
  if(!is.null(value)){
    if(is.null(id)){
      id <- key
    }
  }
  if (is.null(fill)) fill <- group
  if (is.null(color)) color <- fill
  #*******************************************************************************
  # Drop columns that are not needed
  DATA <- DATA %>%
    dplyr::ungroup() %>%
    dplyr::select_at(unique(c(id, key, value, color, fill)))
  #*******************************************************************************
  # Drop missing values
  DATA <- nightowl::prepare_data_for_plotting(DATA, remove_missing = remove_missing, to_factor = FALSE)
  #*******************************************************************************
  # Spread data
  DATA <- nightowl::spread_data(DATA, key, value)
  # Data preparation
  DATA <- nightowl::add_text_wraping(DATA, width = label_width)
  #*******************************************************************************
  # Setup Plot
  .aes <- list(
      fill = fill,
      color = color,
      group = fill
    )
  .aes <- nightowl:::ggplot(DATA, .aes, only_aes = T)
  g <- GGally::ggpairs(
    mapping = .aes,
    data = DATA,
    columns = which(!names(DATA) %in% c(id, fill, color, group)),
    lower = list(continuous = GGally::wrap(nightowl::lowerFn, method = "lm")),
    diag = list(continuous = GGally::wrap(nightowl::diagFn)),
    upper = list(continuous = GGally::wrap(nightowl::upperFn))
  )
  g <- nightowl::ggpairs_theme(g)
  #*******************************************************************************
  # Finishing up
  return(g)
}
# ===============================================================================
