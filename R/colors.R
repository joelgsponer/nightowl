#' Nightowl Color Palette System
#'
#' Internal color palette system to replace external picasso dependency
#' Based on Roche color scheme from picasso package
#'
#' @param color Color name (blue, red, green, etc.) or NULL for all colors
#' @param alpha Transparency value (0-1)
#' @return Character vector of color hex codes or single color
#' @export
nightowl_colors <- function(color = NULL, alpha = 1) {
  # Roche color palette (RGB values from picasso::roche_colors_rgb())
  colors_rgb <- list(
    blue = c(51, 108, 190),
    apple = c(238, 154, 54),
    green = c(48, 167, 106),
    purple = c(124, 64, 157),
    red = c(226, 38, 70),
    yellow = c(245, 200, 0),
    violet = c(109, 74, 164),
    lightblue = c(85, 135, 204),
    grey = c(172, 173, 177),
    black = c(35, 31, 32)
  )
  
  # Convert RGB to hex
  colors_hex <- purrr::map_chr(colors_rgb, ~ sprintf("#%02X%02X%02X", .x[1], .x[2], .x[3]))
  
  if (is.null(color)) {
    if (alpha < 1) {
      return(purrr::map_chr(colors_hex, ~ ggplot2::alpha(.x, alpha = alpha)))
    }
    return(colors_hex)
  } else if (color == "white") {
    return(ggplot2::alpha("#FFFFFF", alpha = alpha))
  } else if (color %in% names(colors_hex)) {
    result <- colors_hex[[color]]
    if (alpha < 1) {
      result <- ggplot2::alpha(result, alpha = alpha)
    }
    return(result)
  } else {
    stop(paste("Color", color, "not found. Available colors:", paste(names(colors_hex), collapse = ", ")))
  }
}

#' Get a single nightowl color (compatible with picasso::roche_color)
#'
#' @param color Color name (blue, red, green, etc.)
#' @param alpha Transparency value (0-1)
#' @return Single color hex code
#' @export
nightowl_color <- function(color = "blue", alpha = 1) {
  nightowl_colors(color = color, alpha = alpha)
}

#' Create discrete color palette for ggplot2
#'
#' @param ... Additional arguments passed to ggplot2::scale_color_manual
#' @return ggplot2 scale object
#' @export
nightowl_palette_discrete <- function(...) {
  ggplot2::scale_color_manual(values = nightowl_colors(), ...)
}