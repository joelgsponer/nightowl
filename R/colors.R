#' Nightowl Color Palette
#'
#' @description
#' Internal color palette for nightowl package, replacing picasso::roche_colors()
#'
#' @return Named vector of colors
#' @export
#' @examples
#' nightowl_colors()
#' nightowl_colors()["blue"]
nightowl_colors <- function() {
  c(
    blue      = "#007DBA",   # Primary blue
    red       = "#E40046",   # Primary red
    green     = "#00847A",   # Primary green
    purple    = "#6C1F7D",   # Primary purple
    orange    = "#FF6900",   # Primary orange
    yellow    = "#FFC72C",   # Primary yellow
    black     = "#000000",   # Black
    grey      = "#BEBEBE",   # Grey
    lightblue = "#B3D9F2",   # Light blue
    lightgrey = "#F5F5F5",   # Light grey
    darkgrey  = "#666666",   # Dark grey
    apple     = "#8FD14F",   # Apple green
    pink      = "#FF69B4",   # Pink
    brown     = "#8B4513"    # Brown
  )
}

#' Get a Single Nightowl Color
#'
#' @param name Character string specifying the color name
#' @param alpha Numeric value between 0 and 1 for transparency (default = 1)
#' @return Character string with hex color code (with alpha if specified)
#' @export
#' @examples
#' nightowl_color("blue")
#' nightowl_color("red", alpha = 0.5)
nightowl_color <- function(name, alpha = 1) {
  colors <- nightowl_colors()
  
  if (!name %in% names(colors)) {
    stop(paste("Color", name, "not found. Available colors:", 
               paste(names(colors), collapse = ", ")))
  }
  
  col <- colors[[name]]
  
  # Add alpha channel if needed
  if (alpha < 1) {
    rgb_val <- col2rgb(col)
    col <- rgb(rgb_val[1], rgb_val[2], rgb_val[3], 
               alpha * 255, maxColorValue = 255)
  }
  
  return(col)
}

#' Nightowl Discrete Color Palette for ggplot2
#'
#' @param n Number of colors to return
#' @return Function that returns n colors
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = factor(cyl), fill = factor(cyl))) +
#'   geom_bar() +
#'   scale_fill_manual(values = nightowl_palette_discrete()(3))
nightowl_palette_discrete <- function(n = 1) {
  colors <- nightowl_colors()
  
  # Define the order of colors to use
  color_order <- c("blue", "red", "green", "purple", "orange", 
                   "yellow", "pink", "brown", "grey")
  
  function(n) {
    if (n <= length(color_order)) {
      unname(colors[color_order[1:n]])
    } else {
      # Use colorRampPalette to generate more colors if needed
      base_colors <- unname(colors[color_order])
      colorRampPalette(base_colors)(n)
    }
  }
}

#' Check if a Color is Dark
#'
#' @param color Character string or vector of color specifications
#' @return Logical value(s) indicating if color(s) are dark
#' @export
#' @examples
#' is_dark("#000000")  # TRUE
#' is_dark("#FFFFFF")  # FALSE
#' is_dark(c("black", "white", "#007DBA"))
is_dark <- function(color) {
  # Vectorized function to handle multiple colors
  sapply(color, function(col) {
    # Convert color to RGB
    rgb_val <- col2rgb(col)
    
    # Calculate relative luminance using WCAG formula
    # See: https://www.w3.org/WAI/GL/wiki/Relative_luminance
    r <- rgb_val[1, 1] / 255
    g <- rgb_val[2, 1] / 255
    b <- rgb_val[3, 1] / 255
    
    # Apply gamma correction
    r <- ifelse(r <= 0.03928, r / 12.92, ((r + 0.055) / 1.055)^2.4)
    g <- ifelse(g <= 0.03928, g / 12.92, ((g + 0.055) / 1.055)^2.4)
    b <- ifelse(b <= 0.03928, b / 12.92, ((b + 0.055) / 1.055)^2.4)
    
    # Calculate luminance
    luminance <- 0.2126 * r + 0.7152 * g + 0.0722 * b
    
    # Return TRUE if dark (luminance < 0.5)
    return(luminance < 0.5)
  })
}

#' Get Contrasting Text Color
#'
#' @param background_color Background color to contrast against
#' @return Character string with "black" or "white"
#' @export
#' @examples
#' get_text_color("#007DBA")  # "white"
#' get_text_color("#FFFFFF")  # "black"
get_text_color <- function(background_color) {
  ifelse(is_dark(background_color), "white", "black")
}