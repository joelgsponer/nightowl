# =================================================
#' @title Load Plot Style Configuration
#' @description 
#' Load a YAML style configuration file from the package's styles directory.
#' @param x Character string specifying the style name (without .yaml extension)
#' @return A list containing the parsed YAML configuration
#' @export
#' @examples
#' \dontrun{
#' style_config <- load_style("Histogram-simple")
#' }
load_style <- function(x) {
  paste0(x, ".yaml") %>%
    file.path("styles", .) %>%
    system.file(package = "nightowl") %>%
    yaml::read_yaml()
}
# =================================================
#' @title Create Styled Plot from Configuration
#' @description 
#' Create a plot using predefined style configurations. Can accept either a file path
#' to a YAML style file or a style name from the package's built-in styles.
#' @param data A data frame containing the data to plot
#' @param style Either a file path to a YAML style file or a style name
#' @param ... Additional arguments passed as mapping parameters
#' @return A nightowl plot object
#' @export
#' @examples
#' \dontrun{
#' styled_plot(mtcars, "Histogram-simple", x = "mpg")
#' }
styled_plot <- function(data, style, ...) {
  mapping <- list(...)
  if (file.exists(style)) {
    style <- yaml::read_yaml(style)
  } else {
    style <- nightowl::load_style(style)
  }
  do.call(
    nightowl::plot,
    c(list(data = data), list(mapping = mapping), style)
  )
}
# =================================================
