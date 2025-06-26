# =================================================
#' @title Load Plot Style Configuration
#' @description Loads a predefined plot style configuration from the package's styles directory
#' @param x Character string specifying the style name (without .yaml extension)
#' @return List containing the style configuration parameters loaded from the YAML file
#' @export
load_style <- function(x) {
  paste0(x, ".yaml") %>%
    file.path("styles", .) %>%
    system.file(package = "nightowl") %>%
    yaml::read_yaml()
}
# =================================================
#' @title Create Styled Plot
#' @description Creates a plot with predefined style configuration from a YAML file or style name
#' @param data Data frame containing the data to plot
#' @param style Either a file path to a YAML style configuration or a style name to load from the package
#' @param ... Additional mapping arguments passed to the plot function
#' @return A nightowl plot object with the specified style applied
#' @export
styled_plot <- function(data, style, ...) {
  # Validate inputs
  data <- validate_data_frame(data, param_name = "data")
  
  mapping <- list(...)
  
  if (is.character(style) && file.exists(style)) {
    # Security fix: validate file path before reading
    style <- validate_file_path(style, must_exist = TRUE, 
                               allowed_extensions = c("yaml", "yml"),
                               param_name = "style")
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
