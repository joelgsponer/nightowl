# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
load_style <- function(x) {
  paste0(x, ".yaml") %>%
    file.path("styles", .) %>%
    system.file(package = "nightowl") %>%
    yaml::read_yaml()
}
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
styled_plot <- function(data, style, ...) {
  mapping <- list(...)
  if(file.exists(style)) {
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
