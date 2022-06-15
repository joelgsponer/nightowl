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
styled_plot <- function(DATA, style, ...) {
  mapping <- list(...)
  do.call(
    nightowl::plot,
    c(list(DATA = DATA), list(mapping = mapping), nightowl::load_style(style))
  )
}
# =================================================
