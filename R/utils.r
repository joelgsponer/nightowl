#' expand mapping
expand_mapping <- function(mapping,
                           envir = parent.frame(),
                           as_symbol = T) {
  force(envir)
  purrr::iwalk(mapping, function(.value, .name) {
    if (!is.null(.value)) {
      assign(.name, rlang::sym(.value), envir = envir)
    }
  })
}
# =================================================
#' Natural sort factor
#' @export
fct_natural <- function(x) {
  x <- forcats::as_factor(x)
  l <- levels(x)
  forcats::fct_relevel(x, gtools::mixedsort(l))
}
# =================================================
#' wrapping labeleer
#' @export
label_with_wraping <- function(x, width = 10) {
  purrr::map(ggplot2::label_both(x), ~ stringr::str_wrap(.x, width))
}# ===============================================================================
#' Spread data
#' @export
spread_data <- function(DATA, key, value = NULL) {
  if (is.null(value)) {
    return(DATA)
  } else {
    tryCatch({
      DATA %>%
        dplyr::ungroup() %>%
        tidyr::pivot_wider(names_from = key, values_from = value)
    })
  }
}
# ===============================================================================
#' Wrap text label at a certain width
#' @param width
#' @export
text_wraping <- function(DATA, cols = NULL, width = 20, ...) {
  if (is.null(cols)) cols <- names(DATA)[purrr::map_lgl(DATA, ~ is.factor(.x))]
  purrr::reduce(cols, function(.out, .col) {
    .x <- .out[[.col]]
    levels(.x) <- stringr::str_wrap(levels(.x), width)
    .out[[.col]] <- .x
    .out
  }, .init = DATA)
}
#=================================================
