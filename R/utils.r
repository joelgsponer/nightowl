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
