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
