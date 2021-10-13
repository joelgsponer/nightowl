# ===============================================================================
#' Wrap text label at a certain width
#' @param width
#' @export
add_text_wraping <- function(DATA, cols = NULL, width) {
  if (is.null(cols)) cols <- names(DATA)[purrr::map_lgl(DATA, ~ is.factor(.x))]
  purrr::reduce(cols, function(.out, .col) {
    .x <- .out[[.col]]
    levels(.x) <- stringr::str_wrap(levels(.x), width)
    .out[[.col]] <- .x
    .out
  }, .init = DATA)
}
# ===============================================================================
#' Handle missing values
#' @export
prepare_data_for_plotting <- function(DATA, cols = NULL, remove_missing = T) {
  if (is.null(cols)) cols <- names(DATA)
  if (remove_missing) {
    DATA <- DATA %>%
      dplyr::filter_at(c(cols), function(x) !is.na(x)) %>%
      dplyr::mutate_if(is.character, factor) %>%
      droplevels()
  } else {
    # Make missing factors explicit
    # Convert Characters to factors
    DATA <- DATA %>%
      dplyr::mutate_if(is.character, factor) %>%
      dplyr::mutate_if(is.factor, forcats::fct_explicit_na)
  }
  return(DATA)
}
# ===============================================================================
#' Create ggplot form list
#' Setup ggplot
#' This was difficult, fist store parameters in list,
#' Convert to symbols
#' drop the onses which are null, call aes_ function (CAVE: ecex)
#' also think of other places where params is used, e.g. params$id
#' @export
ggplot <- function(DATA, aes, ...) {
  aes <- aes %>%
    purrr::compact() %>%
    purrr::map(~ rlang::sym(.x))
  f <- ggplot2::aes_
  .aes <- rlang::exec(.fn = "f", !!!aes)
  ggplot2::ggplot(data = DATA, .aes)
}
# ===============================================================================
