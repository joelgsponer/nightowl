# ar ===============================================================================
#' Summry table
#' @export
table <- function(DATA,
                  mapping,
                  options = list(group_split = "row"),
                  layers = list(
                    list(type = "tern::summarize_vars", denom = "n")
                  ),
                  ...) {
  #*******************************************************************************
  # Adjust naming for
  if (!is.null(mapping$cols) && is.null(mapping$x)) mapping$x <- mapping$columns
  if (!is.null(mapping$rows) && is.null(mapping$y)) mapping$y <- mapping$rows
  if (!is.null(mapping$split_rows_by) && is.null(mapping$facet_row)) {
    mapping$facet_row <- mapping$split_rows_by
  }
  if (!is.null(mapping$split_cols_by) && is.null(mapping$facet_col)) {
    mapping$facet_col <- mapping$split_cols_by
  }
  # Drop columns that are not needed
  DATA <- DATA %>%
    dplyr::select_at(unlist(unname(mapping))) %>%
    droplevels()
  #*******************************************************************************
  # Drop missing values
  DATA <- DATA %>%
    dplyr::mutate_if(is.character, factor) %>%
    dplyr::mutate_if(is.factor, forcats::fct_explicit_na) %>%
    droplevels()
  # Data preparation
  DATA <- nightowl::text_wraping(DATA)
  #*******************************************************************************
  # Build table
  .lyt <- rtables::basic_table()
  .lyt <- purrr::reduce(mapping$facet_col, ~ rtables::split_cols_by(.x, .y), .init = .lyt)
  .lyt <- purrr::reduce(mapping$facet_row, ~ rtables::split_rows_by(.x, .y), .init = .lyt)
  .lyt <- rtables::split_cols_by(.lyt, mapping$x)
  if (!is.null(mapping$group) && options$group_split == "row") .lyt <- rtables::split_rows_by(.lyt, mapping$group)
  if (!is.null(mapping$group) && options$group_split == "col") .lyt <- rtables::split_cols_by(.lyt, mapping$group)
  .lyt <- purrr::reduce(layers, function(.x, .y) {
    type <- .y$type
    .y <- .y[names(.y) %in% waRRior::pop(names(.y), "type")]
    .y$lyt <- .x
    if (!is.null(mapping$y)) .y$vars <- mapping$y
    do.call(waRRior::getfun(type), .y)
  }, .init = .lyt)
  rtables::build_table(.lyt, DATA)
}
# =================================================
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
reactable_default <- function(x) {
  reactable::reactable(
    x,
    filterable = T,
    style = list(fontFamily = "Work Sans, sans-serif", fontSize = "14px", padding = "10px"),
    searchable = TRUE
  )
}
# =================================================
