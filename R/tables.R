#' Summry table
# ar ===============================================================================
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
#' Summry table
# ar ===============================================================================
#' @export
model_output <- function(DATA,
                         mapping,
                         model = "lm",
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

  DATA$combination <- DATA[, c(mapping$facet_col, mapping$facet_row)] %>%
    as.matrix() %>%
    apply(1, function(x) {
      paste(
        c(mapping$facet_col, mapping$facet_row),
        x,
        collapse = "/"
      )
    })
  print(head(DATA))

  DATA %>%
    waRRior::named_group_split_at("combination") %>%
    purrr::imap(function(.x, .y) {
      tryCatch(
        {
          .formula.txt <- glue::glue("`{mapping$y}` ~ `{mapping$x}`")
          .formula <- as.formula(.formula.txt)
          .model <- waRRior::getfun(model)
          res <- .model(.formula, .x) %>%
            base::summary() %>%
            shiny::renderPrint() %>%
            .() %>%
            stringr::str_replace(stringr::fixed(".formula"), .formula.txt)
          pre <- shiny::tag("pre", res)
          shiny::div(shiny::h3(.y), pre)
        },
        error = function(e) {
          return(shiny::div(shiny::h3(.y), shiny::div(class = "lowrider-error", as.character(e))))
        }
      )
    }) %>%
    shiny::div(style = "
      display:flex;
      flex-wrap:wrap;
      flex-direction: column;
      justify-content: space-around;
      align-items: center;
    ") %>%
    htmltools::browsable()
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
reactable_default <- function(x,
                              filterable = T,
                              style = list(fontFamily = "Work Sans, sans-serif", fontSize = "14px", padding = "10px"),
                              searchable = TRUE,
                              selection = "multiple",
                              onClick = "select",
                              showPageSizeOptions = TRUE,
                              pageSizeOptions = c(10, 25, 50, 100),
                              ...) {
  reactable::reactable(
    x,
    filterable = filterable,
    style = style,
    searchable = searchable,
    selection = selection,
    onClick = onClick,
    showPageSizeOptions = showPageSizeOptions,
    pageSizeOptions = pageSizeOptions,
    ...
  )
}
# =================================================
