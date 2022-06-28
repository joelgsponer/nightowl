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
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
render_kable <- function(.tbl,
                         caption = NULL,
                         full_width = FALSE,
                         digits = 2,
                         add_scale = T,
                         column_width = "20em",
                         width_header = 20,
                         footnote = NULL,
                         ...) {
  if (!is.null(width_header)) {
    names(.tbl) <- stringr::str_wrap(names(.tbl), width = width_header)
  }
  if (add_scale) .tbl <- nightowl::add_scale(.tbl)
  .tbl <- dplyr::mutate_if(.tbl, is.numeric, function(x) round(x, digits))
  .tbl <- dplyr::mutate_if(.tbl, is.numeric, as.character)
  .tbl <- dplyr::mutate_if(.tbl, nightowl::is_NightowlPlots, as.character)
  .tbl <- dplyr::mutate_all(.tbl, function(x) tidyr::replace_na(x, ""))
  .kable <- .tbl %>%
    knitr::kable("html", escape = FALSE, caption = caption)
  .kable <- kableExtra::kable_styling(.kable, full_width = full_width, ...)
  if (!is.null(footnote)) {
    .kable <- kableExtra::add_footnote(.kable, footnote)
  }
  return(.kable)
}
# s-------------------------------------------------------------------------------
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
render_reactable <- function(.tbl,
                             html_columns = NULL,
                             theme = function() reactablefmtr::nytimes(centered = TRUE),
                             digits = 2,
                             defaultPageSize = 10,
                             filterable = T,
                             add_scale = T,
                             defaultColDef = list(
                               header = function(value) gsub(".", " ", value, fixed = TRUE),
                               align = "center",
                               html = TRUE
                             ),
                             fullWidth = FALSE,
                             ...) {
  .groups <- waRRior::get_groups(.tbl)
  # Infer column types
  col_NightowlPlots <- names(.tbl)[purrr::map_lgl(.tbl, ~ inherits(.x, "NightowlPlots"))]
  col_HTML <- names(.tbl)[purrr::map_lgl(.tbl, ~ inherits(.x, "html"))]
  # ColDef
  col_def <- list()
  col_def <- purrr::reduce(col_NightowlPlots, function(.old, .new) {
    .old[[.new]] <- reactable::colDef(minWidth = width(.tbl[[.new]]), html = TRUE)
    .old
  }, .init = col_def)
  col_def <- purrr::reduce(col_HTML, function(.old, .new) {
    .old[[.new]] <- reactable::colDef(html = TRUE)
    .old
  }, .init = col_def)
  col_def <- purrr::reduce(.groups, function(.old, .new) {
    .old[[.new]] <- do.call(reactable::colDef, c(list(sticky = "left"), as.list(.old[[.new]])))
    .old
  }, .init = col_def)
  # Prepare data
  .tbl <- dplyr::select_at(.tbl, c(.groups, waRRior::pop(names(.tbl), .groups)))
  .tbl <- dplyr::ungroup(.tbl)
  if (add_scale) .tbl <- nightowl::add_scale(.tbl)
  .tbl <- dplyr::mutate_if(.tbl, is.numeric, function(x) format(round(x, digits), nsmall = 0))
  .tbl <- dplyr::mutate_if(.tbl, nightowl::is_NightowlPlots, as.character)

  # Make groups sticky
  res <-
    reactable::reactable(
      .tbl,
      theme = theme(),
      defaultColDef = do.call(reactable::colDef, defaultColDef),
      columns = col_def,
      filterable = filterable,
      defaultPageSize = defaultPageSize,
      style = list(fontFamily = "Work Sans, sans-serif", fontSize = "11px", padding = "5px"),
      searchable = FALSE,
      bordered = FALSE,
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(1, 5, 10, 100),
      fullWidth = fullWidth,
      ...
    )

  return(res)
}
# =================================================
#' @title
#' Style a cell for kable tables
#' @description
#' Function arguments are translated into CSS adn applied to the cell content
#' @param
#' @return
#' @export
style_cell <- function(x, ...) {
  style <- list(...) %>%
    purrr::imap(~ glue::glue("{stringr::str_replace(.y, '_', '-')}:{.x};")) %>%
    paste(collapse = "")
  purrr::map(x, ~ shiny::HTML(as.character(glue::glue("<div style='{style}'>{.x}</div>"))))
}
#*************************************************
