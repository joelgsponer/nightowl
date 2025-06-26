# =================================================
#' @title Default Reactable Configuration (Deprecated)
#' @description Creates a reactable with default styling options - use render_reactable() instead
#' @param x Data frame to display in the table
#' @return Reactable object with default styling
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
  lifecycle::deprecate_stop("", "reactable_default()", "render_reactable()")
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
#' @title Render Kable Table
#' @description Creates a styled HTML table using knitr::kable with nightowl formatting
#' @param .tbl Data frame to render as a table
#' @return HTML string containing the formatted table
#' @export
render_kable <- function(.tbl,
                         caption = NULL,
                         full_width = FALSE,
                         digits = 2,
                         add_scale = T,
                         width_header = 20,
                         header_above = NULL,
                         htmltable_class = "lightable-minimal",
                         footnote = NULL,
                         align = "l",
                         ...) {
  if (!is.null(width_header)) {
    html_headers <- stringr::str_detect(names(.tbl), "<div")
    names(.tbl)[!html_headers] <- stringr::str_wrap(names(.tbl)[!html_headers], width = width_header)
  }
  if (add_scale) .tbl <- nightowl::add_scale(.tbl)
  .tbl <- dplyr::mutate_if(.tbl, is.numeric, function(x) round(x, digits))
  .tbl <- dplyr::mutate_if(.tbl, is.numeric, as.character)
  .tbl <- dplyr::mutate_if(.tbl, nightowl::is_NightowlPlots, as.character)
  .tbl <- dplyr::mutate_all(.tbl, function(x) tidyr::replace_na(x, ""))
  .tbl <- purrr::map(.tbl, function(.x) nightowl::style_cell(.x, width = "max-content", margin = "auto") %>% as.character()) %>%
    tibble::as_tibble()
  .kable <- .tbl %>%
    knitr::kable("html", escape = FALSE, caption = caption, align = align)
  if (!is.null(header_above)) .kable <- kableExtra::add_header_above(.kable, header_above)
  .kable <- kableExtra::kable_styling(.kable, full_width = full_width, htmltable_class = htmltable_class, ...)
  if (!is.null(footnote)) {
    .kable <- kableExtra::add_footnote(.kable, footnote, notation = "none", escape = FALSE)
  }
  .kable <- .kable %>%
    stringr::str_replace_all(stringr::fixed("<![CDATA["), "") %>%
    stringr::str_replace_all(stringr::fixed("]]>"), "")
  return(.kable)
}
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
render_html <- function(.tbl, html_dependencies = kableExtra:::html_dependency_lightable, ...) {
  shiny::div(
    html_dependencies(),
    shiny::HTML("<style>body {font-family: Lato, sans-serif;}</style>"),
    shiny::HTML(nightowl::render_kable(.tbl, ...))
  ) %>%
    htmltools::browsable()
}
# s-------------------------------------------------------------------------------
# =================================================
#' @title Render Reactable Table
#' @description Creates an interactive table using reactable with nightowl styling and themes
#' @param .tbl Data frame to render as an interactive table
#' @return Reactable object with nightowl styling and interactive features
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
                               na = "-",
                               html = TRUE
                             ),
                             fullWidth = FALSE,
                             ...) {
  css <- "
      .nightowl-scale {
      background: red;
      overflow: visible;
    }
    .nightowl-scale .rt-td-inner {
      background: red;
      overflow: visible;
    }
    .nightowl-scale .rt-text-content {
      background: red;
      overflow: visible !important
      ;
    }
  "
  .groups <- nightowl_get_groups(.tbl)
  # Infer column types
  col_NightowlPlots <- names(.tbl)[purrr::map_lgl(.tbl, ~ inherits(.x, "NightowlPlots"))]
  col_HTML <- names(.tbl)[purrr::map_lgl(.tbl, ~ inherits(.x, "html"))]
  # ColDef
  col_def <- list()
  col_def <- purrr::reduce(col_NightowlPlots, function(.old, .new) {
    .old[[.new]] <- reactable::colDef(
      minWidth = width(.tbl[[.new]]),
      footer = as.character(nightowl::make_scale(.tbl[[.new]])),
      footerClass = "nightowl-scale",
      html = TRUE
    )
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
  .tbl <- dplyr::select_at(.tbl, c(.groups, nightowl_pop(names(.tbl), .groups)))
  .tbl <- dplyr::ungroup(.tbl)
  # if (add_scale) .tbl <- nightowl::add_scale(.tbl)
  .tbl <- dplyr::mutate_if(.tbl, is.numeric, function(x) round(x, digits))
  .tbl <- dplyr::mutate_if(.tbl, nightowl::is_NightowlPlots, as.character)
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
