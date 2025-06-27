# =================================================
#' @title Create Default Reactable Table (Deprecated)
#' @description 
#' Create a reactable table with default styling and functionality. This function
#' is deprecated in favor of render_reactable().
#' @param x A data frame to render as a table
#' @return A reactable table widget
#' @export
#' @keywords internal
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
#' @description 
#' Create a formatted HTML table using knitr::kable with kableExtra styling.
#' Provides comprehensive formatting options for clinical research reports.
#' @param .tbl A data frame to render as a table
#' @param caption Optional table caption
#' @param full_width Logical indicating whether table should span full width
#' @param digits Number of decimal places for numeric values
#' @param add_scale Logical indicating whether to add scale information
#' @param width_header Maximum width for header text wrapping
#' @param header_above Optional named vector for multi-level headers
#' @param htmltable_class CSS class for table styling
#' @param footnote Optional footnote text
#' @param align Column alignment specification
#' @param ... Additional arguments passed to kable_styling
#' @return A formatted HTML table
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
#' @title Render HTML Table with Dependencies
#' @description 
#' Render a table as HTML with required CSS/JavaScript dependencies for proper styling.
#' Wraps the table in a browsable HTML container.
#' @param .tbl A data frame to render as an HTML table
#' @param html_dependencies Function providing HTML dependencies (default: lightable theme)
#' @param ... Additional arguments passed to render_kable
#' @return A browsable HTML element containing the styled table
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
#' @title Render Interactive Reactable Table
#' @description 
#' Create an interactive reactable table with advanced features like filtering,
#' sorting, and custom styling. Optimized for nightowl plot integration.
#' @param .tbl A data frame to render as an interactive table
#' @param html_columns Optional vector specifying columns containing HTML content
#' @param theme Function returning reactable theme (default: nytimes centered)
#' @param digits Number of decimal places for numeric values
#' @param defaultPageSize Default number of rows to display per page
#' @param filterable Logical indicating whether to enable column filtering
#' @param add_scale Logical indicating whether to add scale information
#' @param defaultColDef List of default column definitions
#' @param fullWidth Logical indicating whether table should span full width
#' @param ... Additional arguments passed to reactable
#' @return A reactable widget with nightowl styling
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
  .groups <- waRRior::get_groups(.tbl)
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
  .tbl <- dplyr::select_at(.tbl, c(.groups, waRRior::pop(names(.tbl), .groups)))
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
