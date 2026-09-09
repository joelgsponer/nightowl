#' Render a table with kableExtra
#'
#' Renders a data frame as an HTML table in the nightowl style. Columns of
#' class [NightowlPlots] are rendered as inline SVG; numeric columns are
#' rounded; headers are wrapped.
#'
#' @param data A data frame or tibble. Grouping is ignored.
#' @param caption,footnote Optional caption above and note below the table.
#'   Both may contain HTML.
#' @param digits Decimals for numeric columns.
#' @param header_width Wrap width for column headers; `NULL` disables.
#' @param header_above Passed to [kableExtra::add_header_above()].
#' @param scale_row Add a row with an axis under every inline-plot column, see
#'   [add_inline_scale()].
#' @param align Column alignment, see [knitr::kable()].
#' @param full_width Passed to [kableExtra::kable_styling()].
#' @param ... Further arguments to [kableExtra::kable_styling()].
#' @return A `kableExtra` object (a character string with class attributes)
#'   that prints as HTML in R Markdown and Quarto. Wrap it in
#'   [render_html()] to view it in the browser with nightowl styling.
#' @examples
#' render_kable(head(mtcars[, 1:4]), caption = "First cars")
#' @export
render_kable <- function(data,
                         caption = NULL,
                         footnote = NULL,
                         digits = 2,
                         header_width = nightowl_option("header_width"),
                         header_above = NULL,
                         scale_row = TRUE,
                         align = NULL,
                         full_width = FALSE,
                         ...) {
  check_data_frame(data)
  data <- dplyr::ungroup(data)
  plot_cols <- names(data)[vapply(data, is_NightowlPlots, logical(1))]
  if (scale_row && length(plot_cols) > 0) {
    data <- add_inline_scale(data)
  }
  if (!is.null(header_width)) {
    is_html <- stringr::str_detect(names(data), "<")
    names(data)[!is_html] <- stringr::str_wrap(names(data)[!is_html], width = header_width)
  }
  data <- dplyr::mutate(data, dplyr::across(dplyr::where(is.numeric), ~ round(.x, digits)))
  data <- dplyr::mutate(data, dplyr::across(dplyr::where(is_NightowlPlots), ~ as.character(.x)))
  data <- dplyr::mutate(data, dplyr::across(dplyr::everything(), ~ tidyr::replace_na(as.character(.x), "")))
  data <- dplyr::mutate(data, dplyr::across(dplyr::everything(), cell_div))
  if (is.null(align)) align <- rep("c", ncol(data))
  k <- kableExtra::kbl(data, format = "html", escape = FALSE, caption = caption, align = align,
                       table.attr = "class='nightowl-table'")
  if (!is.null(header_above)) k <- kableExtra::add_header_above(k, header_above, escape = FALSE)
  k <- kableExtra::kable_styling(k, full_width = full_width, htmltable_class = "nightowl-table", ...)
  if (!is.null(footnote)) {
    k <- kableExtra::footnote(k, general = footnote, general_title = "", escape = FALSE)
  }
  strip_cdata(k)
}

strip_cdata <- function(k) {
  attrs <- attributes(k)
  out <- gsub("<![CDATA[", "", k, fixed = TRUE)
  out <- gsub("]]>", "", out, fixed = TRUE)
  attributes(out) <- attrs
  out
}

#' Render a table as a browsable HTML block
#'
#' Wraps [render_kable()] output in the nightowl card with its CSS attached,
#' so it can be printed in the viewer, saved with [htmltools::save_html()], or
#' embedded in Shiny.
#'
#' @inheritParams render_kable
#' @param title,subtitle Optional heading above the table.
#' @param footnote Optional note below the table; may contain HTML.
#' @param ... Passed to [render_kable()].
#' @return A browsable [htmltools::tag].
#' @examples
#' render_html(head(mtcars[, 1:4]), title = "Cars")
#' @export
render_html <- function(data, title = NULL, subtitle = NULL, footnote = NULL, ...) {
  k <- render_kable(data, footnote = NULL, ...)
  html_card(htmltools::HTML(k), title = title, subtitle = subtitle, footnote = footnote)
}

#' Render a table with reactable
#'
#' An interactive table in the nightowl style. Columns of class
#' [NightowlPlots] are rendered as inline SVG with a matching width and an
#' axis in the footer; grouping columns are pinned to the left.
#'
#' @inheritParams render_kable
#' @param columns Named list of [reactable::colDef()] overrides, merged over
#'   the generated definitions.
#' @param default_col_def Arguments to [reactable::colDef()] used as
#'   `defaultColDef`.
#' @param theme A [reactable::reactableTheme()].
#' @param page_size Rows per page.
#' @param filterable Show column filters.
#' @param ... Further arguments to [reactable::reactable()].
#' @return An htmlwidget.
#' @examples
#' render_reactable(head(mtcars[, 1:4]))
#' @export
render_reactable <- function(data,
                             columns = list(),
                             digits = 2,
                             scale_row = TRUE,
                             default_col_def = list(align = "center", na = "-", html = TRUE, minWidth = 70,
                                                    style = list(whiteSpace = "nowrap")),
                             theme = nightowl_reactable_theme(),
                             page_size = 10,
                             filterable = FALSE,
                             ...) {
  check_data_frame(data)
  group_cols <- dplyr::group_vars(data)
  data <- dplyr::ungroup(data)
  data <- dplyr::select(data, dplyr::all_of(group_cols), dplyr::everything())
  plot_cols <- names(data)[vapply(data, is_NightowlPlots, logical(1))]
  html_cols <- names(data)[vapply(data, function(x) inherits(x, "html"), logical(1))]
  col_def <- list()
  for (col in plot_cols) {
    footer <- if (scale_row) as.character(make_scale_plot(data[[col]])$svg(download_button = FALSE)) else NULL
    col_def[[col]] <- reactable::colDef(
      minWidth = plots_width(data[[col]]),
      footer = footer,
      footerClass = "nightowl-scale",
      html = TRUE,
      filterable = FALSE,
      sortable = FALSE
    )
  }
  for (col in html_cols) {
    col_def[[col]] <- reactable::colDef(html = TRUE)
  }
  for (col in setdiff(names(data), c(plot_cols, html_cols))) {
    col_def[[col]] <- reactable::colDef(minWidth = text_width_px(c(col, data[[col]])))
  }
  for (col in group_cols) {
    col_def[[col]] <- reactable::colDef(sticky = "left", align = "left")
  }
  for (col in names(columns)) {
    col_def[[col]] <- columns[[col]]
  }
  data <- dplyr::mutate(data, dplyr::across(dplyr::where(is.numeric), ~ round(.x, digits)))
  data <- dplyr::mutate(data, dplyr::across(dplyr::where(is_NightowlPlots), ~ as.character(.x)))
  data <- dplyr::mutate(data, dplyr::across(dplyr::where(function(x) inherits(x, "html")), ~ as.character(.x)))
  widget <- reactable::reactable(
    data,
    columns = if (length(col_def) > 0) col_def else NULL,
    defaultColDef = do.call(reactable::colDef, default_col_def),
    theme = theme,
    defaultPageSize = page_size,
    filterable = filterable,
    showPageSizeOptions = nrow(data) > page_size,
    pageSizeOptions = unique(c(page_size, 25, 50, 100)),
    bordered = FALSE,
    class = "nightowl nightowl-reactable",
    ...
  )
  htmltools::attachDependencies(widget, nightowl_dependency(), append = TRUE)
}

#' The reactable theme used by nightowl
#'
#' @return A [reactable::reactableTheme()].
#' @examples
#' render_reactable(head(mtcars), theme = nightowl_reactable_theme())
#' @export
nightowl_reactable_theme <- function() {
  ink <- unname(nightowl_colours("ink"))
  reactable::reactableTheme(
    color = ink,
    borderColor = "#EEEEEE",
    stripedColor = "#FAFAFA",
    highlightColor = "#F3F7FB",
    headerStyle = list(
      borderBottom = paste0("2px solid ", ink),
      fontWeight = 700
    ),
    style = list(fontFamily = nightowl_option("font_family"), fontSize = "0.9rem"),
    footerStyle = list(borderTop = paste0("1px solid ", ink)),
    searchInputStyle = list(width = "100%")
  )
}

#' Add an axis row under inline-plot columns
#'
#' Inline plots hide their axes. `add_inline_scale()` appends one row to the
#' table holding, for every [NightowlPlots] column, a plot with nothing but
#' the x axis of that column, so readers can see the scale.
#'
#' @param data A data frame with at least one [NightowlPlots] column.
#' @param columns Which plot columns to scale; defaults to all of them.
#' @param height Height of the axis row plots in inches.
#' @return `data` with one extra row. Factor columns are converted to
#'   character so the empty cells can hold `""`.
#' @examples
#' \donttest{
#' s <- Summary$new(mtcars, "mpg", group_by = "cyl", method = summarise_numeric_pointrange)
#' nrow(add_inline_scale(s$raw()))
#' }
#' @export
add_inline_scale <- function(data, columns = NULL, height = 0.3) {
  check_data_frame(data)
  data <- dplyr::ungroup(data)
  plot_cols <- names(data)[vapply(data, is_NightowlPlots, logical(1))]
  columns <- columns %||% plot_cols
  bad <- setdiff(columns, plot_cols)
  if (length(bad) > 0) {
    cli::cli_abort("{cli::qty(length(bad))}Column{?s} {.val {bad}} {?is/are} not {.cls NightowlPlots}.")
  }
  if (length(columns) == 0) {
    return(data)
  }
  scale_row <- purrr::map(columns, function(col) new_NightowlPlots(make_scale_plot(data[[col]], height = height)))
  scale_row <- setNames(scale_row, columns)
  scale_row <- tibble::as_tibble(scale_row)
  data <- dplyr::mutate(data, dplyr::across(dplyr::where(is.factor), as.character))
  out <- dplyr::bind_rows(data, scale_row)
  dplyr::mutate(out, dplyr::across(dplyr::where(is.character), ~ tidyr::replace_na(.x, "")))
}

#' Build the axis-only plot for a column of inline plots
#' @noRd
make_scale_plot <- function(plots, height = 0.3) {
  first <- vctrs::vec_data(plots)[[1]]
  gg <- first$plot
  gg$layers <- list()
  gg <- gg + theme_scale_row()
  opts <- first$svg_options
  opts$height <- height
  Plot$new(plot = gg, svg = opts, type = "Scale", resize = FALSE)
}

#' Rough pixel width needed to show strings on one line (0.9rem font)
#' @noRd
text_width_px <- function(x, char_px = 7.5, padding = 26, max = 320) {
  x <- as.character(x)
  x <- x[!is.na(x)]
  header <- if (length(x) > 0) nchar(x[1]) * 1.2 else 0
  widest <- max(c(0, header, nchar(x[-1])))
  min(max, max(60, ceiling(widest * char_px + padding)))
}
