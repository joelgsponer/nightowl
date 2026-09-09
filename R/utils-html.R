#' nightowl HTML dependency
#'
#' The stylesheet and the small script (SVG download button) that nightowl
#' output relies on. The package's own renderers attach it automatically;
#' attach it yourself when you embed nightowl output in other htmltools
#' markup.
#'
#' @return An [htmltools::htmlDependency()].
#' @examples
#' htmltools::tagList(nightowl_dependency(), htmltools::div(class = "nightowl", "text"))
#' @export
nightowl_dependency <- function() {
  htmltools::htmlDependency(
    name = "nightowl",
    version = as.character(utils::packageVersion("nightowl")),
    src = c(file = "assets"),
    package = "nightowl",
    stylesheet = "css/nightowl.css",
    script = "js/nightowl.js"
  )
}

#' Wrap rendered content in the nightowl card
#' @noRd
html_card <- function(body, title = NULL, subtitle = NULL, footnote = NULL, class = NULL, plain = FALSE) {
  footnote <- footnote %||% character(0)
  footnote <- footnote[!is.na(footnote) & nzchar(footnote)]
  card <- htmltools::div(
    class = paste(c("nightowl", "nightowl-card", if (plain) "nightowl-card--plain", class), collapse = " "),
    if (!is.null(title)) htmltools::tags$p(class = "nightowl-title", htmltools::HTML(title)),
    if (!is.null(subtitle)) htmltools::tags$p(class = "nightowl-subtitle", htmltools::HTML(subtitle)),
    htmltools::div(class = "nightowl-body", body),
    if (length(footnote) > 0) {
      htmltools::div(class = "nightowl-footnote", lapply(footnote, function(f) htmltools::tags$p(htmltools::HTML(f))))
    }
  )
  htmltools::browsable(htmltools::attachDependencies(card, nightowl_dependency()))
}

#' Centre cell content in kable output
#' @noRd
cell_div <- function(x, class = "nightowl-cell") {
  vapply(x, function(v) as.character(htmltools::div(class = class, htmltools::HTML(v))), character(1), USE.NAMES = FALSE)
}

#' HTML colour swatch used in frequency legends
#' @noRd
html_swatch <- function(colour) {
  as.character(htmltools::span(class = "nightowl-swatch", style = glue::glue("background-color:{colour};")))
}

#' Header for a forest-plot column
#' @noRd
forest_header <- function(label = "log(HR)", left = "Comparison better", right = "Reference better") {
  as.character(htmltools::div(
    class = "nightowl-forest-header",
    htmltools::div(label),
    htmltools::div(htmltools::HTML(glue::glue("&larr; {left} | {right} &rarr;")))
  ))
}
