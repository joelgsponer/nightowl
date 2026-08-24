# =================================================
#' @title Arrange Summary Tables in HTML Layout
#' @description 
#' Arrange multiple summary tables in a flexible HTML layout with styled headers 
#' and borders. Creates a responsive grid display suitable for clinical research reports.
#' @param tbls A named list of summary tables to arrange
#' @return An HTML div element containing the arranged tables
#' @export
arrange_summaries <- function(tbls) {
  # Full version
  shiny::div(
    style = "
      width:fit-content;
      font-family:Lato;
      display:flex;
      flex-wrap:wrap;
      justify-content:space-between;
      flex-direction:row;
    ",
    kableExtra:::html_dependency_lightable(),
    purrr::imap(tbls, function(.x, .y) {
      .title <- shiny::h3(.y)
      shiny::div(
        style = "
        border-style: solid;
        border-color: black;
        border-width: medium;
        margin: 5px;
      ",
        shiny::HTML("<style>
          th:first-child {
            border-bottom: none !important;
          }
        </style>"),
        shiny::div(
          style = "display: flex; flex-direction: column; align-items: center; align-content: flex-start;",
          .title,
          shiny::div(
            style = "
                display: flex;
                flex-direction: row;
                align-items: center;
                align-content: flex-start;
              ",
            shiny::h3(style = "width: 10px; margin-left: 10px; transform: rotate(-90deg);", "CLUSTER"),
            .x
          )
        )
      )
    })
  ) %>%
    htmltools::browsable()
}
# =================================================
