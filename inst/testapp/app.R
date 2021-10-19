library(shiny)
library(shinyjs)
ui <- function(input, ouput) {
  shiny::fluidPage(
    shiny::div(id = "container", style = "width:100%;height:100%"),
    shinyjs::useShinyjs()
  )
}
server <- function(input, output, session) {
  shiny::observe({
    testdata <- palmerpenguins::penguins_raw
    g <- nightowl::boxplot(testdata,
      x = "Sex",
      y = "Culmen Depth (mm)",
      add_violin = T,
      add_boxplot = F,
      points_size = 3,
      facet_row = c("Sex", "Region"),
      remove_missing = F,
      plot_height = T # This is an additonal parameter that is not needed
    )
    svg <- nightowl::render_svg(g)
    shinyjs::html("container", svg)
  })
}
shiny::shinyApp(ui, server)
# ===============================================================================
