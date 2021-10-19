library(shiny)
library(shinyjs)
ui <- function(input, ouput) {
  shiny::fluidPage(
    shiny::div(id = "container", style="width:100%;height:100%"),
    shinyjs::useShinyjs()
  )
}
server <- function(input, output, session) {
  shiny::observe({
    testdata <- palmerpenguins::penguins_raw
    g <- nightowl::boxplot(testdata,
      x = "Species",
      y = "Culmen Depth (mm)",
      add_violin = T,
      add_boxplot = F,
      points_size = , 3,
      facet_row = c("Sex", "Island")
    )
    s <- svglite::svgstring()
    print(g)
    dev.off()
    shinyjs::html("container", s())
  })
}
shiny::shinyApp(ui, server)
#===============================================================================
