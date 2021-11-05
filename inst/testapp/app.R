library(shiny)
library(shinyjs)
library(future)
library(promises)
future::plan(future::multisession)
ui <- function(input, ouput) {
  shiny::fluidPage(
    shiny::actionButton("run", "RUN"),
    shiny::div(id = "container", style = "width:100%;height:100%"),
    shinyjs::useShinyjs()
  )
}
server <- function(input, output, session) {
  shiny::observeEvent(input$run, {
    promises::future_promise(
      {
        testdata <- palmerpenguins::penguins_raw
        nightowl::boxplot(testdata,
          x = "Sex",
          y = "Culmen Depth (mm)",
          add_violin = T,
          add_boxplot = F,
          points_size = 3,
          facet_row = c("Sex", "Region"),
          remove_missing = F,
          plot_height = T # This is an additonal parameter that is not needed
        ) %>%
          nightowl::render_svg()
      },
      seed = 99
    ) %...>%
      shinyjs::html(html = ., id = "container")
    shinyjs::html(html = "waiting", id = "container")
  })
}
shiny::shinyApp(ui, server)
# ===============================================================================
