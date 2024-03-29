#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

ui <- fluidPage(
  checkboxInput("error", "error?"),
  textOutput("result")
)

server <- function(input, output, session) {
  a <- reactive({
    if (input$error) {
      stop("Error!")
    } else {
      1
    }
  })
  b <- reactive(a() + 1)
  c <- reactive(b() + 1)
  output$result <- renderText(c())
}

# Run the application
shinyApp(ui = ui, server = server)
