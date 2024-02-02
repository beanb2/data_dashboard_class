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
  numericInput("x", "x", value = 50, min = 0, max = 100),
  actionButton("capture", "capture"),
  textOutput("out")
)

server <- function(input, output) {
  
  # Define a reactive value to store the captured value of x
  captured_x <- reactiveVal(NULL)
  
  # Update the captured value of x when the capture button is clicked
  observeEvent(input$capture, {
    captured_x(input$x)
  })
  
  # Render the captured value of x
  output$out <- renderText({
    captured_x()
  })
}

shinyApp(ui = ui, server = server)
