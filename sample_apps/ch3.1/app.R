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
  textInput("name", "What's your name?"),
  textOutput("greeting")
)

server1 <- function(input, output, server) {
  output$greeting <- renderText(paste0("Hello ", input$name))
}

server2 <- function(input, output, server) {
  greeting <- reactive(paste0("Hello ", input$name))
  output$greeting <- renderText(greeting())
}

server3 <- function(input, output, server) {
  output$greeting <- renderText(paste0("Hello ", input$name))
}

# Run the application 
shinyApp(ui = ui, server = server3)
