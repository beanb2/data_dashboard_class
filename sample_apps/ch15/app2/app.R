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

# Define server logic required to draw a histogram
server <- function(input, output) {

  observeEvent(input$capture, {
    output$out <- renderText(input$x)
  })

  
}

# Run the application 
shinyApp(ui = ui, server = server)
