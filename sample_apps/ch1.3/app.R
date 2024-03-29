library(shiny)

ui <- fluidPage(
  sliderInput("x", "If x is", min = 1, max = 50, value = 30),
  sliderInput("y", "and y is", min = 1, max = 50, value = 5),
  "then, (x * y) is", 
  textOutput("product"),
)

server <- function(input, output, session) {
  output$product <- renderText({ 
    input$x * input$y
  })
}

shinyApp(ui, server)