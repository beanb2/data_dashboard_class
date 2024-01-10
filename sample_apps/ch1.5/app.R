library(shiny)
library(ggplot2)

datasets <- c("economics", "faithfuld", "seals") 
ui <- fluidPage(
  selectInput("dataset", "Dataset", choices = datasets),
  verbatimTextOutput("summary"),
  plotOutput("plot") # error 3
)

server <- function(input, output, session) {
  dataset <- reactive({
    get(input$dataset, "package:ggplot2")
  })
  output$summmary <- renderPrint({# error 1
    summary(dataset())
  })
  output$plot <- renderPlot({
    plot(dataset()) # error 2
  }, res = 96)
}

shinyApp(ui, server)