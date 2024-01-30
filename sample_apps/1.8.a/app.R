#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("binsx",
                  "Waiting bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      sliderInput("binsy",
                  "Eruption bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("scatterplot", brush = "plot_brush"),
      plotOutput("distPlot1"),
      plotOutput("distPlot2")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$scatterplot <- renderPlot({
    # draw the histogram with the specified number of bins
    ggplot(faithful, aes(x = eruptions, y = waiting)) + 
      geom_point() 
  })
  
  subset <- reactive({
    if(is.null(input$plot_brush) || 
       nrow(brushedPoints(faithful, input$plot_brush)) < 2 ){
      faithful
    }else{
      brushedPoints(faithful, input$plot_brush)             
    } 
  })
  
  output$distPlot1 <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- subset()[, 2]
    binsx <- seq(min(x), max(x), length.out = input$binsx + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = binsx, col = 'darkgray', border = 'white',
         xlab = 'Waiting time to next eruption (in mins)',
         main = 'Histogram of waiting times')
  })
  
  output$distPlot2 <- renderPlot({
    # generate bins based on input$bins from ui.R
    y    <- subset()[, 1]
    binsy <- seq(min(y), max(y), length.out = input$binsy + 1)
    
    # draw the histogram with the specified number of bins
    hist(y, breaks = binsy, col = 'darkgray', border = 'white',
         xlab = 'Waiting time to next eruption (in mins)',
         main = 'Histogram of waiting times')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
