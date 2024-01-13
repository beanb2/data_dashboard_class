#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

headings <- list()
headings$odd <- seq(1, 19, 2)
headings$even <- seq(2, 20, 2)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      # First input is from the shiny app template. 
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      
      # Answer to Problem 1, Section 2.2.8. 
      textInput("name", "", value = "Your Name"),
      
      # Answer to Problem 2, Section 2.2.8. 
      sliderInput("delivery", "When should we deliver?",
                  min = as.Date("2020-09-16"),
                  max = as.Date("2020-09-23"),
                  value = as.Date("2020-09-17")),
      
      # Answer to Problem 3, Section 2.2.8. 
      sliderInput("animate", "Animate", 
                  min = 0, max = 100, step = 5,
                  animate = TRUE, value = 0),
      
      # Answer to Problem 4, Section 2.2.8 (see headings list above). 
      selectInput("select", "Select Input", choices = headings)
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      # Adapted answer to problem 2, Section 2.3.5 (see also renderPlot)
      plotOutput("distPlot", width = "700px", height = "300px"),
      dataTableOutput("table")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Adapted answer to problem 2, Section 2.3.5 (see also plotOutput)
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white',
         xlab = 'Waiting time to next eruption (in mins)',
         main = 'Histogram of waiting times')
  }, res = 96, 
  alt = "Histogram of waiting times between Old Faithful eruptions")
  
  # Answer to problem 3, Section 2.3.5 
  # Updated answer requires installing the DT package from CRAN.
  output$table <- DT::renderDataTable(mtcars, 
                                      options = list(pageLength = 5, 
                                                     ordering = FALSE,
                                                     searching = FALSE,
                                                     ))
  
  # (note that problem 4 is skipped)
}

# Run the application 
shinyApp(ui = ui, server = server)