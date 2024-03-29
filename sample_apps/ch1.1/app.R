#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Nice to Meet You"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          textInput("name", "What's your name?")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          textOutput("greeting")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$greeting <- renderText({
    paste0("Hello ", input$name)
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
