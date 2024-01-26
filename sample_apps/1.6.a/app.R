#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(stat1080r)
library(ggplot2)
library(dplyr)
library(tidyr)

tcol <- cereal |> 
  select(-Shelf, -Name, -Manufacturer, -Type) |>
  colnames()

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Cereal Distribution Comparisons"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("tvar", "Variable", choices = tcol)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  tvar <- reactive(input$tvar)
  
  # Help from chatgpt on .data[[]] syntax
  output$distPlot <- renderPlot({
    cereal |>
      select(.data[[tvar()]], Shelf) |>
      tidyr::drop_na() |>
    ggplot(aes(x = .data[[input$tvar]], color = Shelf)) + 
      geom_density()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
