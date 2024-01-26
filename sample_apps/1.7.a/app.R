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

# Make cereal shelf a factor variable to make colors consistent.
shelves <- c("Top", "Middle", "Bottom")

cereal <- cereal |>
  mutate(Shelf = factor(Shelf, levels = shelves))

# Make a list of the relevant variables. 
tcol <- cereal |> 
  select(-Shelf, -Name, -Manufacturer, -Type) |>
  colnames()

colors = c("#1b9e77", "#d95f02", "#7570b3")

# function to extract range of density values and return the max
tdens <- function(x){
  max(density(x)$y)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Cereal Distribution Comparisons"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("tvar", "Variable - x", choices = tcol),
      selectInput("tvar2", "Variable - y", choices = tcol),
      checkboxGroupInput("shelves", 
                         "Shelves", 
                         shelves, inline = TRUE,
                         selected = shelves)
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
  tvar2 <- reactive(input$tvar2)
  rshelves <- reactive(input$shelves)
  
  # Retain only the colors we need so color matches regardless. 
  color_sub <- reactive({
    match(rshelves(), shelves)
  })
  
  
  tdat <- reactive({
    cereal |>
      select(all_of(tvar()), all_of(tvar2()), Shelf) |>
      tidyr::drop_na()
  })
  
  xrange <- reactive({
    range(tdat()[, tvar()])
  })
  
  yrange <- reactive({
    range(tdat()[, tvar2()])
  })
  
  # Help from chatgpt on .data[[]] syntax
  output$distPlot <- renderPlot({
    tdat() |>
      dplyr::filter(Shelf %in% rshelves()) |> 
      ggplot(aes(x = .data[[input$tvar]], 
                 y = .data[[input$tvar2]],
                 color = Shelf)) + 
      geom_point() + 
      geom_smooth(se = FALSE, stat = "lm") + 
      scale_x_continuous(limits = xrange()) +
      scale_y_continuous(limits = yrange()) + 
      scale_color_manual(values = colors[color_sub()])
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
