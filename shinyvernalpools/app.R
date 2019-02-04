#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  
  # Application title
  titlePanel("CCBER Vernal Pools"),
  
  navbarPage("Welcome!",
             
             tabPanel("Summary/Map",
                      h1("Summary/Map"),
                      h2("Summary"),
                      p("There are 6 sites with vernal pools: Manzanita, West Campus Bluffs, North Parcel, South Parcel, North Campus Open Space, and Sierra Madre."),
                      p("Hydroperiod data is collected on a weekly basis during the rainy season by recording the water level based on a measuring gauge installed in the deepest point in each pool.  Vegetation data is collected in the summer via transect sampling.  Each pool has a permanent transect running along the diameter of the pool.  Meter-square quadrats are placed every 2 meters.  In each quadrat, percent cover of each plant species present is recorded."),
                      h2("Map of CCBER Vernal Pools"),
                      p("Insert map...")
                      
             )
             
  )
  
)









# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = input$color, border = 'white')
  })
  
  output$scatter <- renderPlot({
    
    ggplot(faithful, aes(x = waiting, y = eruptions)) +
      geom_point(color = input$scattercolor) +
      geom_smooth(method = "lm", se = FALSE)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

