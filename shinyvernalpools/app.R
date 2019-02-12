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
library(RColorBrewer)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  
  # Application title
  titlePanel("CCBER Vernal Pools"),
  
  navbarPage("Welcome!",
             
             # Tab 1: Summary and map
             
             tabPanel("Summary/Map",
                      h1("Summary/Map"),
                      h2("Summary"),
                      p("The Cheadle Center for Biodiversity Ecological Restoration (CCBER) has been creating and restoring vernal pools in and around UCSB since the mid-1980s."),
                      p("There are 6 sites with vernal pools: Manzanita, West Campus Bluffs, North Parcel, South Parcel, North Campus Open Space, and Sierra Madre."),
                      h2("Map of CCBER Vernal Pools"),
                      p("Insert map...")
                      
             ),
             
             
             # Tab 2: Hydroperiod line graphs
             
             tabPanel("Pool Hydroperiod",
                      h1("Pool Hydroperiod"),
                      h2("Hydroperiod Data"),
                      p("Hydroperiod data is collected on a weekly basis during the rainy season by recording the water level based on a measuring gauge installed in the deepest point in each pool."),
                      
                      # Sidebar with a select input for pool and date range input 
                      sidebarLayout(
                        sidebarPanel(
                          
                          selectInput("pool", 
                                      "Select vernal pool:",
                                      choices = c("Manzanita: San Miguel","Manzanita: Santa Rosa","Manzanita: Santa Cruz", "Manzanita: Santa Barbara", "Manzanita: Santa Catalina")),
                          dateRangeInput("dates",
                                         label = "Select date range"),
                          
                          hr(),
                          fluidRow(column(4, verbatimTextOutput("value")))
                        ),
                        
                        # Show a plot of the generated hydroperiod
                        mainPanel(
                          plotOutput("hydroperiod")
                        )
                      )),
             
             
             # Tab 3: Vegetation column graphs

             tabPanel("Pool Vegetation Composition",
                      h1("Pool Vegetation Composition"),
                      h2("Vegetation Monitoring Data"),
                      p("Vegetation data is collected in the summer via transect sampling.  Each pool has a permanent transect running along the diameter of the pool.  Meter-square quadrats are placed every 2 meters.  In each quadrat, percent cover of each plant species present is recorded."),
                      
                      # Sidebar with a select input for pool and radio button input for year 
                      sidebarLayout(
                        sidebarPanel(
                          
                          selectInput("pool", 
                                      "Select vernal pool:",
                                      choices = c("Manzanita: San Miguel","Manzanita: Santa Rosa","Manzanita: Santa Cruz", "Manzanita: Santa Barbara", "Manzanita: Santa Catalina")),
                          
                          radioButtons("year", 
                                       "Select year:",
                                       choices = c("2005","2006","2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018"))
                        ),
                        
                        # Show a plot of the generated column graph
                        mainPanel(
                          plotOutput("veg_col")
                        )
                      )),
             
             
             # Tab 4: North Parcel
             
             tabPanel("North Parcel",
                      h1("North Parcel"),
                      h2("Hydroperiod Data"),
                
                      
                      # Sidebar with a select input for pool and date range input 
                      sidebarLayout(
                        sidebarPanel(
                          
                          selectInput("pool", 
                                      "Select vernal pool:",
                                      choices = c("Phase 1", "Tadpole", "Redtail", "Whitetail 1", "Whitetail 2", "Creekside", "Mini South")),
                          dateRangeInput("dates",
                                         label = "Select date range"),
                          
                          hr(),
                          fluidRow(column(4, verbatimTextOutput("value"))),
                          radioButtons("graph", 
                                       "Select graph:",
                                       choices = c("Hydroperiod", "Vegetation Composition"))
                        ),
                        
                        # Show a plot of the generated hydroperiod
                        mainPanel(
                          plotOutput("graph")
                        )
                      ))
             
  )
  
)









# Define server logic required to draw a line graph
server <- function(input, output) {
  
  output$hydroperiod <- renderPlot({
    # generate pool based on input$pool from ui.R (X replace faithful with df)
    ggplot(faithful, aes(x = waiting, y = eruptions)) +
      geom_point(color = input$year) +
      geom_line(color = input$year)
    
  })
  
  output$veg_col <- renderPlot({
    # generate pool based on input$pool from ui.R (X replace faithful with df)
    ggplot(faithful, aes(x = waiting, y = eruptions)) +
      geom_col(color = input$year)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

