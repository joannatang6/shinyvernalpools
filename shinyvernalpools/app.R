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
library(knitr)
library(kableExtra)

# Manzanita data organization

## Read in data
manzanita_veg_master <- read_csv("manzanita_03_06_copy.csv")
metadata_master <- read_csv("manz_metadata.csv")
manzanita_hydro_master <- read_csv("manzanita_hydro_02_03.csv")


## Reformat metadata so it can be full_join-ed to data
metadata <- metadata_master %>% 
  rename(Species_Abbr = Species, Species = Species_Full_Name)

## Convert manzanita_03_06 into tidy format, join with species metadata, convert day/month/year columns to "date" column, replace NAs with 0s
manzanita_veg <- gather(filter(manzanita_veg_master, Pool != "NA"), key = "Transect_Distance", value = "Percent_Cover", -c(Pool, Day, Month, Year, Species, Transect_Direction))
manzanita_veg <- full_join(manzanita_veg, metadata) %>% 
  mutate(date = paste(Month, Year, sep="-" )) %>% 
  mutate_all(funs(replace(., is.na(.), 0)))


## Convert manzanita_hydro_master into tidy format
manzanita_hydro <- manzanita_hydro_master %>% 
  gather(key = "Pool", value = "Depth", -c(start_date, end_date))

## Make data frame for total native cover and total exotic cover
ne_df <- manzanita_veg %>% 
  filter(Percent_Cover != "x") %>% 
  filter(Pool != "0") %>% 
  mutate(Percent_Cover = as.numeric(Percent_Cover)) %>% 
  group_by(Pool, Species, date, Native_Status, Year) %>% 
  summarize(
    mean = mean(Percent_Cover)
  ) %>% 
  group_by(Pool, Native_Status, date, Year) %>% 
  summarize(
    total = sum(mean)
  ) %>%
  filter(Native_Status != 0)


# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  
  # Application title
  titlePanel("CCBER's Manzanita Village Vernal Pools"),
  
  navbarPage("Welcome!",
             
             # Tab 1: Summary and map
             
             tabPanel("Summary",
                      h1("Summary"),
                      h2("Summary"),
                      p("Vernal pool restoration around UCSB has been ocurring since the mid-1980s.  The Cheadle Center for Biodiversity Ecological Restoration (CCBER) was formed in 2005 as the official mitigation engine for UCSB.  As UCSB continues to expand its campus and student/faculty housing, CCBER has been tasked with implementing restoration projects as mitigation."),
                      p("The vernal pools at Manzanita Village were created in 2005."),
                      h2("Map of Manzanita Village Vernal Pools"),
                      img(src="manzanita_map.png", align = "left", height = 500)
                      
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
                                      c("San Miguel","Santa Rosa","Santa Cruz", "Santa Barbara", "Santa Catalina")),
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
           p("In the graph below, the arithmetic mean percent cover of each species over all transect samples is shown."),
           
           # Sidebar with a select input for pool and radio button input for year 
           sidebarLayout(
             sidebarPanel(
               
               selectInput("pool", 
                           "Select vernal pool:",
                           c("San Miguel","Santa Rosa","Santa Cruz", "Santa Barbara", "Santa Catalina")
               ),
               selectInput("year", 
                           "Select year:",
                           c("2002","2003","2004", "2005", "2006")
               )
             ),
             
             # Show a plot of the generated column graph
             mainPanel(
               plotOutput(outputId = "veg_col")
             )
             
             )),
  
  
  
  
  # Tab 5: Native vs. exotic species line graphs
  
  tabPanel("Native vs. Exotic Vegetation Trends",
           h1("Native vs. Exotic Vegetation Trends"),
           h2("Vegetation Monitoring Data"),
           p("Vegetation data is collected in the summer via transect sampling.  Each pool has a permanent transect running along the diameter of the pool.  Meter-square quadrats are placed every 2 meters.  In each quadrat, percent cover of each plant species present is recorded."),
           p("In the graph below, the total percent cover is calculated by summing the arithmetic mean percent cover of each species over all transect samples."),
           
           # Sidebar with a select input for pool and radio button input for year 
           sidebarLayout(
             sidebarPanel(
               selectInput("pool", 
                           "Select vernal pool:",
                           c("San Miguel","Santa Rosa","Santa Cruz", "Santa Barbara", "Santa Catalina")),
               dateRangeInput("dates",
                              label = "Select date range")
             ),
             
             # Show a plot of the generated column graph
             mainPanel(
               plotOutput(outputId = "veg_line")
               
             )
           ))
  
  
  
  
  
  
  ))



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  output$hydroperiod <- renderPlot({
    # generate pool based on input$pool from ui.R
    hydro <- manzanita_hydro %>% 
      filter(Pool == input$pool)
    
      ggplot(hydro, aes(x = start_date, y = Depth)) +
      geom_col(fill = "darkblue") +
      geom_path(aes(group = NULL)) +
      labs(title = "Hydroperiod", y = "Depth (in)", x = "Date") +
      theme_classic() +
      scale_y_continuous(expand = c(0,0), lim = c(0, 17)) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90))
  })
  
  
  
  output$veg_col <- renderPlot({
    # generate pool based on input$pool from ui.R
    veg <- manzanita_veg %>% 
      filter(Pool == input$pool) %>% 
      filter(Percent_Cover != "x") %>% 
      mutate(Percent_Cover = as.numeric(Percent_Cover)) %>% 
      group_by(Pool, Species, date, Native_Status, Year) %>% 
      summarize(
        mean = mean(Percent_Cover)
      ) %>% 
      filter(Year == "2005") %>% 
      filter(Native_Status == "N" | Native_Status == "E")
    
    ggplot(veg, aes(x = Species, y = mean)) +
      geom_col(aes(fill = Native_Status)) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90)) +
      scale_fill_manual(values = c("firebrick", "darkgreen"), name = "Native Status", label = c("Exotic", "Native")) +
      scale_y_continuous(expand = c(0,0)) +
      labs(title = "Percent Cover", x = "Species", y = "Mean Percent Cover")
    
  })
  
  
  
  output$veg_line <- renderPlot({
    ne <- ne_df %>% 
      filter(Pool == input$pool)
      
      ggplot(ne, aes(x = date, y = total)) +
      geom_line(aes(color = Native_Status, group = Native_Status)) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90)) +
      scale_color_manual(values = c("firebrick", "darkgreen"), name = "Native Status", label = c("Exotic", "Native")) +
      scale_y_continuous(expand = c(0,0)) +
      labs(title = "Total Percent Cover of Natives and Exotics", x = "Date", y = "Percent Cover")
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

