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
library(scales)
library(lubridate)

# Manzanita data organization

## Read in data
manzanita_veg_master <- read_csv("manzanita_03_06_copy.csv")
metadata_master <- read_csv("manz_metadata.csv")
manzanita_hydro_master <- read_csv("manzanita_hydro_02_06.csv")


## Convert manzanita_hydro_master into tidy format, add year column
manzanita_hydro <- manzanita_hydro_master %>% 
  gather(key = "Pool", value = "Depth", -c(start_date, full_date)) %>% 
  separate(., full_date, c("month", "day", "year", sep = "/")) %>% 
  mutate(year = as.numeric(year) + 2000)


## Reformat metadata so it can be full_join-ed to vegetation data
metadata <- metadata_master %>% 
  rename(Species_Abbr = Species, Species = Species_Full_Name)

## Convert manzanita_03_06 into tidy format, join with species metadata, convert day/month/year columns to "date" column, replace NAs with 0s
manzanita_veg <- gather(filter(manzanita_veg_master, Pool != "NA"), key = "Transect_Distance", value = "Percent_Cover", -c(Pool, Day, Month, Year, Species, Transect_Direction))
manzanita_veg <- full_join(manzanita_veg, metadata) %>% 
  mutate(date = paste(Month, Year, sep="-" )) %>% 
  mutate_all(funs(replace(., is.na(.), 0)))


## Make data frame for total native cover and total exotic cover
ne_df <- manzanita_veg %>% 
  filter(Percent_Cover != "x") %>% 
  filter(Pool != "0") %>% 
  mutate(date = as.Date(date, "%m/%Y")) %>% 
  mutate(Percent_Cover = as.numeric(Percent_Cover)) %>% 
  group_by(Pool, Species, Native_Status, Year) %>% 
  summarize(
    mean = mean(Percent_Cover)
  ) %>% 
  group_by(Pool, Native_Status, Year) %>% 
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
                      sidebarLayout(
                        sidebarPanel(width = 5, h2("Map of Manzanita Village Vernal Pools"), img(src="manzanita_map.png", align = "center", height = 439, label = "Manzanita")
                        ),
                      mainPanel(width = 5,
                      h1("Summary"),
                      h2("Introduction"),
                      p("Vernal pool restoration around UCSB has been ocurring since the mid-1980s.  The Cheadle Center for Biodiversity Ecological Restoration (CCBER) was formed in 2005 as the official mitigation engine for UCSB.  As UCSB continues to expand its campus and student/faculty housing, CCBER has been tasked with implementing restoration projects as mitigation."),
                      p("The vernal pools at Manzanita Village were created in 2005."),
                      h2("How to Use This App"),
                      p("...")
                      
             ))),
             
             # Tab 2: Hydroperiod line graphs
             
             tabPanel("Pool Hydroperiod",
                      h1("Pool Hydroperiod"),
                      h2("Hydroperiod Data"),
                      p("Hydroperiod data is collected on a weekly basis during the rainy season by recording the water level based on a measuring gauge installed in the deepest point in each pool."),
                      
                      # Sidebar with a select input for pool and date range input 
                      sidebarLayout(
                        sidebarPanel(
                          
                          selectInput("hydro_pool", 
                                      "Select vernal pool:",
                                      c("San Miguel","Santa Rosa","Santa Cruz", "Santa Barbara", "Santa Catalina")),
                          radioButtons("year_hydro", 
                                       "Select year:",
                                       c("2003","2004", "2005", "2006")
                          ),
                          
                          hr(),
                          fluidRow(column(4, verbatimTextOutput("value")))
                        ),
                        
                        # Show a plot of the generated hydroperiod
                        mainPanel(
                          plotOutput("hydroperiod")
                        )
                      )),
             
             # Tab 3: Vegetation column graphs
  
  tabPanel("Vegetation Composition",
           h1("Vegetation Composition"),
           h2("Vegetation Monitoring Data"),
           p("Vegetation data is collected in the summer via transect sampling.  Each pool has a permanent transect running along the diameter of the pool.  Meter-square quadrats are placed every 2 meters.  In each quadrat, percent cover of each plant species present is recorded."),
           p("In the graph below, the arithmetic mean percent cover of each species over all transect samples is shown."),
           
           # Sidebar with a select input for pool and radio button input for year 
           sidebarLayout(
             sidebarPanel(
               
               selectInput("veg_pool", 
                           "Select vernal pool:",
                           c("San Miguel","Santa Rosa","Santa Cruz", "Santa Barbara", "Santa Catalina")
               ),
               radioButtons("year", 
                           "Select year:",
                           c("2003","2004", "2005", "2006")
               ),
               radioButtons("native_status", 
                            "Do you want to see data on Native species (N) or Exotic species (E)?",
                            c("N","E")
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
               selectInput("ne_pool", 
                           "Select vernal pool:",
                           c("San Miguel","Santa Rosa","Santa Cruz", "Santa Barbara", "Santa Catalina")),
               sliderInput("ne_dates",
                              label = "Select date range:",
                              min = 2003,
                              max = 2006,
                              value = c(2003, 2006),
                           sep = "")
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
      filter(Pool == input$hydro_pool) %>% 
      mutate(date = as.Date(start_date, "%m/%d/%y")) %>% 
      filter(year == input$year_hydro)
    
      ggplot(hydro, aes(x = date, y = Depth)) +
      geom_path(color = "darkblue", size = 1) +
      labs(title = "Hydroperiod", y = "Depth (in)", x = "Date") +
      theme_classic() +
      scale_y_continuous(expand = c(0,0), lim = c(0, 17)) +
      scale_x_date(breaks = waiver(), date_breaks = "1 month") +
      theme_classic() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 15), axis.text.y = element_text(size = 15), axis.title = element_text(size = 20), plot.title = element_text(size = 25))
  })
  
  
  
  output$veg_col <- renderPlot({
    # generate pool based on input$pool from ui.R
    veg <-  manzanita_veg %>% 
      filter(Pool == input$veg_pool) %>% 
      filter(Percent_Cover != "x") %>% 
      mutate(Percent_Cover = as.numeric(Percent_Cover)) %>% 
      group_by(Pool, Species, date, Native_Status, Year) %>% 
      summarize(
        mean = mean(Percent_Cover)
      ) %>% 
      filter(Year == input$year) %>% 
      filter(Native_Status == input$native_status) %>% 
      filter(mean >0)
    
    ggplot(veg, aes(x = reorder(Species, -mean), y = mean)) +
      geom_col(fill = "seagreen3") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 10), axis.text.y = element_text(size = 15), axis.title = element_text(size = 20), plot.title = element_text(size = 25)) +
      scale_y_continuous(expand = c(0,0)) +
      labs(title = "Species Percent Cover", x = "Species", y = "Mean Percent Cover")
    
  })
  
  
  
  output$veg_line <- renderPlot({
    ne <- ne_df %>% 
      filter(Pool == input$ne_pool) %>% 
      filter(Year >= input$ne_dates[1] & Year <= input$ne_dates[2])
    
    
      ggplot(ne, aes(x = as.character(Year), y = total)) +
      geom_line(aes(color = Native_Status, group = Native_Status), size = 2) +
      theme_classic() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 15), axis.text.y = element_text(size = 15), axis.title = element_text(size = 20), plot.title = element_text(size = 25)) +
      scale_color_manual(values = c("firebrick", "darkgreen"), name = "Native Status", label = c("Exotic", "Native")) +
      scale_y_continuous(expand = c(0,0)) +
      labs(title = "Percent Cover of Natives and Exotics", x = "Year", y = "Percent Cover")
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

