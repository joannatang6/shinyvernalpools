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
                        sidebarPanel(width = 6, h2("Map of Manzanita Village Vernal Pools"), img(src="manzanita_map.png", align = "center", width = 407), img(src = "santabarbara.png", width = 332),
                                     h2("Endangered Vernal Pool Species"), img(src = "limnanthes.png", height = 150, align = "right"), img(src = "pogogyne.png", height = 150), img(src = "fairy.png", height = 150, align = "right"), img(src = "salamander.png", height = 170)
                        ),
                      mainPanel(width = 6,
                      h1("Summary"),
                      h2("Introduction"),
                      h3("CCBER"),
                      p("Vernal pool restoration around UCSB has been occurring since the mid-1980s.  The Cheadle Center for Biodiversity Ecological Restoration (CCBER) was formed in 2005 as the official mitigation engine for UCSB.  As UCSB continues to expand its campus and student/faculty housing, CCBER has been tasked with implementing restoration projects as mitigation.  These restoration projects often involve vernal pool restoratoin, as UCSB is built atop historic vernal pool habitat."),
                      h3("What is a vernal pool?"),
                      p("Vernal pools are seasonally flooded wetlands that form in small depressions in the landscape.  During California's cool, wet winters, rainwater pools in these depressions, where the rainwater is prevented from percolating deep into the soil by an impermeable subsurface soil layer such as a hardpan or a claypan.  These pools of water sustain specifically-adapted vernal pool flora and fauna: endemic flora grow and set seed quickly during the spring before the pool dries up in the summer, and endemic aquatic fauna spring to life during the wet season and go dormant as cysts during the dry season.  Because vernal pools have this highly dynamic seasonality, vernal pool ecosystems harbor a host of native and endangered flora and fauna, such as wooly meadowfoam (Limnanthes floccosa ssp. californica), San Diego mesa mint (Pogogyne abramsii), conservancy fairy shrimp (Branchinecta conservatio), and the California tiger salamander (Ambystoma californiense).  These endemic species are endangered due to habitat loss: 95% of California's historic vernal pool ecosystems have been lost, due to both urban pressures, such as development, agricultural, and grazing, and environmental stressors, such as increased drought and exotic species invasion."),
                      h3("Manzanita Village"),
                      p("Because of this widespread loss of vernal pools, there have been large statewide efforts to restore and create new vernal pools, including CCBER's numerous restoration projects.   The vernal pools at the Manzanita Village student housing complex were created in 2002.  There are five vernal pools, named after the Channel Islands, that are interspersed with other wetland features such as vernal marshes and bioswales.  These vernal pools were excavated, lined with 3 feet of topsoil, and inoculated with seed bank and invertebrate cyst material from local vernal pools.  In addition to this inoculum, over 80,000 native plants of 100 different species were planted in and around the pools.  These pools achieved the restoration goal of over 50% native vegetation cover after four years, and thus this restoration project was adjuged a success."),
                      h2("How to Use This App"),
                      p("This Shiny app displays monitoring information about the five Manzanita vernal pools.  There are three tabs in this app that interactively show three types of data: Pool Hydroperiod, Vegetation Composition, and Native vs. Exotic Vegetation Trends.  These data were collected starting in 2003 in order to assess the success of the created pools.  All data were collected by CCBER staff.  Throughout the tabs, you can compare hydrology and vegetation trends between pools and throughout several years.  Please visit each tab to find out more about the hydrology and vegetation dynamics of the Manzanita vernal pools!")
                      
             ))),
             
             # Tab 2: Hydroperiod line graphs
             
             tabPanel("Pool Hydroperiod",
                      h1("Pool Hydroperiod"),
                      h2("Vernal Pool Hydrology"),
                      p("Because vernal pools only seasonally flood during the rainy season, they have a unique hydrologic regime.  This hydrologic regime gives rise to unique vernal pool plants that are specially adapted to specific periods of inundantion.  In the graph below, you can see this seasonality in Manzanita's vernal pools.  Check out the hydroperiod for each pool, and see how the seasonality of the pool changes from year to year."),
                      h2("Hydroperiod Data Collection"),
                      p("Hydroperiod data is collected on a weekly basis during the rainy season.  Each pool has a measuring gauge installed in its deepest point.  Hydroperiod data is collected by recording the water level in each pool every week.  Data source: CCBER"),
                      
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
           h2("Vernal Pool Vegetation"),
           p("Only certain specially-adapted plants can survive in vernal pools, which are completely flooded with nutrient-poor rainwater during part of the year and then completely dry during the rest of the year.  Southern California vernal pools are dominated by perennial grasses, rushes, and sedges, such as Eleocharis and Juncus species.  You can see the native plant community composition of each pool by selecting the N button below.  However, vernal pools are facing increased exotic species invasion due to changing precipitation -- and subusequently, hydrologic -- patterns.  You can see how Manzanita pools are being invaded by exotic annual grasses, such as Polypogon and Hordeum species, by selecting the E button below.  However, note that the y-axis scale changes when you change your selection.  Thus, you can see that the Manzanita vernal pools are supporting predominantly native plant communities!  Check out how the vegetation composition changes in each pool from year to year."),
           h2("Vegetation Monitoring Data Collection"),
           p("Vegetation data is collected in the summer via transect sampling.  Each pool has a permanent transect running along the diameter of the pool.  Meter-square quadrats are placed every 2 meters.  In each quadrat, percent cover of each plant species present is recorded.  Data source: CCBER"),
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
                            "Select Native species (N) or Exotic species (E):",
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
           h2("Vernal Pool Vegetation"),
           p("Only certain specially-adapted plants can survive in vernal pools, which are completely flooded with nutrient-poor rainwater during part of the year and then completely dry during the rest of the year.  In the graph below, you can see how the total amount of native plants has increased over time.  This graph shows that each pool attained the restoration goal of over 50% cover after four years.  However, vernal pools are facing increased exotic species invasion due to changing precipitation -- and subusequently, hydrologic -- patterns.  You can see this trend of increase in exotic plants in the Manzanita pools, but the pools are still predominantly native.  Check out how the vegetation community changes in each pool over time."),
           h2("Vegetation Monitoring Data Collection"),
           p("Vegetation data is collected in the summer via transect sampling.  Each pool has a permanent transect running along the diameter of the pool.  Meter-square quadrats are placed every 2 meters.  In each quadrat, percent cover of each plant species present is recorded.  Data source: CCBER"),
           p("In the graph below, the total percent cover of natives and exotics is calculated by summing the arithmetic mean percent cover of each species over all transect samples."),
           
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
      geom_line(aes(color = Native_Status, group = Native_Status), size = 1.5) +
      theme_classic() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 15), axis.text.y = element_text(size = 15), axis.title = element_text(size = 20), plot.title = element_text(size = 25)) +
      scale_color_manual(values = c("firebrick", "darkgreen"), name = "Native Status", label = c("Exotic", "Native")) +
      scale_y_continuous(expand = c(0,0)) +
      labs(title = "Percent Cover of Natives and Exotics", x = "Year", y = "Percent Cover")
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

