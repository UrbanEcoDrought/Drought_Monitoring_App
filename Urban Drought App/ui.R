####################################################################################################################

#Purpose: Be a visualization portal for the Urban Drought Project, meant to be a real time portal that is updated regularly
#Pulls from NDVI_drought_monitoring workflow & UrbanDrought_SpatialAnalysis_Chicago workflow

####################################################################################################################

library(shiny)
library(shinydashboard)
library(leaflet)
library(leaflet.extras)
library(sf)
library(tidyverse)
library(ggplot2)
library(DT)
library(lubridate)

#For documentation of this app
#https://docs.google.com/document/d/1I8WkmUjuPLf0SS_IF0F6P97xyH3aQhth8m9iYUQM4hs/edit?usp=sharing

source("Graph_Plotting.R")
source("Helper_Functions_Code.R")


# path.UrbDrought <- "/Users/jocelyngarcia/Library/CloudStorage/GoogleDrive-jgarcia@mortonarb.org/Shared drives/Urban Ecological Drought"
# path.UrbDrought <- "~/Google Drive/Shared drives/Urban Ecological Drought/"

#NDVI file path (Using NDVI data from NDVI Drought Monitoring Workflow so they are fit to the spline)
NDVI_data <- read_csv("data/allNDVI_data.csv")%>%
  mutate(date = as.Date(date, format="%Y-%m-%d"))
NDVI_data$date <- as.Date(NDVI_data$date)


#CSV file path (Using CSV data from NDVI Drought Monitoring Workflow )
CI_csv <- read_csv("data/k=12_norms_all_LC_types.csv")

#putting NDVI_data in order by date
NDVI_data <-NDVI_data[order(as.Date(NDVI_data$date, format="%Y-%m-%d"), decreasing = TRUE), ]

head(NDVI_data)


#Pulling yday of latest data (most recent day) 

#finding latest day & pulling date
latest_day<-head(NDVI_data, 1)
date_needed <-latest_day$date

#pulling any rows with matching date 
most_recent_data<- filter(NDVI_data, date == date_needed)


#Need to run this code before app
lcnames <- c("forest", "crop", "grassland", "urban-high", "urban-medium", "urban-low", "urban-open")

panel_plot_files <- list.files(
  path = file.path(path.UrbDrought, "data/NDVI_drought_monitoring/figures/04_panel_plots_usdm_deviation_meanNDVI"),
  pattern = "\\.png$", 
  full.names = TRUE
)

scatterplot_files <- list.files(
  path = file.path(path.UrbDrought, "data/NDVI_drought_monitoring/figures/06_scatterplots_usdm_deviation_growing_season"),
  pattern = "\\.png$", 
  full.names = TRUE
)

for_files <- c()
crop_files <- c()
grass_files <- c()
uh_files <- c()
um_files <- c()
ul_files <- c()
uo_files <- c()


for (file in panel_plot_files) {
  if (grepl("forest", file)) {
    for_files <- append(for_files, file)
  } else if (grepl("crop", file)) {
    crop_files <- append(crop_files, file)
  } else if (grepl("grassland", file)) {
    grass_files <- append(grass_files, file)
  } else if (grepl("urban-high", file)) {
    uh_files <- append(uh_files, file)
  } else if (grepl("urban-medium", file)) {
    um_files <- append(um_files, file)
  } else if (grepl("urban-low", file)) {
    ul_files <- append(ul_files, file)
  } else if (grepl("urban-open", file)) {
    uo_files <- append(uo_files, file)
  }
}

for (file in scatterplot_files) {
  if (grepl("forest", file)) {
    for_files <- append(for_files, file)
  } else if (grepl("crop", file)) {
    crop_files <- append(crop_files, file)
  } else if (grepl("grassland", file)) {
    grass_files <- append(grass_files, file)
  } else if (grepl("urban-high", file)) {
    uh_files <- append(uh_files, file)
  } else if (grepl("urban-medium", file)) {
    um_files <- append(um_files, file)
  } else if (grepl("urban-low", file)) {
    ul_files <- append(ul_files, file)
  } else if (grepl("urban-open", file)) {
    uo_files <- append(uo_files, file)
  }
}

img_list<- c()

#from https://www.census.gov/geographies/mapping-files/time-series/geo/cartographic-boundary.html
counties <- sf::read_sf("cb_2023_us_county_500k",
                        layer = "cb_2023_us_county_500k")%>% 
  st_transform(crs = 4326)

il_counties <- subset(counties, counties$NAME %in% c(
  "Cook","DuPage","Kane","McHenry","Lake","Will","Kendall") &
    STATE_NAME == "Illinois")

#for heat map

# Join and compute differences
merged_data <- NDVI_data %>%
  left_join(CI_csv, by = c("yday", "type")) %>%
  mutate(difference = NDVIMissionPred - mean)

# Prepare data for heatmap
heatmap_data <- merged_data %>%
  select(NDVIMissionPred, yday, year, difference, mean, lwr, upr, type, date)
heatmap_data$year <- as.numeric(heatmap_data$year)


####################################################################################################################

ui <- dashboardPage(skin = "black",
                    dashboardHeader(
                      title = "Urban Drought Dashboard",
                      titleWidth = 300),
                    dashboardSidebar(
                      width = 300,
                      sidebarMenu(
                        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                        menuItem("Analysis", tabName = "analysis", icon = icon("gears"),
                                 menuSubItem("NDVI Data Review",
                                             tabName = "NDVI_data_review"),
                                 menuSubItem("Heat Maps for each LC Type",
                                             tabName = "ndvi_diff")),
                        menuItem("Specifics", tabName = "specifics", icon = icon("bookmark"))
                      )
                    ),
                    dashboardBody(
                      # Custom CSS
                      tags$head(
                        tags$style(HTML("
        .small-box {
          min-height: 10px !important;  /* Adjust the height of the box */
          width: 150px !important;  /* Adjust the width */
        }
        
        .small-box .inner h3 { 
          font-size: 14px !important;  /* Adjust main value text size */
        }
        
        .small-box .inner p { 
          font-size: 14px !important;  /* Adjust subtitle text size */
        }
      "))
                      ),
                      
                      tabItems(
                        tabItem(tabName = "dashboard",
                                # Row for drought status for each LC type
                                fluidRow(
                                  column(width = 12,
                                         div(
                                           style = "
        display: flex; 
        flex-wrap: wrap;    /* Allows items to wrap to a new line if needed */
        gap: 5px;           /* Spacing between boxes */
        justify-content: space-around; /* or space-between/center, etc. */
      ",
                                           # Let each valueBoxOutput have no fixed Bootstrap column width
                                           valueBoxOutput("cropBox", width = NULL),
                                           valueBoxOutput("forBox", width = NULL),
                                           valueBoxOutput("grassBox", width = NULL),
                                           valueBoxOutput("uhBox", width = NULL),
                                           valueBoxOutput("umBox", width = NULL),
                                           valueBoxOutput("ulBox", width = NULL),
                                           valueBoxOutput("uoBox", width = NULL)
                                         )
                                  )
                                )
                                ,
                                
                                # Map layout
                                fluidRow(
                                  column(width = 5, 
                                         leafletOutput("il_county_map", height = "350px")),
                                  #Density plot graphs, put here for formatting
                                  tabBox(
                                    title = tagList(shiny::icon("bars"), "General Information"),
                                    width = 7,
                                    tabPanel(
                                      "Latest Data Report",
                                      h5(tags$b(paste("Most Recent Data is from", format(date_needed, "%B %d, %Y")))),
                                      br(),
                                      h5(HTML("<u>Percentiles for Current NDVI data:</u>")),
                                      # Proper text output for the percentile value
                                      h5(textOutput("percentile_crop")),  
                                      h5(textOutput("percentile_for")),  
                                      h5(textOutput("percentile_grass")),  
                                      h5(textOutput("percentile_uh")),  
                                      h5(textOutput("percentile_um")),  
                                      h5(textOutput("percentile_ul")),
                                      h5(textOutput("percentile_uo")),  
                                      br(),
                                      
                                      p("For a more in-depth exploration, take a look at the other tabs or the directory.")
                                    ),
                                    tabPanel(
                                      "Status Key",
                                      h5(HTML("<span style='color:blue;'>Blue</span> = NDVI is at or Above Upper Bound of CI")),
                                      h5(HTML("<span style='color:yellow;'>Yellow</span> = NDVI is between Upper & Lower Bound of CI")),
                                      h5(HTML("<span style='color:orange;'>Orange</span> = NDVI is below Lower Bound of CI")),
                                      h5(HTML("Status (Most Recent NDVI Value - Mean) is also listed in each Land Cover Status Box<br><br>")),
                                      HTML("<img src='NDVI_Categories.png' alt='NDVI Categories' style='width:60%;'>")
                                    )
                                    
                                    ,
                                    tabPanel(
                                      "Directory",
                                      h4("Dashboard"),
                                      HTML(
                                        "<ul>
      <li>Map of LC Types</li>
      <li>Distribution Plots of LC Types</li>
      <li>Status of LC Types</li>
    </ul>"      ),
                                      h4("Analysis"),
                                      HTML(
                                        "<ul>
      <li>Yearly, Monthly & Weekly NDVI Graphs</li>
      <li>Distribution Plot Statistics</li>
    </ul>"      ),
                                      h4("Specifics"),
                                      HTML(
                                        "<ul>
      <li>Grant Information & Contributors</li>
      <li>Github & Documentation Links</li>
      <li>Workflows</li>
    </ul>"      )
                                    )
                                  )),
                                fluidRow(
                                  #Density plot graphs, put here for formatting
                                  tabBox(
                                    width = 12,
                                    tabPanel(
                                      "Crop Density Plot",
                                      plotlyOutput("crop_density_plot"),
                                      textOutput("crop_daily")
                                    ),
                                    tabPanel(
                                      "Forest Density Plot",
                                      plotlyOutput("forest_density_plot"),
                                      textOutput("for_daily")
                                    ),
                                    tabPanel(
                                      "Grassland Density Plot",
                                      plotlyOutput("grassland_density_plot"),
                                      textOutput("grass_daily")
                                    ),
                                    tabPanel(
                                      "Urban-High Density Plot",
                                      plotlyOutput("uh_density_plot"),
                                      textOutput("uh_daily")
                                    ),
                                    tabPanel(
                                      "Urban-Medium Density Plot",
                                      plotlyOutput("um_density_plot"),
                                      textOutput("um_daily")
                                    ),
                                    tabPanel(
                                      "Urban-Low Density Plot",
                                      plotlyOutput("ul_density_plot"),
                                      textOutput("ul_daily")
                                    ),
                                    tabPanel(
                                      "Urban-Open Density Plot",
                                      plotlyOutput("uo_density_plot"),
                                      textOutput("uo_daily")
                                    )
                                  )
                                  
                                )
                                
                        ),
                        tabItem(tabName = "NDVI_data_review",
                                # NDVI Data Tab Box
                                tabBox(
                                  title = "NDVI Data",
                                  id = "tab1",
                                  height = "250px",
                                  width = 12, 
                                  tabPanel("Full Review", plotOutput("all_data_graph")),
                                  tabPanel("Yearly", plotOutput("yearly_graph"),
                                           dateInput(inputId = "start_date", label = "Enter Start Date", value = Sys.Date() - 365)),
                                  tabPanel("Monthly", plotOutput("monthly_graph"),
                                           dateInput(inputId = "mstart_date", label = "Enter Start Date", value = Sys.Date() %m-% months(1))),
                                  tabPanel("Weekly", plotOutput("weekly_graph"),
                                           h5(HTML("If the graph isn't populating there might not be enough data currently, try an early date")),
                                           dateInput(inputId = "wstart_date", label = "Enter Start Date", value = Sys.Date() - 7))
                                )
                        ),
                        tabItem(tabName = "ndvi_diff",
                                h5(HTML("Check Status Key Tab on Dashboard Page for NDVI Catergories Visual (shows ranges for color system being used)")),
                                checkboxGroupInput("selected_years", "Select Years:", 
                                                   choices = unique(heatmap_data$year), 
                                                   selected = unique(heatmap_data$year)[1:5],
                                                   inline = TRUE),  # Default: first 5 years
                                plotOutput("ndvi_heatmap_crop"),
                                plotOutput("ndvi_heatmap_forest"),
                                plotOutput("ndvi_heatmap_grass"),
                                plotOutput("ndvi_heatmap_uh"),
                                plotOutput("ndvi_heatmap_um"),
                                plotOutput("ndvi_heatmap_ul"),
                                plotOutput("ndvi_heatmap_uo")
                        ),
                        tabItem(tabName = "specifics",
                                tabBox(
                                  height = "250px",
                                  width = 12,
                                  tabPanel("Grant Information",
                                           h5(HTML("This research was supported by NIDIS through the FY 2022 Coping with Drought Competition - Ecological Drought (Award NA22OAR4310233).<br><br>
                                                   The Shiny App serves as a near real-time portal, providing a comprehensive view of the current conditions across seven land cover types: 
                                                   crop, forest, grassland, urban-high, urban-medium, urban-low, and urban-open.
                                                   Additional tabs are included to facilitate further analysis and research, broadening the scope of exploration."))
                                  ),
                                  tabPanel("Contributors",
                                           h5(HTML("Jocelyn Garcia, The Morton Arboretum (jgarcia@mortonarb.org)<br><br>
                                                    Juliana Harr, The Morton Arboretum (jharr@mortonarb.org)<br><br>
                                                    Christine R. Rollinson, The Morton Arboretum (crollinson@mortonarb.org)"))
                                  ),
                                  tabPanel("Links to Github",
                                           h5(HTML("Github Links: <br><br>
<p> <a href='https://github.com/UrbanEcoDrought'>UrbanEcoDrought Repository</a><br><br>
    <a href='https://github.com/UrbanEcoDrought/NDVI_drought_monitoring'>NDVI_Drought_Monitoring</a><br><br>
    <a href='https://github.com/UrbanEcoDrought/UrbanDrought_SpatialAnalysis_Chicago'>UrbanDrought_SpatialAnalysis_Chicago Workflow</a><br><br> 
    <a href='https://github.com/UrbanEcoDrought/UrbanDrought_SpatialAnalysis_Chicago/tree/main/NDVI_Automation_Workflow'>NDVI_Automation_Workflow</a><br><br>
    <a href='https://github.com/UrbanEcoDrought/UrbanDrought_SpatialAnalysis_Chicago/tree/main/Urban%20Drought%20App'>Urban Drought App</a><br><br><br></p>")),
                                           h5(HTML("Documentation Link: <a href='https://docs.google.com/document/d/1I8WkmUjuPLf0SS_IF0F6P97xyH3aQhth8m9iYUQM4hs/edit?usp=sharing'>Urban Drought Portal Documentation</a>"))
                                  )
                                )
                        )
                      )
                    )
                    
                    
                    
)


# Run the application
# shinyApp(ui = ui, server = server)

