####################################################################################################################

#Purpose: Be a visualization portal for the Urban Drought Project, meant to be a real time portal that is updated regularly
#Pulls from NDVI_drought_monitoring workflow & UrbanDrought_SpatialAnalysis_Chicago workflow

####################################################################################################################

library(shiny);library(shinydashboard);library(shinyBS);library(shinyalert);library(DT);library(lubridate)
library(leaflet);library(leaflet.extras);library(sf);library(tidyverse);library(ggplot2);library(plotly);
library(ggplot2);library(hrbrthemes);library(dplyr);library(tidyverse);library(tidyr)
library(tidyquant);library(scales);library(bs4Dash);library(shinyjs)



#For documentation of this app
#https://docs.google.com/document/d/1I8WkmUjuPLf0SS_IF0F6P97xyH3aQhth8m9iYUQM4hs/edit?usp=sharing

# source("Graph_Plotting.R")
# source("Helper_Functions_Code.R")


# path.UrbDrought <- "/Users/jocelyngarcia/Library/CloudStorage/GoogleDrive-jgarcia@mortonarb.org/Shared drives/Urban Ecological Drought"
# path.UrbDrought <- "~/Google Drive/Shared drives/Urban Ecological Drought/"

################################
#####for testing######
 path.UrbDrought <- "/Users/jocelyngarcia/Library/CloudStorage/GoogleDrive-jgarcia@mortonarb.org/Shared drives/Urban Ecological Drought"
 NDVI_data <- read_csv(file.path(path.UrbDrought, "data/UrbanEcoDrought_NDVI_LocalExtract/allNDVI_data.csv"), locale = locale(encoding = "UTF-8"))
 NDVI_data$date <- as.Date(NDVI_data$date)
 CI_csv <- read_csv(file.path(path.UrbDrought, "data/NDVI_drought_monitoring/k=12_norms_all_LC_types.csv"))
################################
#####Uncomment after testing ######
#####NDVI file path (Using NDVI data from NDVI Drought Monitoring Workflow so they are fit to the spline)
#NDVI_data <- read_csv("data/allNDVI_data.csv")%>%
#mutate(date = as.Date(date, format="%Y-%m-%d"))
#NDVI_data$date <- as.Date(NDVI_data$date)

#CSV file path (Using CSV data from NDVI Drought Monitoring Workflow )
#CI_csv <- read_csv("data/k=12_norms_all_LC_types.csv")
####################
#putting NDVI_data in order by date
NDVI_data <-NDVI_data[order(as.Date(NDVI_data$date, format="%Y-%m-%d"), decreasing = TRUE), ]
#head(NDVI_data)
#Pulling yday of latest data (most recent day) 

#finding latest day & pulling date
latest_day<-head(NDVI_data, 1)
date_needed <-latest_day$date

#pulling any rows with matching date 
most_recent_data<- filter(NDVI_data, date == date_needed)


#Need to run this code before app
lcnames <- c("forest", "crop", "grassland", "urban-high", "urban-medium", "urban-low", "urban-open")

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
                      title = HTML("<span style='font-size: 16px; font-weight: bold;'>Urban Drought Portal BETA</span>"),
                      titleWidth = 200),
                    dashboardSidebar(
                      width = 600,
                      sidebarMenu(
                        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                        menuItem("Analysis", tabName = "analysis", icon = icon("gears"),
                                 menuSubItem("NDVI Data Review",
                                             tabName = "NDVI_data_review"),
                                 menuSubItem("Heat Maps for each LC Type",
                                             tabName = "ndvi_diff")),
                        menuItem("About", tabName = "specifics", icon = icon("bookmark"))
                      )
                    ),
                    dashboardBody(
                      useShinyalert(),
                      useShinyjs(), 
                      tags$head(
                        tags$script(HTML('$(document).ready(function() { 
      $(\'[data-toggle="tooltip"]\').tooltip(); 
    });'))),
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
                                # Title for the status boxes
                                h6(HTML("<b>Drought Status for Each Land Cover Type:</b>"), 
                                   style = "left-align: center; margin-bottom: 20px;"),
                                
                                fluidRow(
                                  column(
                                    width = 12,
                                    div(
                                      style = "
        display: flex; 
        flex-wrap: wrap;    /* Allows items to wrap to a new line if needed */
        gap: 10px;          /* Adjust spacing between boxes */
        justify-content: space-around; /* Ensures equal spacing */
        align-items: center; /* Aligns items vertically */
      ",
                                      # Each valueBoxOutput is treated as a flexible item
                                      div(style = "display: inline-block;", valueBoxOutput("cropBox", width = NULL)),
                                      div(style = "display: inline-block;", valueBoxOutput("forBox", width = NULL)),
                                      div(style = "display: inline-block;", valueBoxOutput("grassBox", width = NULL)),
                                      div(style = "display: inline-block;", valueBoxOutput("uoBox", width = NULL)),
                                      div(style = "display: inline-block;", valueBoxOutput("ulBox", width = NULL)),
                                      div(style = "display: inline-block;", valueBoxOutput("umBox", width = NULL)),
                                      div(style = "display: inline-block;", valueBoxOutput("uhBox", width = NULL))
                                    )
                                  ),
                                ),
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
                                      h6(tags$b(paste("Most Recent Data is from", format(date_needed, "%B %d, %Y")))),
                                      br(),
                                      h6(HTML("<b>The NDVI percentiles below indicate how current vegetation conditions compare to the past two weeks 
                                              of observed data. A lower percentile suggests below-average greenness, while a higher
                                              percentile indicates above-average greenness</b><br>")),
                                      # Proper text output for the percentile value
                                      h6(textOutput("percentile_crop")),  
                                      h6(textOutput("percentile_for")),  
                                      h6(textOutput("percentile_grass")),  
                                      h6(textOutput("percentile_uh")),  
                                      h6(textOutput("percentile_um")),  
                                      h6(textOutput("percentile_ul")),
                                      h6(textOutput("percentile_uo")),  
                                      br(),
                                      p("For a more in-depth exploration, take a look at the other tabs or the directory.")
                                    ),
                                    tabPanel(
                                      "Status Key",
                                      h6(HTML("<span style='color:green;'>Green</span> = Significantly greener than normal")),
                                      h6(HTML("<span style='color:olive;'>Olive</span> = Greener than normal")),
                                      h6(HTML("<span style='color:grey;'>Grey</span> = NDVI is within CI")),
                                      h6(HTML("<span style='color:pink;'>Pink</span> = Browner than normal")),
                                      h6(HTML("<span style='color:maroon;'>Maroon</span> = Significantly browner than normal"))
                                      #h6(HTML("<br>Status (Most Recent NDVI Value - Mean) is also listed in each Land Cover Status Box<br><br>")),
                                      #HTML("<img src='NDVI_Categories.png' alt='NDVI Categories' style='width:60%;'>")
                                    ),
                                    tabPanel(
                                      "Directory",
                                      h6("Dashboard"),
                                      HTML(
                                        "<ul>
      <li>Map of LC Types</li>
      <li>Distribution Plots of LC Types</li>
      <li>Status of LC Types</li>
    </ul>"      ),
                                      h6("Analysis"),
                                      HTML(
                                        "<ul>
      <li>Yearly, Monthly & Weekly NDVI Graphs</li>
      <li>Heat Maps for each LC Type</li>
    </ul>"      ),
                                      h6("About"),
                                      HTML(
                                        "<ul>
      <li>Preliminary Information</li>
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
                                      h6(HTML("<b>This plot shows the distribution of crop NDVI values
                                              over the past two weeks. The x-axis represents NDVI 
                                              (greenness), the y-axis shows frequency, and peaks indicate
                                              the most common values. The current NDVI (Green Diamond) and norm for the Current NDVI yday (Purple Circle) are also displayed.</b><br>")),
                                      plotlyOutput("crop_density_plot"),
                                      textOutput("crop_daily")
                                    ),
                                    tabPanel(
                                      "Forest Density Plot",
                                      h6(HTML("<b>This plot shows the distribution of forest NDVI values
                                              over the past two weeks. The x-axis represents NDVI 
                                              (greenness), the y-axis shows frequency, and peaks indicate
                                              the most common values. The current NDVI (Green Diamond) and 
                                              norm for the Current NDVI yday (Purple Circle) are also displayed.</b><br>")),
                                      plotlyOutput("forest_density_plot"),
                                      textOutput("for_daily")
                                    ),
                                    tabPanel(
                                      "Grassland Density Plot",
                                      h6(HTML("<b>This plot shows the distribution of grassland NDVI values
                                              over the past two weeks. The x-axis represents NDVI 
                                              (greenness), the y-axis shows frequency, and peaks indicate
                                              the most common values. The current NDVI (Green Diamond) and 
                                              norm for the Current NDVI yday (Purple Circle) are also displayed.</b><br>")),
                                      plotlyOutput("grassland_density_plot"),
                                      textOutput("grass_daily")
                                    ),
                                    tabPanel(
                                      "Urban-High Density Plot",
                                      h6(HTML("<b>This plot shows the distribution of Urban-High NDVI values
                                              over the past two weeks. The x-axis represents NDVI 
                                              (greenness), the y-axis shows frequency, and peaks indicate
                                              the most common values. The current NDVI (Green Diamond) and 
                                              norm for the Current NDVI yday (Purple Circle) are also displayed.</b><br>")),
                                      plotlyOutput("uh_density_plot"),
                                      textOutput("uh_daily")
                                    ),
                                    tabPanel(
                                      "Urban-Medium Density Plot",
                                      h6(HTML("<b>This plot shows the distribution of Urban-Medium NDVI values
                                              over the past two weeks. The x-axis represents NDVI 
                                              (greenness), the y-axis shows frequency, and peaks indicate
                                              the most common values. The current NDVI (Green Diamond) and 
                                              norm for the Current NDVI yday (Purple Circle) are also displayed.</b><br>")),
                                      plotlyOutput("um_density_plot"),
                                      textOutput("um_daily")
                                    ),
                                    tabPanel(
                                      "Urban-Low Density Plot",
                                      h6(HTML("<b>This plot shows the distribution of Urban-Low NDVI values
                                              over the past two weeks. The x-axis represents NDVI 
                                              (greenness), the y-axis shows frequency, and peaks indicate
                                              the most common values. The current NDVI (Green Diamond) and 
                                              norm for the Current NDVI yday (Purple Circle) are also displayed.</b><br>")),
                                      plotlyOutput("ul_density_plot"),
                                      textOutput("ul_daily")
                                    ),
                                    tabPanel(
                                      "Urban-Open Density Plot",
                                      h6(HTML("<b>This plot shows the distribution of Urban-Open NDVI values
                                              over the past two weeks. The x-axis represents NDVI 
                                              (greenness), the y-axis shows frequency, and peaks indicate
                                              the most common values. The current NDVI (Green Diamond) and 
                                              norm for the Current NDVI yday (Purple Circle) are also displayed.</b><br>")),
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
                                           h6(HTML("If the graph isn't populating there might not be enough data currently, try an early date")),
                                           dateInput(inputId = "wstart_date", label = "Enter Start Date", value = Sys.Date() - 7))
                                )
                        ),
                        tabItem(tabName = "ndvi_diff",
                                h6(HTML("Check Status Key Tab on Dashboard Page for NDVI Catergories Visual (shows ranges for color system being used)")),
                                # Adjust checkbox size and styling using tags$style
                                tags$style(HTML("
  #selected_years label {
    font-size: 13px;  /* Adjust label text size */
    height: 10px;    /* Adjust height of the label */
  }
  #selected_years input[type='checkbox'] {
    transform: scale(0.8);  /* Resize the checkboxes */
  }
")),
                                # The checkboxGroupInput with custom CSS
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
                                  height = "3000px",
                                  width = 12,
                                  tabPanel("Preliminary Information",
                                           h6(HTML("<b>LC Types</b> = Landcover types (crop, forest, grass/grassland, urban-high, urban-medium, urban-low, urban-open)<br>
                <b>NDVI</b> = Normalized Difference Vegetation Index (used as a measure of green)</b><br><br>")),
                                           h6(HTML("Documentation Link: <a href='https://docs.google.com/document/d/1I8WkmUjuPLf0SS_IF0F6P97xyH3aQhth8m9iYUQM4hs/edit?usp=sharing'>Urban Drought Portal Documentation</a>"))
                                  ),
                                  tabPanel("Grant Information",
                                           h6(HTML("This research was supported by NIDIS through the FY 2022 Coping with Drought Competition - Ecological Drought (Award NA22OAR4310233).<br><br>
                                                   The Shiny App serves as a near real-time portal, providing a comprehensive view of the current conditions across seven land cover types: 
                                                   crop, forest, grassland, urban-high, urban-medium, urban-low, and urban-open.
                                                   Additional tabs are included to facilitate further analysis and research, broadening the scope of exploration."))
                                  ),
                                  tabPanel("Contributors",
                                           h6(HTML("Jocelyn Garcia, The Morton Arboretum (jgarcia@mortonarb.org)<br><br>
                                                    Juliana Harr, The Morton Arboretum (jharr@mortonarb.org)<br><br>
                                                    Ayo Andra J. Deas, The City University of New York (adeas@gc.cuny.edu)<br><br>
                                                    Christine R. Rollinson, The Morton Arboretum (crollinson@mortonarb.org)"))
                                  ),
                                  tabPanel("Links to Github",
                                           h6(HTML("Github Links: <br>
<p> <a href='https://github.com/UrbanEcoDrought'>UrbanEcoDrought Repository</a><br>
    <a href='https://github.com/UrbanEcoDrought/NDVI_drought_monitoring'>NDVI_Drought_Monitoring</a><br>
    <a href='https://github.com/UrbanEcoDrought/UrbanDrought_SpatialAnalysis_Chicago'>UrbanDrought_SpatialAnalysis_Chicago Workflow</a><br> 
    <a href='https://github.com/UrbanEcoDrought/UrbanDrought_SpatialAnalysis_Chicago/tree/main/NDVI_Automation_Workflow'>NDVI_Automation_Workflow</a><br>
    <a href='https://github.com/UrbanEcoDrought/UrbanDrought_SpatialAnalysis_Chicago/tree/main/Urban%20Drought%20App'>Urban Drought App</a><br></p>"))
                                  )
                                  
                                )
                        )
                        
  
)
))

# Run the application
# shinyApp(ui = ui, server = server)

