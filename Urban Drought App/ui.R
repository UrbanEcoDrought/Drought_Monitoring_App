####################################################################################################################

#Purpose: Be a visualization portal for the Urban Drought Project, meant to be a real time portal that is updated regularly
#Pulls from NDVI_drought_monitoring workflow & UrbanDrought_SpatialAnalysis_Chicago workflow

####################################################################################################################

library(shiny);library(shinydashboard);library(shinyBS);library(shinyalert);library(DT);library(lubridate)
library(leaflet);library(leaflet.extras);library(sf);library(tidyverse);library(ggplot2);library(plotly);
library(ggplot2);library(hrbrthemes);library(dplyr);library(tidyverse);library(tidyr); library(shinycssloaders)
library(tidyquant);library(scales);library(bs4Dash);library(shinyjs)


#For documentation of this app
#https://docs.google.com/document/d/1I8WkmUjuPLf0SS_IF0F6P97xyH3aQhth8m9iYUQM4hs/edit?usp=sharing
####################################################################################################################
#for testing
#Putting in all filepaths like this for now
NDVIall_normals_modeled <-read_csv("/Users/jocelyngarcia/Documents/GitHub/Drought_Monitoring_App/Urban Drought App/data/NDVIall_normals_modeled.csv")
NDVIall_years_modeled<-read_csv("/Users/jocelyngarcia/Documents/GitHub/Drought_Monitoring_App/Urban Drought App/data/NDVIall_years_modeled.csv")
####################################################################################################################
# path.UrbDrought <- "/Users/jocelyngarcia/Library/CloudStorage/GoogleDrive-jgarcia@mortonarb.org/Shared drives/Urban Ecological Drought"
# path.UrbDrought <- "~/Google Drive/Shared drives/Urban Ecological Drought/"
################################
#####Uncomment after testing ######
#####NDVI file path (Using NDVI data from NDVI Drought Monitoring Workflow so they are fit to the spline)
#NDVI_data <- read_csv("data/allNDVI_data.csv")%>%
#mutate(date = as.Date(date, format="%Y-%m-%d"))
#NDVI_data$date <- as.Date(NDVI_data$date)

#CSV file path (Using CSV data from NDVI Drought Monitoring Workflow )
#CI_csv <- read_csv("data/k=12_norms_all_LC_types.csv")
####################
#Subsetting all data here for reference (anything used for the functions)
####################################################################################################################
#DENSITY PLOTS & STATUS BOXES & PERCENTILE
NDVIall_years_modeled$year <- as.numeric(NDVIall_years_modeled$year)
latest_year <- max(NDVIall_years_modeled$year, na.rm = TRUE)

latest_yday <- max(NDVIall_years_modeled$yday[NDVIall_years_modeled$year == latest_year], na.rm = TRUE)

#pulling any rows with matching date 
most_recent_data<- filter(NDVIall_years_modeled, year == latest_year & yday == latest_yday)

##################################################
#DENSITY PLOTS & PERCENTILE (gives 2 week period)
# Setting the period for 2 weeks prior
two_week_ago_year <- if (latest_yday > 14){
  latest_year
}else {
  latest_year - 1
}

two_week_prior_yday <- if (latest_yday > 14) {
  latest_yday - 14
} else {
  365 + (latest_yday - 14)
}

# Filtering the data between two_week_prior_date and last_day_date
current_time_period <- NDVIall_years_modeled %>%
  filter(
    year >= two_week_ago_year & year <= latest_year,
    yday >= two_week_prior_yday & yday <= latest_yday
  )

NDVIall_years_modeled <- NDVIall_years_modeled %>%
  mutate(date = as.Date(yday - 1, origin = paste0(year, "-01-01")))

date_needed <- NDVIall_years_modeled %>%
  slice_max(date, n = 1, with_ties = FALSE) %>%
  pull(date)

#Need to run this code before app
lcnames <- c("forest", "crop", "grassland", "urban-high", "urban-medium", "urban-low", "urban-open")

#from https://www.census.gov/geographies/mapping-files/time-series/geo/cartographic-boundary.html
counties <- sf::read_sf("cb_2023_us_county_500k",
                        layer = "cb_2023_us_county_500k")%>% 
  st_transform(crs = 4326)

il_counties <- subset(counties, counties$NAME %in% c(
  "Cook","DuPage","Kane","McHenry","Lake","Will","Kendall") &
    STATE_NAME == "Illinois")

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
                      tags$head(
                        tags$script(HTML('
  $(document).ready(function() {
    setTimeout(function() {
      $(\'[data-toggle="tooltip"]\').tooltip();
    }, 1000);
  });
'))),
                      tags$head(
                        tags$style(HTML("
        .small-box {
          min-height: 10px !important;  
          width: 150px !important;  
        }
        
        .small-box .inner h3 { 
          font-size: 14px !important; 
        }
        
        .small-box .inner p { 
          font-size: 14px !important;
        }
      "))
                      ),
                      tabItems(
                        tabItem(tabName = "dashboard",
                                # Title for the status boxes
                                h6(HTML("<b>Status for Land Cover Types - (Status Categories: <span style='color:green;'>Much Greener than Normal</span>, <span style='color:olive;'>Greener than Normal</span>
                                        , <span style='color:#7C7779;'>Normal</span>, <span style='color:#E1278D;'>Browner than Normal</span>, <span style='color:#D8085C;'>Much Browner than Normal</span>)</b>"), 
                                   style = "center-align: center; margin-bottom: 20px;"),
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
                                      h6(HTML("<b>The NDVI percentiles below indicate how current vegetation conditions compare to the past two weeks 
                                              of observed data.</b><br><br>")),
                                      # Proper text output for the percentile value
                                      h6(textOutput("percentile_crop")),  
                                      h6(textOutput("percentile_for")),  
                                      h6(textOutput("percentile_grass")),  
                                      h6(textOutput("percentile_uh")),  
                                      h6(textOutput("percentile_um")),  
                                      h6(textOutput("percentile_ul")),
                                      h6(textOutput("percentile_uo")),  
                                      h6(HTML("<b>A lower percentile suggests below-average greenness, while a higher
                                      percentile indicates above-average greenness</b>")),
                                      p("For a more in-depth exploration, take a look at the other tabs or the directory.")
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
                                      h6(HTML("<b>Distribution of crop norm values for ydays 1-365.The current NDVI (Green Diamond) and
                                              normal for the current yday (Purple Circle) are also displayed
                                              with 95% CI surrounding them. Overlap in the CI is expected, but points of nonoverlap are noteable.</b><br>")),
                                      plotlyOutput("crop_density_plot"),
                                      textOutput("crop_daily")
                                    ),
                                    tabPanel(
                                      "Forest Density Plot",
                                      h6(HTML("<b>Distribution of forest norm values for ydays 1-365.The current NDVI (Green Diamond) and
                                              normal for the current yday (Purple Circle) are also displayed
                                              with 95% CI surrounding them. Overlap in the CI is expected, but points of nonoverlap are noteable.</b><br>")),
                                      plotlyOutput("forest_density_plot"),
                                      textOutput("for_daily")
                                    ),
                                    tabPanel(
                                      "Grassland Density Plot",
                                      h6(HTML("<b>Distribution of grassland norm values for ydays 1-365.The current NDVI (Green Diamond) and
                                              normal for the current yday (Purple Circle) are also displayed
                                              with 95% CI surrounding them. Overlap in the CI is expected, but points of nonoverlap are noteable.</b><br>")),
                                      plotlyOutput("grassland_density_plot"),
                                      textOutput("grass_daily")
                                    ),
                                    tabPanel(
                                      "Urban-High Density Plot",
                                      h6(HTML("<b>Distribution of urban-high norm values for ydays 1-365.The current NDVI (Green Diamond) and
                                              normal for the current yday (Purple Circle) are also displayed
                                              with 95% CI surrounding them. Overlap in the CI is expected, but points of nonoverlap are noteable.</b><br>")),
                                      plotlyOutput("uh_density_plot"),
                                      textOutput("uh_daily")
                                    ),
                                    tabPanel(
                                      "Urban-Medium Density Plot",
                                      h6(HTML("<b>Distribution of urban-medium norm values for ydays 1-365.The current NDVI (Green Diamond) and
                                              normal for the current yday (Purple Circle) are also displayed
                                              with 95% CI surrounding them. Overlap in the CI is expected, but points of nonoverlap are noteable.</b><br>")),
                                      plotlyOutput("um_density_plot"),
                                      textOutput("um_daily")
                                    ),
                                    tabPanel(
                                      "Urban-Low Density Plot",
                                      h6(HTML("<b>Distribution of urban-low norm values for ydays 1-365.The current NDVI (Green Diamond) and
                                              normal for the current yday (Purple Circle) are also displayed
                                              with 95% CI surrounding them. Overlap in the CI is expected, but points of nonoverlap are noteable.</b><br>")),
                                      plotlyOutput("ul_density_plot"),
                                      textOutput("ul_daily")
                                    ),
                                    tabPanel(
                                      "Urban-Open Density Plot",
                                      h6(HTML("<b>Distribution of urban-open norm values for ydays 1-365.The current NDVI (Green Diamond) and
                                              normal for the current yday (Purple Circle) are also displayed
                                              with 95% CI surrounding them. Overlap in the CI is expected, but points of nonoverlap are noteable.</b><br>")),
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
                                  height = "3000px",
                                  width = 12, 
                                  tabPanel("Full Review",
                                           h6(HTML("<b>This feature may need additional time to load, please allow a few minutes for graphs to load.</b><br>")),
                                           plotOutput("all_data_graph", height = "400px")),
                                  tabPanel("Yearly",
                                           h6(HTML("<b>This feature may need additional time to load, please allow a few minutes for graphs to load.</b><br>")),
                                           plotOutput("yearly_graph", height = "400px"),
                                           dateInput(inputId = "start_date", label = "Enter Start Date", value = Sys.Date() - 365)),
                                  tabPanel("Monthly", 
                                           h6(HTML("<b>This feature may need additional time to load, please allow a few minutes for graphs to load.</b><br>")),
                                           plotOutput("monthly_graph", height = "400px"),
                                           dateInput(inputId = "mstart_date", label = "Enter Start Date", value = Sys.Date() %m-% months(1))),
                                  tabPanel("Weekly",
                                           h6(HTML("<b>This feature may need additional time to load, please allow a few minutes for graphs to load.</b><br>")),
                                           plotOutput("weekly_graph",height = "400px"),
                                           h6(HTML("If the graph isn't populating there might not be enough data currently, try an early date")),
                                           dateInput(inputId = "wstart_date", label = "Enter Start Date", value = Sys.Date() - 7))
                                )
                        ),
                        tabItem(tabName = "ndvi_diff",
                                h6(HTML("<b>The graphs display NDVI (Normalized Difference Vegetation Index) trends over time. 
                                        Each color represents a different status. Years can be toggled on an off for convience & comparison.</b>")),
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
                                                   choices = as.character(sort(unique(NDVIall_years_modeled$year), decreasing = TRUE)), 
                                                   selected = as.character(sort(unique(NDVIall_years_modeled$year), decreasing = TRUE))[1:5],
                                                   inline = TRUE),
                                h6(HTML("<b>This feature may need additional time to load, please allow a few minutes for graphs to load.</b><br>")),
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
                                           h6(HTML("The Shiny App serves as a near real-time portal, providing a comprehensive view of the current conditions across seven land cover types.
                                                   Additional tabs are included to facilitate further analysis and research, broadening the scope of exploration.<br><br>
                                           <b>LC Types</b> = Landcover types (crop, forest, grass/grassland, urban-high, urban-medium, urban-low, urban-open)<br>
                <b>NDVI</b> = Normalized Difference Vegetation Index (used as a measure of green)</b><br><br>The main workflows use include:<br>
                <b>NDVI_Drought_Monitoring Workflow</b><br>
                <b>UrbanDrought_SpatialAnalysis_Chicago Workflow</b><br><br>
                Links can be found under 'Links to Github' and they were created by Juliana Harr & Christy Rollinson<br>")),
                                           h6(HTML("Documentation Link: <a href='https://docs.google.com/document/d/1I8WkmUjuPLf0SS_IF0F6P97xyH3aQhth8m9iYUQM4hs/edit?usp=sharing'>Urban Drought Portal Documentation</a>")),
                                           h6(HTML("USDM Link: <a href='https://droughtmonitor.unl.edu'>USDM</a>"))
                                           
                                  ),
                                  tabPanel("Grant Information",
                                           h6(HTML("This research was supported by NIDIS through the FY 2022 Coping with Drought Competition - Ecological Drought (Award NA22OAR4310233).<br><br>
                                                "))
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

