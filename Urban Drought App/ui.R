####################################################################################################################

#Purpose: Be a visualization portal for the Urban Drought Project, meant to be a real time portal that is updated regularly
#Pulls from NDVI_drought_monitoring workflow & UrbanDrought_SpatialAnalysis_Chicago workflow

####################################################################################################################
# Libraries ----
library(shiny);library(shinydashboard);library(shinyBS);library(shinyalert);library(DT);library(lubridate)
library(leaflet);library(leaflet.extras);library(sf);library(tidyverse);library(ggplot2);library(plotly);
library(ggplot2);library(hrbrthemes);library(dplyr);library(tidyverse);library(tidyr); library(shinycssloaders)
library(tidyquant);library(scales);library(bs4Dash);library(shinyjs);library(shinyGovstyle)


#For documentation of this app
#https://docs.google.com/document/d/1I8WkmUjuPLf0SS_IF0F6P97xyH3aQhth8m9iYUQM4hs/edit?usp=sharing
####################################################################################################################
# Read in Data ----
#for testing
#Putting in all filepaths like this for now
# NDVIall_normals_modeled <-read_csv("/Users/jocelyngarcia/Documents/GitHub/Drought_Monitoring_App/Urban Drought App/data/NDVIall_normals_modeled.csv")
# NDVIall_years_modeled<-read_csv("/Users/jocelyngarcia/Documents/GitHub/Drought_Monitoring_App/Urban Drought App/data/NDVIall_years_modeled.csv")

NDVIall_normals_modeled <-read_csv("data/NDVIall_normals_modeled.csv")
NDVIall_years_modeled<-read_csv("data/NDVIall_years_modeled.csv")

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
#DENSITY PLOTS & STATUS BOXES & PERCENTILE -----
NDVIall_years_modeled$year <- as.numeric(NDVIall_years_modeled$year)
latest_year <- max(NDVIall_years_modeled$year, na.rm = TRUE)

latest_yday <- max(NDVIall_years_modeled$yday[NDVIall_years_modeled$year == latest_year], na.rm = TRUE)

#pulling any rows with matching date 
most_recent_data<- filter(NDVIall_years_modeled, year == latest_year & yday == latest_yday)

##################################################
NDVIall_years_modeled <- NDVIall_years_modeled %>%
  mutate(date = as.Date(yday - 1, origin = paste0(year, "-01-01")))

date_needed <- NDVIall_years_modeled %>%
  slice_max(date, n = 1, with_ties = FALSE) %>%
  pull(date)

#Need to run this code before app
lcnames <- c("forest", "crop", "grassland", "urban-high", "urban-medium", "urban-low", "urban-open")

yrNow <- lubridate::year(Sys.Date())
day.labels <- data.frame(Date=seq.Date(as.Date(paste0(yrNow, "-01-01")), as.Date(paste0(yrNow, "-12-01")), by="month"))
day.labels$yday <- lubridate::yday(day.labels$Date)
day.labels$Text <- paste(lubridate::month(day.labels$Date, label=T), lubridate::day(day.labels$Date))
day.labels
summary(day.labels)

#from https://www.census.gov/geographies/mapping-files/time-series/geo/cartographic-boundary.html
counties <- sf::read_sf("cb_2023_us_county_500k",
                        layer = "cb_2023_us_county_500k")%>% 
  st_transform(crs = 4326)

il_counties <- subset(counties, counties$NAME %in% c(
  "Cook","DuPage","Kane","McHenry","Lake","Will","Kendall") &
    STATE_NAME == "Illinois")
####################################################################################################################

#Needed to move this here to add the banner ----
dbHeader <- dashboardHeader(
  title = HTML("<span style='font-size: 16px; font-weight: bold;'>Urban Drought Portal BETA</span>"),
  titleWidth = 200
 
 # shinyGovstyle::banner(
  #  inputId = "banner", 
   # type = "Beta Version:",
    #HTML('<span style="background-color:#B9D3EE; padding: 2px;"><b>This portal is in beta and still under development. 
     #   Some features may be incomplete or subject to change.</b></span>')
  )
#)

#Needed to move this here to add the logo ----
dbSidebar <- dashboardSidebar(
  style = "position: relative; height: 93vh;",  # Ensures the sidebar takes full height
  
  tags$div(
    style = "position: absolute; bottom: 10px; left: 60px;",  # Adjust position as needed
    tags$a(
      href = 'https://mortonarb.org', 
      tags$img(src = 'mortonarb.png', height = '60', width = '150')
    )
  ),
  tags$div(
    style = "position: absolute; bottom: 15px; left: 5px;",  # Adjust position as needed
    tags$a(
      href = 'https://www.drought.gov', 
      tags$img(src = 'NIDIS.png', height = '48', width = '55')
    )
  ),
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Analysis", tabName = "analysis", icon = icon("gears"),
             menuSubItem("NDVI Data Review",
                         tabName = "NDVI_data_review"),
             menuSubItem("Heat Maps for each LC Type",
                         tabName = "ndvi_diff")),
    menuItem("About", tabName = "specifics", icon = icon("bookmark"))
  )
)

####################################################################################################################
# UI ----
ui <- dashboardPage(skin = "black",
                    dbHeader,
                    dbSidebar,
                    dashboardBody(
                      div(
                        style = "
    height: 25px; 
    background-color: #1A85FF; 
    width: 100%; 
    position: relative; 
    display: flex; 
    justify-content: center; 
    align-items: center;",
                        h6(HTML("<b style='color: white;'> ------------------------------------  This portal is in beta & still under development, some features 
          may be incomplete or subject to change  ------------------------------------ </b>"))
                      ),
                      br(),
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
                                h6(HTML("<b>Status for Land Cover Types - (Status Categories: <span style='color:#4DAC26;'>Much Greener than Normal</span>, <span style='color:#B8E186;'>Greener than Normal</span>
                                        , <span style='color:#7C7779;'>Normal</span>, <span style='color:#F1B6DA;'>Browner than Normal</span>, <span style='color:#D01C8B;'>Much Browner than Normal</span>)</b>"), 
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
                                      h6(HTML("<b>The NDVI percentiles below show how current vegetation conditions compare
                                              to those on the same calendar day in previous years for their respective land cover type.</b><br>")),
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
                                      "Crop Plots",
                                      h6(HTML("Current smoothed NDVI time series (blue) versus normal (black).<br>")),
                                      plotOutput("crop_currentTS_plot"),
                                      h6(HTML("<br><br>Distribution of normal crop NDVI for an entire year with the current NDVI (Green Diamond) and
                                              normal for the current day of the year (Purple Circle) shown with a 95% confidence interval surrounding them. Non-overlapping intervals indicate the landcover is significantly greener or browner than normal.<br>")),
                                      plotOutput("crop_density_plot"),
                                      textOutput("crop_daily")
                                    ),
                                    tabPanel(
                                      "Forest Plots",
                                      h6(HTML("Current smoothed NDVI time series (blue) versus normal (black).<br>")),
                                      plotOutput("forest_currentTS_plot"),
                                      
                                      h6(HTML("<br><br>Distribution of normal forest NDVI for an entire year with the current NDVI (blue diamond) and
                                              normal for the current day of the year (orange circle) shown with a 95% confidence interval surrounding them. Non-overlapping intervals indicate the landcover is significantly greener or browner than normal.<br>")),
                                      plotOutput("forest_density_plot"),
                                      textOutput("for_daily")
                                    ),
                                    tabPanel(
                                      "Grassland Plots",
                                      h6(HTML("Current smoothed NDVI time series (blue) versus normal (black).<br>")),
                                      plotOutput("grass_currentTS_plot"),
                                      h6(HTML("<br><br>Distribution of normal grassland NDVI for an entire year with the current NDVI (blue diamond) and
                                              normal for the current day of the year (orange circle) shown with a 95% confidence interval surrounding them. Non-overlapping intervals indicate the landcover is significantly greener or browner than normal.<br>")),
                                      plotOutput("grassland_density_plot"),
                                      textOutput("grass_daily")
                                    ),
                                    tabPanel(
                                      "Urban-High Plots",
                                      h6(HTML("Current smoothed NDVI time series (blue) versus normal (black).<br>")),
                                      plotOutput("uh_currentTS_plot"),
                                      h6(HTML("<br><br>Distribution of normal high intensity urban NDVI for an entire year with the current NDVI (blue diamond) and
                                              normal for the current day of the year (orange circle) shown with a 95% confidence interval surrounding them. Non-overlapping intervals indicate the landcover is significantly greener or browner than normal.<br>")),
                                      plotOutput("uh_density_plot"),
                                      textOutput("uh_daily")
                                    ),
                                    tabPanel(
                                      "Urban-Medium Plots",
                                      h6(HTML("Current smoothed NDVI time series (blue) versus normal (black).<br>")),
                                      plotOutput("um_currentTS_plot"),
                                      h6(HTML("<br><br>Distribution of normal medium intensity urban NDVI for an entire year with the current NDVI (orange circle) and
                                              normal for the current day of the year (blue diamond) shown with a 95% confidence interval surrounding them. Non-overlapping intervals indicate the landcover is significantly greener or browner than normal.<br>")),
                                      plotOutput("um_density_plot"),
                                      textOutput("um_daily")
                                    ),
                                    tabPanel(
                                      "Urban-Low Plots",
                                      h6(HTML("Current smoothed NDVI time series (blue) versus normal (black).<br>")),
                                      plotOutput("ul_currentTS_plot"),
                                      h6(HTML("<br><br>Distribution of normal low intensity urban NDVI for an entire year with the current NDVI (blue diamond) and
                                              normal for the current day of the year (orange circle) shown with a 95% confidence interval surrounding them. Non-overlapping intervals indicate the landcover is significantly greener or browner than normal.<br>")),
                                      plotOutput("ul_density_plot"),
                                      textOutput("ul_daily")
                                    ),
                                    tabPanel(
                                      "Urban-Open Plots",
                                      h6(HTML("Current smoothed NDVI time series (blue) versus normal (black).<br>")),
                                      plotOutput("uo_currentTS_plot"),
                                      h6(HTML("<br><br>Distribution of normal open urban NDVI for an entire year with the current NDVI (blue diamond) and
                                              normal for the current day of the year (orange circle) shown with a 95% confidence interval surrounding them. Non-overlapping intervals indicate the landcover is significantly greener or browner than normal.<br>")),
                                      plotOutput("uo_density_plot"),
                                      textOutput("uo_daily")
                                    )
                                  )
                                  
                                )
                                
                        ),
                        tabItem(tabName = "NDVI_data_review",
                                # NDVI Data Tab Box
                                tabBox(
                                  id = "tab1",
                                  height = "3000px",
                                  width = 12, 
                                  tabPanel("Overview of Feature",
                                           h6(HTML("<b>Feature</b>: <u>Trends in NDVI Data Visuals</u><br><br>")),
                                           h6(HTML("<b>Purpose</b>:To help users see visually if there are any patterns in the NDVI data across Land Cover Types (ie Was there a Land Cover Type that dipped or peaked
                                                   when other Land Cover Types didn't?)")),
                                           h6(HTML("<b>Description</b>: Plotted NDVI values with 4 time frame options.<br><br>
                                           <li><u>Full Review tab</u> - overview with all data for all years in dataset</li>
                                           <li><u>Yearly tab</u> - 12 month overview, where user can determine which years are shown with slider</li>
                                           <li><u>Monthly tab</u> - view where year and month are inputted by the user</li>
                                          <li><u>Weekly tab</u> - where start date is inputted by the user and the following 7 days of data are shown</li><br>
                                           Default start dates are set to the most current pull of data<br>")),
                                           h6(HTML("<b>Condsiderdations</b>:<br><li>In the Chicago region, the winter season makes it difficult to interpret NDVI in the winter seasons.
                                                   Caution should be exercised to prevent over interpretation of November - March greenness values.</li>
                                                   <li>At the start of the new year there is a jump in the data marked by a vertical line. The jump is due to how the data was processed by the GAMs.</li><br><br>")),
                                           div(
                                             style = "text-align: center;",
                                             tags$img(src = 'LC_info_table.png', height = '400', width = '700')
                                           ),
                                           div(
                                             style = "text-align: center;",
                                             h6(HTML("<br><b>Landcover designations are from the U.S. National Landcover Database (NLCD)</b>"))),
                                  ),
                                  tabPanel("Full Review",
                                           h6(HTML("<b>This feature may need additional time to load, please allow a few minutes for graphs to load.</b><br>")),
                                           plotOutput("all_data_graph", height = "400px")),
                                  tabPanel("Yearly",
                                           h6(HTML("<b>This feature may need additional time to load, please allow a few minutes for graphs to load. <br><br>
                                           NOTE: At the start of the new year there is a jump in the data marked by a vertical line, this is from how
                                                   the data was processed using GAMs.</b><br>")),
                                           sliderInput("yearRange", "Select Year Range:",
                                                       min = min(NDVIall_years_modeled$year),
                                                       max = max(NDVIall_years_modeled$year),
                                                       value = c(min(NDVIall_years_modeled$year), max(NDVIall_years_modeled$year)),
                                                       sep = ""
                                                       ),
                                           plotOutput("yearly_graph", height = "400px")),
                                           #dateInput(inputId = "start_date", label = "Enter Start Date", value = Sys.Date() - 365)),
                                  tabPanel("Monthly", 
                                           h6(HTML("<b>This feature may need additional time to load, please allow a few minutes for graphs to load.</b><br>")),
                                           plotOutput("monthly_graph", height = "400px"),
                                           dateInput(inputId = "mstart_date", label = "Enter Start Date", value = date_needed %m-% months(1))),
                                  tabPanel("Weekly",
                                           h6(HTML("<b>This feature may need additional time to load, please allow a few minutes for graphs to load.</b><br>")),
                                           plotOutput("weekly_graph",height = "400px"),
                                           h6(HTML("If the graph isn't populating there might not be enough data currently, try an early date")),
                                           dateInput(inputId = "wstart_date", label = "Enter Start Date", value = date_needed - 7))
                                )
                        ),
                        tabItem(tabName = "ndvi_diff",
                                tabBox(
                                  id = "tab2",
                                  height = "3000px",
                                  width = 12, 
                                  tabPanel("Overview of Feature",
                                           h6(HTML("<b>Feature</b>: <u>Heatmaps</u><br><br>")),
                                           h6(HTML("<b>Purpose</b>:To help users see visually if there are any patterns in the data for NDVI status across an extended period of time(ie Was there a period 
                                                   of the year that was consistently below the normal? In what months was this deficiency the strongest?)")),
                                           h6(HTML("<b>Description</b>: Heat map of the NDVI status across all the years available in the data (<u>see visual below for status categories</u>). Current year
                                           will update as more data is available. Years can be toggled on & off for comparisons.<br>")),
                                           h6(HTML("<b>Condsiderdations</b>:<br><li>In the Chicago region, the winter season makes it difficult to interpret NDVI in the winter seasons.
                                                   Caution should be exercised to prevent over interpretation of November - March greenness values.</li><br><br>")),
                                           div(
                                             style = "text-align: center;",
                                             tags$img(src = 'status_info_table.png', height = '400', width = '700')
                                           ),
                                           div(
                                             style = "text-align: center;",
                                           h6(HTML("<br><b>Insert information on how categories were determined</b>"))),
                                           
                                  ),
                                  tabPanel("Heatmap Graphs",
                                h6(HTML("<b>The graphs display NDVI trends over time. 
                                        Each color represents a different status. Years can be toggled on an off for convenience & comparison.</b>")),
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
                                plotOutput("ndvi_heatmap_uo"))
                        )),
                        tabItem(tabName = "specifics",
                                tabBox(
                                  height = "3000px",
                                  width = 12,
                                  tabPanel("Preliminary Information",
                                           h6(HTML("The Shiny App serves as a near real-time portal, providing a comprehensive view of the current conditions across seven land cover types.
                                                   Additional tabs are included to facilitate further analysis and research, broadening the scope of exploration.<br><br>
                                           <b>LC Types</b> = Landcover types (crop, forest, grass/grassland, urban-high, urban-medium, urban-low, urban-open)<br>
                <b>NDVI</b> = Normalized Difference Vegetation Index (used as a measure of green)</b><br><br>Data Visualized comes from two main workflows:<br>
                <b>NDVI_Drought_Monitoring Workflow</b><br>
                <b>UrbanDrought_SpatialAnalysis_Chicago Workflow</b><br>
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
                                                    Lindsay Darling, The Morton Arboretum (ldarling@mortonarb.org) <br><br>
                                                    Christine R. Rollinson, The Morton Arboretum (crollinson@mortonarb.org)<br><br>
                                                   M. Ross Alexander, Consortium for Advanced Science and Engineering, University of Chicago<br><br>
                                                   Trent Ford, Illinois State Water Survey, University of Illinois, Urbana-Champaign (twford@illinois.edu)<br><br>"))
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

