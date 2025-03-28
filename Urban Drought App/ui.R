####################################################################################################################

#Purpose: Be a visualization portal for the Urban Drought Project, meant to be a real time portal that is updated regularly
#Pulls from NDVI_drought_monitoring workflow & UrbanDrought_SpatialAnalysis_Chicago workflow

####################################################################################################################

library(shiny);library(shinydashboard);library(shinyBS);library(shinyalert);library(DT);library(lubridate)
library(leaflet);library(leaflet.extras);library(sf);library(tidyverse);library(ggplot2);library(plotly)
library(hrbrthemes);library(dplyr);library(tidyverse);library(tidyr); library(shinycssloaders)
library(tidyquant);library(scales);library(bs4Dash);library(shinyjs);library(shinyGovstyle)


#For documentation of this app
#https://docs.google.com/document/d/1I8WkmUjuPLf0SS_IF0F6P97xyH3aQhth8m9iYUQM4hs/edit?usp=sharing
####################################################################################################################
NDVIall_normals_modeled <-read_csv("data/NDVIall_normals_modeled.csv")
NDVIall_years_modeled<-read_csv("data/NDVIall_years_modeled.csv")
####################################################################################################################
# path.UrbDrought <- "/Users/jocelyngarcia/Library/CloudStorage/GoogleDrive-jgarcia@mortonarb.org/Shared drives/Urban Ecological Drought"
# path.UrbDrought <- "~/Google Drive/Shared drives/Urban Ecological Drought/"
################################
#####NDVI file path (Using NDVI data from NDVI Drought Monitoring Workflow so they are fit to the spline)
#NDVI_data <- read_csv("data/allNDVI_data.csv")%>%
#mutate(date = as.Date(date, format="%Y-%m-%d"))
#NDVI_data$date <- as.Date(NDVI_data$date)

#CSV file path (Using CSV data from NDVI Drought Monitoring Workflow )
#CI_csv <- read_csv("data/k=12_norms_all_LC_types.csv")
####################################################################################################################
#Subsetting all data here for reference (anything used for the functions)
  #FOR DENSITY PLOTS & STATUS BOXES & PERCENTILE
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

#End of subsetting
####################################################################################################################
#Need to run this code before app
#For map
lcnames <- c("forest", "crop", "grassland", "urban-high", "urban-medium", "urban-low", "urban-open")

#from https://www.census.gov/geographies/mapping-files/time-series/geo/cartographic-boundary.html
counties <- sf::read_sf("cb_2023_us_county_500k",
                        layer = "cb_2023_us_county_500k")%>% 
  st_transform(crs = 4326)

il_counties <- subset(counties, counties$NAME %in% c(
  "Cook","DuPage","Kane","McHenry","Lake","Will","Kendall") &
    STATE_NAME == "Illinois")
####################################################################################################################

#Needed to move this here to add the banner
dbHeader <- dashboardHeader(
  title = HTML("<span style='font-size: 16px; font-weight: bold;'>Urban Drought Portal BETA</span>"),
  titleWidth = 200
  )

#Needed to move this here to add the logo
dbSidebar <- dashboardSidebar(
  style = "position: relative; height: 93vh;",  # Ensures the sidebar takes full height
  
  tags$div(
    style = "position: absolute; bottom: 15px; left: 30px;",  # Adjust position as needed
    tags$a(
      href = 'https://mortonarb.org', 
      tags$img(src = 'mortonarb.png', height = '70', width = '160')
    )
  ),
  tags$div(
    style = "position: absolute; bottom: 85px; left: 85px;",  # Adjust position as needed
    tags$a(
      href = 'https://www.drought.gov', 
      tags$img(src = 'NIDIS.png', height = '50', width = '60')
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
 ##########################################################################################################################################################
 #STATUS BOXES
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
 ##########################################################################################################################################################
 #MAP
                                 fluidRow(
                                  column(width = 5, 
                                         leafletOutput("il_county_map", height = "350px")),
 ##########################################################################################################################################################
 #GENERAL INFORMATION TABBOX (PERCENTILES, LATEST DATA REPORT, ETC.)
                                  tabBox(
                                    title = tagList(shiny::icon("bars"), "General Information"),
                                    width = 7,
                                    tabPanel(
                                      "Latest Data Report",
                                      h6(tags$b(paste("Most Recent Data is from", format(date_needed, "%B %d, %Y")))),
                                      h6(HTML("<b>The NDVI percentiles below show how current vegetation conditions compare
                                              to those on the same calendar day in previous years for their respective land cover type.</b><br>")),
                                      # Percentiles
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
 ##########################################################################################################################################################
 #DENSITY PLOTS
                                  tabBox(
                                    width = 12,
                                    tabPanel(
                                      "Crop Density Plot",
                                      h6(HTML("<b>Distribution of crop norm values for ydays 1-365.The current NDVI (Blue Diamond) and
                                              normal for the current day of the year (Orange Circle) are also displayed
                                             with 95% CI surrounding them. Overlap in the CI is expected, but areas of nonoverlap are noteable.<br><br>
                                             <span style='color:#FF8247;'>Orange Shaded Area</span> = Normal 95% CI<br>
                                              <span style='color:#7EC0EE;'>Blue Shaded Area</span> = NDVI 95% CI</b>")),
                                      plotlyOutput("crop_density_plot"),
                                      textOutput("crop_daily")
                                    ),
                                    tabPanel(
                                      "Forest Density Plot",
                                      h6(HTML("<b>Distribution of forest norm values for days of the year 1-365.The current NDVI (Blue Diamond) and
                                              normal for the current day of the year (Orange Circle) are also displayed
                                              with 95% CI surrounding them. Overlap in the CI is expected, but areas of nonoverlap are noteable.<br><br>
                                              <span style='color:#FF8247;'>Orange Shaded Area</span> = Normal 95% CI<br>
                                              <span style='color:#7EC0EE;'>Blue Shaded Area</span> = NDVI 95% CI</b>")),
                                      plotlyOutput("forest_density_plot"),
                                      textOutput("for_daily")
                                    ),
                                    tabPanel(
                                      "Grassland Density Plot",
                                      h6(HTML("<b>Distribution of grassland norm values for days of the year 1-365.The current NDVI (Blue Diamond) and
                                              normal for the current day of the year (Orange Circle) are also displayed
                                              with 95% CI surrounding them. Overlap in the CI is expected, but areas of nonoverlap are noteable.<br><br>
                                             <span style='color:#FF8247;'>Orange Shaded Area</span> = Normal 95% CI<br>
                                              <span style='color:#7EC0EE;'>Blue Shaded Area</span> = NDVI 95% CI</b>")),
                                      plotlyOutput("grassland_density_plot"),
                                      textOutput("grass_daily")
                                    ),
                                    tabPanel(
                                      "Urban-High Density Plot",
                                      h6(HTML("<b>Distribution of urban-high norm values for days of the year 1-365. The current NDVI (Blue Diamond) and
                                              normal for the current day of the year (Orange Circle) are also displayed
                                              with 95% CI surrounding them. Overlap in the CI is expected, but areas of nonoverlap are noteable.<br><br>
                                              <span style='color:#FF8247;'>Orange Shaded Area</span> = Normal 95% CI<br>
                                              <span style='color:#7EC0EE;'>Blue Shaded Area</span> = NDVI 95% CI</b>")),
                                      plotlyOutput("uh_density_plot"),
                                      textOutput("uh_daily")
                                    ),
                                    tabPanel(
                                      "Urban-Medium Density Plot",
                                      h6(HTML("<b>Distribution of urban-medium norm values for days of the year 1-365.The current NDVI (Blue Diamond) and
                                              normal for the current day of the year (Orange Circle) are also displayed
                                             with 95% CI surrounding them. Overlap in the CI is expected, but areas of nonoverlap are noteable.<br><br>
                                              <span style='color:#FF8247;'>Orange Shaded Area</span> = Normal 95% CI<br>
                                              <span style='color:#7EC0EE;'>Blue Shaded Area</span> = NDVI 95% CI</b>")),
                                      plotlyOutput("um_density_plot"),
                                      textOutput("um_daily")
                                    ),
                                    tabPanel(
                                      "Urban-Low Density Plot",
                                      h6(HTML("<b>Distribution of urban-low norm values for days of the year 1-365.The current NDVI (Blue Diamond) and
                                              normal for the current day of the year (Orange Circle) are also displayed
                                              with 95% CI surrounding them. Overlap in the CI is expected, but areas of nonoverlap are noteable.<br><br>
                                              <span style='color:#FF8247;'>Orange Shaded Area</span> = Normal 95% CI<br>
                                              <span style='color:#7EC0EE;'>Blue Shaded Area</span> = NDVI 95% CI</b>")),
                                      plotlyOutput("ul_density_plot"),
                                      textOutput("ul_daily")
                                    ),
                                    tabPanel(
                                      "Urban-Open Density Plot",
                                      h6(HTML("<b>Distribution of urban-open norm values for days of the year 1-365.The current NDVI (Blue Diamond) and
                                              normal for the current day of the year (Orange Circle) are also displayed
                                              with 95% CI surrounding them. Overlap in the CI is expected, but areas of nonoverlap are noteable.<br><br>
                                              <span style='color:#FF8247;'>Orange Shaded Area</span> = Normal 95% CI<br>
                                              <span style='color:#7EC0EE;'>Blue Shaded Area</span> = NDVI 95% CI</b>")),
                                      plotlyOutput("uo_density_plot"),
                                      textOutput("uo_daily")
                                    )
                                  )
                                  
                                )
                                
                        ),
 ##########################################################################################################################################################
 #NDVI DATA REVIEW 
                        tabItem(tabName = "NDVI_data_review",
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
  ##########################################################################################################################################################
  #HEATMAPS 
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
                                  ),
                                  tabPanel("Heatmap Graphs",
                                h6(HTML("<b>The graphs display NDVI trends over time. 
                                        Each color represents a different status. Years can be toggled on an off for convenience & comparison.</b>")),
                                tags$style(HTML("
  #selected_years label {
    font-size: 13px;  /* Adjust label text size */
    height: 10px;    /* Adjust height of the label */
  }
  #selected_years input[type='checkbox'] {
    transform: scale(0.8);  /* Resize the checkboxes */
  }
")),
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
  ##########################################################################################################################################################
  #ABOUT TAB
                        tabItem(tabName = "specifics",
                                tabBox(
                                  height = "3000px",
                                  width = 12,
                                  tabPanel("Preliminary Information",
                                           h6(HTML("<b> The Shiny App serves as a near real-time portal, providing a comprehensive view of the current conditions across seven land cover types.
                                                   Additional tabs are included to facilitate further analysis and research, broadening the scope of exploration.</b><br><br>"), 
                                              style = "text-align: center;"),
                                              h6(HTML("<u>Terms to Know</u>:<br>
                                                   <b>LC Types</b> = Landcover types (crop, forest, grass/grassland, urban-high, urban-medium, urban-low, urban-open)<br>
                                                   <b>NDVI</b> = Normalized Difference Vegetation Index (used as a measure of green)</b><br><br>")),
                                           h6(HTML("<u>More Information on Portal Creation</u>:<br>
                                           *Possibly insert background infromaiton about the papers being done with this research 
                                           - might be good primer before talking about the differnet workflows that go into this portal*<br><br>
                                        <u>Main Workflows Used</u>:<br>
                                          NDVI_Drought_Monitoring Workflow<br>
                                          UrbanDrought_SpatialAnalysis_Chicago Workflow<br><br>
                                          Links to the code for these workflows can be found under 'Links to Github' and they were created by Juliana Harr & Christy Rollinson<br><br><br>")),
                                           h6(HTML("<b>Documentation is avaiable for this portal with feature descriptions, visuals, and consideration for possible extensions!<br>(<a href='https://docs.google.com/document/d/1I8WkmUjuPLf0SS_IF0F6P97xyH3aQhth8m9iYUQM4hs/edit?usp=sharing'>Urban Drought Portal Documentation</a></b>)")),
                                           h6(HTML("<b>For additional information check out the USDM!<br>(<a href='https://droughtmonitor.unl.edu'>USDM</a></b>)"))
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
                                           h6(HTML("<b>Github Links</b>: <br>
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

