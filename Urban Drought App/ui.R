####################################################################################################################

#Purpose: Be a visualization portal for the Urban Drought Project, meant to be a real time portal that is updated regularly
#Pulls from NDVI_drought_monitoring workflow & UrbanDrought_SpatialAnalysis_Chicago workflow

####################################################################################################################
# Libraries ----
library(shiny); library(shinydashboard)
library(leaflet); library(sf)
library(tidyverse); library(lubridate); library(scales)
library(shinyalert)


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
  title = tags$div(
    style = "display: flex; align-items: center; gap: 12px; padding: 4px 0;",
    tags$a(
      href = 'https://mortonarb.org',
      tags$img(src = 'mortonarb.png', height = '48',
               style = "background-color: white; padding: 4px; border-radius: 4px;")
    ),
    tags$a(
      href = 'https://www.drought.gov',
      tags$img(src = 'NIDIS.png', height = '46')
    )
  ),
  titleWidth = 220
)

#Needed to move this here to add the logo ----
dbSidebar <- dashboardSidebar(

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
                        style = "text-align: center; padding: 8px 0 4px 0;",
                        h4(HTML("<b>Chicago Urban Greenness & Drought Portal</b>"),
                           style = "margin: 0; color: #333;")
                      ),
                      div(
                        style = "
    height: 25px;
    background-color: #BA8E23;
    width: 100%;
    position: relative;
    display: flex;
    justify-content: center;
    align-items: center;",
                        h6(HTML("<b style='color: white;'> ----- NOTE: This portal is in beta & still under development, some features 
          may be incomplete or subject to change  ----- </b>"))
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
        body, .content-wrapper {
          font-size: 18px !important;
        }
        .content-wrapper p, .content-wrapper label,
        .content-wrapper .control-label, .content-wrapper .shiny-input-container,
        .content-wrapper .tab-content, .content-wrapper .nav-tabs > li > a {
          font-size: 18px !important;
        }

        .sidebar-menu > li > a {
          font-size: 18px !important;
        }
        .sidebar-menu .treeview-menu > li > a {
          font-size: 16px !important;
        }

        h6 {
          font-size: 18px !important;
          font-weight: normal;
        }

        .small-box {
          min-height: 60px !important;
          width: 210px !important;
        }

        .small-box .inner h3 {
          font-size: 15px !important;
          white-space: normal !important;
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
                                      div(style = "display: inline-block;", uiOutput("cropBox")),
                                      div(style = "display: inline-block;", uiOutput("forBox")),
                                      div(style = "display: inline-block;", uiOutput("grassBox")),
                                      div(style = "display: inline-block;", uiOutput("uoBox")),
                                      div(style = "display: inline-block;", uiOutput("ulBox")),
                                      div(style = "display: inline-block;", uiOutput("umBox")),
                                      div(style = "display: inline-block;", uiOutput("uhBox"))
                                      
                                    )
                                  ),
                                ),
                                fluidRow(
                                  column(width = 12,
                                    p(HTML("<b>Note:</b> NDVI values for <b>November–March</b> should be interpreted with caution — winter vegetation signals in the Chicago region are less reliable due to snow cover, leaf-off conditions, and low solar angle."),
                                      style = "color: #8B6914; background-color: #FFF8DC; border-left: 4px solid #DAA520; padding: 8px 12px; margin: 4px 0 10px 0; font-size: 18px;")
                                  )
                                ),
                                # Map layout
                                fluidRow(
                                  column(width = 5, 
                                         leafletOutput("il_county_map", height = "350px")),
                                  #Density plot graphs, put here for formatting
                                  tabBox(
                                    # title = tagList(shiny::icon("bars"), "General Information"),
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
                                    )
                                  )
                                  ),
                                fluidRow(
                                  #Density plot graphs, put here for formatting
                                  tabBox(
                                    width = 12,
                                    tabPanel(
                                      "Crop Plots",
                                      h6(HTML("Current smoothed NDVI time series (blue) versus normal (black).<br>")),
                                      plotOutput("crop_currentTS_plot"),
                                      h6(HTML("<br><br>Distribution of normal crop NDVI for an entire year with the current NDVI (blue diamond) and
                                              normal for the current day of the year (orange circle) shown with a 95% confidence interval surrounding them. Non-overlapping intervals indicate the landcover is significantly greener or browner than normal.<br>")),
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
                                      h6(HTML("<br><br>Distribution of normal medium intensity urban NDVI for an entire year with the current NDVI (blue diamond) and
                                              normal for the current day of the year (orange circle) shown with a 95% confidence interval surrounding them. Non-overlapping intervals indicate the landcover is significantly greener or browner than normal.<br>")),
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
                                tabBox(
                                  id = "tab1",
                                  width = 12,
                                  tabPanel("Overview of Feature",
                                           h6(HTML("<b>Feature</b>: <u>NDVI Data Review</u><br><br>")),
                                           h6(HTML("<b>Purpose</b>: To help users explore patterns in NDVI data across land cover types and years. Was there a land cover type that dipped during a particular drought period? How did one year compare to another?")),
                                           h6(HTML("<b>Description</b>: Two views allow flexible exploration of the data:<br><br>
                                           <li><u>By Landcover</u> — one panel per selected land cover type; each colored line is a selected year plotted against the long-term normal (gray ribbon with dashed mean line)</li>
                                           <li><u>By Year</u> — one panel per selected year; each colored line is a selected land cover type plotted against the long-term normal</li><br>
                                           Both views include a <b>date window</b> to zoom in on a specific time of year (e.g., a declared drought period such as June 3–27).
                                           An optional <b>summary stats table</b> shows start, end, min, max, and change in NDVI for each land cover and year within the selected window, with the long-term normal appended for comparison.<br>")),
                                           h6(HTML("<b>Considerations</b>:<br>
                                                   <li><b>November–March NDVI values should be interpreted with caution</b> — winter vegetation signals in the Chicago region are less reliable due to snow cover, leaf-off conditions, and low solar angle.</li>
                                                   <li>At the start of each new year there is a small jump in the data. This is due to how the GAM smooths are fit year-by-year.</li>
                                                   <li>Date windows cannot span a year boundary (e.g., Dec 15 to Jan 15) — keep start and end in the same calendar year.</li><br><br>")),
                                           div(
                                             style = "text-align: center;",
                                             tags$img(src = 'LC_info_table.png', height = '400', width = '700')
                                           ),
                                           div(
                                             style = "text-align: center;",
                                             h6(HTML("<br><b>Landcover designations are from the U.S. National Landcover Database (NLCD)</b>")))
                                  ),
                                  tabPanel("By Landcover",
                                           sidebarLayout(
                                             sidebarPanel(width = 3,
                                               dateRangeInput("lc_date_window",
                                                              "Date window:",
                                                              start = paste0(yrNow, "-01-01"),
                                                              end   = paste0(yrNow, "-12-31")),
                                               checkboxGroupInput("lc_years",
                                                                  "Years to show:",
                                                                  choices  = as.character(sort(unique(NDVIall_years_modeled$year))),
                                                                  selected = as.character(sort(unique(NDVIall_years_modeled$year), decreasing = TRUE)[1:2]),
                                                                  inline   = TRUE),
                                               checkboxGroupInput("lc_types",
                                                                  "Land covers:",
                                                                  choices  = sort(unique(NDVIall_years_modeled$type)),
                                                                  selected = sort(unique(NDVIall_years_modeled$type))),
                                               checkboxInput("lc_show_stats", "Show summary stats", value = FALSE)
                                             ),
                                             mainPanel(width = 9,
                                               plotOutput("plot_by_lc", height = "600px"),
                                               conditionalPanel(
                                                 condition = "input.lc_show_stats == true",
                                                 tableOutput("stats_by_lc")
                                               )
                                             )
                                           )
                                  ),
                                  tabPanel("By Year",
                                           sidebarLayout(
                                             sidebarPanel(width = 3,
                                               dateRangeInput("yr_date_window",
                                                              "Date window:",
                                                              start = paste0(yrNow, "-01-01"),
                                                              end   = paste0(yrNow, "-12-31")),
                                               checkboxGroupInput("yr_years",
                                                                  "Years to show:",
                                                                  choices  = as.character(sort(unique(NDVIall_years_modeled$year))),
                                                                  selected = as.character(sort(unique(NDVIall_years_modeled$year), decreasing = TRUE)[1:2]),
                                                                  inline   = TRUE),
                                               checkboxGroupInput("yr_types",
                                                                  "Land covers:",
                                                                  choices  = sort(unique(NDVIall_years_modeled$type)),
                                                                  selected = sort(unique(NDVIall_years_modeled$type))),
                                               checkboxInput("yr_show_stats", "Show summary stats", value = FALSE)
                                             ),
                                             mainPanel(width = 9,
                                               plotOutput("plot_by_yr", height = "600px"),
                                               conditionalPanel(
                                                 condition = "input.yr_show_stats == true",
                                                 tableOutput("stats_by_yr")
                                               )
                                             )
                                           )
                                  )
                                )
                        ),
                        tabItem(tabName = "ndvi_diff",
                                tabBox(
                                  id = "tab2",
                                  width = 12,
                                  tabPanel("Overview of Feature",
                                           h6(HTML("<b>Feature</b>: <u>Heatmaps</u><br><br>")),
                                           h6(HTML("<b>Purpose</b>: To help users see patterns in NDVI data across all land cover types simultaneously — Was there a period of the year that was consistently below normal? Which land covers showed abnormal browning trends?")),
                                           h6(HTML("<b>Description</b>: Three heatmap views, each showing all 7 land cover types in a single faceted figure. Years can be toggled on and off for comparison.<br><br>
                                           <li><u>NDVI Anomaly</u> — how each year's NDVI compares to the long-term normal on each day (same categories as the Dashboard status boxes)</li>
                                           <li><u>Trend Direction</u> — whether vegetation is actively getting greener, getting browner, or showing no change on each day</li>
                                           <li><u>Trend Anomaly</u> — how the <i>rate of change</i> compares to the typical seasonal trajectory (e.g., 'Browning Faster than Normal' means greenness is declining more rapidly than expected for that time of year)</li><br>")),
                                           h6(HTML("<b>Considerations</b>:<br>
                                                   <li><b>November–March values should be interpreted with caution</b> — winter signals in the Chicago region are less reliable due to snow cover and leaf-off conditions.</li><br><br>")),
                                           div(
                                             style = "text-align: center;",
                                             tags$img(src = 'status_info_table.png', height = '400', width = '700')
                                           ),
                                           div(
                                             style = "text-align: center;",
                                             p(HTML("<br><b>How NDVI Anomaly categories are determined:</b> Each year's smoothed NDVI curve (and its 95% confidence interval) is compared to the long-term normal curve (and its 95% CI) for each day of year.
                                             <b>Significantly Greener/Browner</b> means the year's CI does not overlap the normal CI at all.
                                             <b>Slightly Greener/Browner</b> means the year's mean falls outside the normal CI but the intervals still overlap.
                                             <b>Normal</b> means the year's mean falls within the normal 95% CI.
                                             All comparisons use GAM-derived posterior confidence intervals.")))
                                  ),
                                  tabPanel("Heatmap Graphs",
                                    tags$style(HTML("
  #selected_years label {
    font-size: 13px;
    height: 10px;
  }
  #selected_years input[type='checkbox'] {
    transform: scale(0.8);
  }
")),
                                    checkboxGroupInput("selected_years", "Select Years:",
                                                       choices  = as.character(sort(unique(NDVIall_years_modeled$year), decreasing = TRUE)),
                                                       selected = as.character(sort(unique(NDVIall_years_modeled$year), decreasing = TRUE))[1:2],
                                                       inline   = TRUE),
                                    tabsetPanel(
                                      tabPanel("NDVI Anomaly",
                                               p(HTML("<br>How each year's NDVI compares to the long-term normal. Colors match the Dashboard status boxes.<br>")),
                                               plotOutput("ndvi_heatmap_all", height = "600px")
                                      ),
                                      tabPanel("Trend Direction",
                                               p(HTML("<br>Whether vegetation is actively <b style='color:#A0522D;'>getting browner</b>, showing <b style='color:#888;'>no change</b>, or <b style='color:#2E7D32;'>getting greener</b> on each day. This reflects the direction of the smoothed NDVI curve, not its level relative to normal.<br>")),
                                               plotOutput("trend_heatmap_all", height = "600px")
                                      ),
                                      tabPanel("Trend Anomaly",
                                               p(HTML("<br>How the <i>rate of change</i> in NDVI compares to the typical seasonal trajectory. <b>'Browning Faster than Normal'</b> means greenness is declining more rapidly than expected for that time of year; <b>'Abnormal Browning'</b> indicates an extreme departure from the normal rate of change.<br>")),
                                               plotOutput("trend_anomaly_heatmap_all", height = "600px")
                                      )
                                    )
                                  )
                        )),
                        tabItem(tabName = "specifics",
                                tabBox(
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

