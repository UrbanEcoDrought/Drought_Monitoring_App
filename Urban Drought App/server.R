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

# Define server logic
server <- function(input, output, session) {
  
  # Render the map
  output$il_county_map <- renderLeaflet({
    # Initialize the map
    map <- leaflet() %>%
      addTiles() %>%  # Blank map background
      setView(lng = -88, lat = 41.8, zoom = 8)  # Center the map on Illinois
    
    # Loop to add each county as a toggleable layer
    for (county_name in unique(il_counties$NAME)) {
      county_data <- il_counties[il_counties$NAME == county_name, ]
      
      # Add individual county polygons
      map <- map %>%
        addPolygons(
          data = county_data,
          color = "#444444",
          weight = 1,
          opacity = 1,
          fillOpacity = 0.6,
          fillColor = "#FFEDA0",
          label = ~NAME,
          group = county_name
        )
    }
    # Add layer control for individual counties
    map %>%
      addLayersControl(
        overlayGroups = unique(il_counties$NAME),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  ####################################################################################################################
  #Status KPI Boxes for each LC Type
  output$cropBox <- renderValueBox({
    result <- LC_status("crop", NDVI_data, CI_csv, most_recent_data)
    
    valueBox(
      value = "Crop Status",
      subtitle = result$status,  
      color = result$color
    )
  }) 
  
  
  output$forBox <- renderValueBox({
    result <- LC_status("forest", NDVI_data, CI_csv, most_recent_data)
    
    valueBox(
      value = "Forest Status",
      subtitle = result$status,  
      color = result$color  
    )
  })
  
  output$grassBox <- renderValueBox({
    result <- LC_status("grassland", NDVI_data, CI_csv, most_recent_data)
    
    valueBox(
      value = "Grassland Status",
      subtitle = result$status,
      color = result$color  
    )
  })
  
  output$uhBox <- renderValueBox({
    result <- LC_status("urban-high", NDVI_data, CI_csv, most_recent_data)
    
    valueBox(
      value = "Urban-High Status",
      subtitle = result$status,  
      color = result$color  
    )
  })
  
  output$umBox <- renderValueBox({
    result <- LC_status("urban-medium", NDVI_data, CI_csv, most_recent_data)
    
    valueBox(
      value = "Urban-Medium Status",
      subtitle = result$status,  
      color = result$color  
    )
  })
  
  output$ulBox <- renderValueBox({
    result <- LC_status("urban-low", NDVI_data, CI_csv, most_recent_data)
    
    valueBox(
      value = "Urban-Low Status",
      subtitle = result$status,  
      color = result$color  
    )
  })
  
  output$uoBox <- renderValueBox({
    result <- LC_status("urban-open", NDVI_data, CI_csv, most_recent_data)
    
    valueBox(
      value = "Urban-Open Status",
      subtitle = result$status,  
      color = result$color  
    )
  })
  
  
  ####################################################################################################################
  #NDVI graphs
  #All Data
  output$all_data_graph <- renderPlot({
    all_data_graph()  
  })
  #Yearly Data
  output$yearly_graph <- renderPlot({
    req(input$start_date) 
    print(paste("Start date selected:", input$start_date))
    plot <- twelve_month_graph(input$start_date)
    if (!is.null(plot)) {
      print("Plot generated successfully")
      return(plot)
    } else {
      print("No data available for this date range.")
      return(NULL)
    }
  })
  
  
  #Monthly Data
  output$monthly_graph <- renderPlot({
    req(input$mstart_date) 
    plot <- monthly_graph(input$mstart_date)
    if (!is.null(plot)) {
      return(plot)
    } else {
      return(NULL)
    }
  })
  
  #Weekly Data
  output$weekly_graph <- renderPlot({
    req(input$wstart_date) 
    plot <- weekly_graph(input$wstart_date)
    if (!is.null(plot)) {
      return(plot)
    } else {
      return(NULL)
    }
  })
  
  ####################################################################################################################
  #For Current NDVI value percentile
  output$percentile_crop <- renderText({
    # Ensure necessary inputs exist
    req(NDVI_data, CI_csv, most_recent_data)
    
    # Calculate percentile
    percentile <- ndvi_percentile("crop", current_time_period, CI_csv, most_recent_data)
    
    # Ensure it returns a valid value
    if (is.null(percentile) || is.na(percentile)) {
      return("Crop NDVI Percentile: Data Unavailable")
    } else {
      return(paste0("Crop NDVI Percentile: ", round(percentile, 1), "%"))
    }
  })
  
  output$percentile_for <- renderText({
    # Ensure necessary inputs exist
    req(NDVI_data, CI_csv, most_recent_data)
    
    # Calculate percentile
    percentile <- ndvi_percentile("forest", current_time_period, CI_csv, most_recent_data)
    
    # Ensure it returns a valid value
    if (is.null(percentile) || is.na(percentile)) {
      return("Forest NDVI Percentile: Data Unavailable")
    } else {
      return(paste0("Forest NDVI Percentile: ", round(percentile, 1), "%"))
    }
  })
  
  output$percentile_grass <- renderText({
    # Ensure necessary inputs exist
    req(NDVI_data, CI_csv, most_recent_data)
    
    # Calculate percentile
    percentile <- ndvi_percentile("grassland", current_time_period, CI_csv, most_recent_data)
    
    # Ensure it returns a valid value
    if (is.null(percentile) || is.na(percentile)) {
      return("Grassland NDVI Percentile: Data Unavailable")
    } else {
      return(paste0("Grassland NDVI Percentile: ", round(percentile, 1), "%"))
    }
  })
  
  output$percentile_uh <- renderText({
    # Ensure necessary inputs exist
    req(NDVI_data, CI_csv, most_recent_data)
    
    # Calculate percentile
    percentile <- ndvi_percentile("urban-high", current_time_period, CI_csv, most_recent_data)
    
    # Ensure it returns a valid value
    if (is.null(percentile) || is.na(percentile)) {
      return("Urban-High NDVI Percentile: Data Unavailable")
    } else {
      return(paste0("Urban-High NDVI Percentile: ", round(percentile, 1), "%"))
    }
  })
  
  output$percentile_um <- renderText({
    # Ensure necessary inputs exist
    req(NDVI_data, CI_csv, most_recent_data)
    
    # Calculate percentile
    percentile <- ndvi_percentile("urban-medium", current_time_period, CI_csv, most_recent_data)
    
    # Ensure it returns a valid value
    if (is.null(percentile) || is.na(percentile)) {
      return("Urban-Medium NDVI Percentile: Data Unavailable")
    } else {
      return(paste0("Urban-Medium NDVI Percentile: ", round(percentile, 1), "%"))
    }
  })
  
  output$percentile_ul <- renderText({
    # Ensure necessary inputs exist
    req(NDVI_data, CI_csv, most_recent_data)
    
    # Calculate percentile
    percentile <- ndvi_percentile("urban-low", current_time_period, CI_csv, most_recent_data)
    
    # Ensure it returns a valid value
    if (is.null(percentile) || is.na(percentile)) {
      return("Urban-Low NDVI Percentile: Data Unavailable")
    } else {
      return(paste0("Urban-Low NDVI Percentile: ", round(percentile, 1), "%"))
    }
  })
  
  output$percentile_uo <- renderText({
    # Ensure necessary inputs exist
    req(NDVI_data, CI_csv, most_recent_data)
    
    # Calculate percentile
    percentile <- ndvi_percentile("urban-open", current_time_period, CI_csv, most_recent_data)
    
    # Ensure it returns a valid value
    if (is.null(percentile) || is.na(percentile)) {
      return("Urban-Open NDVI Percentile: Data Unavailable")
    } else {
      return(paste0("Urban-Open NDVI Percentile: ", round(percentile, 1), "%"))
    }
  })
  
  ####################################################################################################################
  #Density Plots
  output$crop_density_plot <- renderPlotly({
    crop_plot <- density_plot("crop", "Crop", current_time_period, CI_csv, most_recent_data)
    print(crop_plot)
  })
  
  output$forest_density_plot <- renderPlotly({
    forest_plot <- density_plot("forest", "Forest", current_time_period, CI_csv, most_recent_data)
    print(forest_plot)
  })
  
  output$grassland_density_plot <- renderPlotly({
    grassland_plot <- density_plot("grassland", "Grassland", current_time_period, CI_csv, most_recent_data)
    print(grassland_plot)
  })
  
  output$uh_density_plot <- renderPlotly({
    uh_plot <- density_plot("urban-high", "Urban-High", current_time_period, CI_csv, most_recent_data)
    print(uh_plot)
  })
  
  output$um_density_plot <- renderPlotly({
    um_plot <- density_plot("urban-medium", "Urban-Medium", current_time_period, CI_csv, most_recent_data)
    print(um_plot)  
  })
  
  output$ul_density_plot <- renderPlotly({
    ul_plot <- density_plot("urban-low", "Urban-Low", current_time_period, CI_csv, most_recent_data)
    print(ul_plot)  
  })
  
  output$uo_density_plot <- renderPlotly({
    uo_plot <- density_plot("urban-open", "Urban-Open", current_time_period, CI_csv, most_recent_data)
    print(uo_plot)
  })
  ####################################################################################################################
  #Change Stats
  output$crop_daily <- renderText({
    daily_diff <- daily_change("crop", date_needed, NDVI_data)
    weekly_diff <- weekly_change("crop", date_needed, NDVI_data)
    monthly_diff <-monthly_change("crop", date_needed, NDVI_data)
    yearly_diff <-yearly_change("crop", date_needed, NDVI_data)
    
    # Combine both outputs into a single string
    paste(daily_diff, " | ", weekly_diff," | ", monthly_diff," | ",yearly_diff)
    
  })
  
  output$for_daily<-renderText({
    daily_diff <- daily_change("forest", date_needed, NDVI_data)
    weekly_diff <- weekly_change("forest", date_needed, NDVI_data)
    monthly_diff <-monthly_change("forest", date_needed, NDVI_data)
    yearly_diff <-yearly_change("forest", date_needed, NDVI_data)
    
    # Combine both outputs into a single string
    paste(daily_diff, " | ", weekly_diff," | ", monthly_diff," | ",yearly_diff)
  })
  output$grass_daily<-renderText({
    daily_diff <- daily_change("grassland", date_needed, NDVI_data)
    weekly_diff <- weekly_change("grassland", date_needed, NDVI_data)
    monthly_diff <-monthly_change("grassland", date_needed, NDVI_data)
    yearly_diff <-yearly_change("grassland", date_needed, NDVI_data)
    
    # Combine both outputs into a single string
    paste(daily_diff, " | ", weekly_diff," | ", monthly_diff," | ",yearly_diff)
  })
  output$uh_daily<-renderText({
    daily_diff <- daily_change("urban-high", date_needed, NDVI_data)
    weekly_diff <- weekly_change("urban-high", date_needed, NDVI_data)
    monthly_diff <-monthly_change("urban-high", date_needed, NDVI_data)
    yearly_diff <-yearly_change("urban-high", date_needed, NDVI_data)
    
    # Combine both outputs into a single string
    paste(daily_diff, " | ", weekly_diff," | ", monthly_diff," | ",yearly_diff)
  })
  output$um_daily<-renderText({
    daily_diff <- daily_change("urban-medium", date_needed, NDVI_data)
    weekly_diff <- weekly_change("urban-medium", date_needed, NDVI_data)
    monthly_diff <-monthly_change("urban-medium", date_needed, NDVI_data)
    yearly_diff <-yearly_change("urban-medium", date_needed, NDVI_data)
    
    # Combine both outputs into a single string
    paste(daily_diff, " | ", weekly_diff," | ", monthly_diff," | ",yearly_diff)
  })
  output$ul_daily<-renderText({
    daily_diff <- daily_change("urban-low", date_needed, NDVI_data)
    weekly_diff <- weekly_change("urban-low", date_needed, NDVI_data)
    monthly_diff <-monthly_change("urban-low", date_needed, NDVI_data)
    yearly_diff <-yearly_change("urban-low", date_needed, NDVI_data)
    
    # Combine both outputs into a single string
    paste(daily_diff, " | ", weekly_diff," | ", monthly_diff," | ",yearly_diff)
  })
  output$uo_daily<-renderText({
    daily_diff <- daily_change("urban-open", date_needed, NDVI_data)
    weekly_diff <- weekly_change("urban-open", date_needed, NDVI_data)
    monthly_diff <-monthly_change("urban-open", date_needed, NDVI_data)
    yearly_diff <-yearly_change("urban-open", date_needed, NDVI_data)
    
    # Combine both outputs into a single string
    paste(daily_diff, " | ", weekly_diff," | ", monthly_diff," | ",yearly_diff)
  })
  ####################################################################################################################
  output$ndvi_heatmap_crop <- renderPlot({
    req(input$selected_years)  # Ensure at least one year is selected
    
    filtered_data <- heatmap_data %>% filter(year %in% input$selected_years)
    plot_ndvi_heatmap(filtered_data, input$selected_years, "crop", "Crop")  # Pass the filtered data
  })
  #####################
  output$ndvi_heatmap_forest <- renderPlot({
    req(input$selected_years)  # Ensure at least one year is selected
    
    filtered_data <- heatmap_data %>% filter(year %in% input$selected_years)
    plot_ndvi_heatmap(filtered_data, input$selected_years, "forest", "Forest")  # Pass the filtered data
  })
  #####################
  output$ndvi_heatmap_grass <- renderPlot({
    req(input$selected_years)  # Ensure at least one year is selected
    
    filtered_data <- heatmap_data %>% filter(year %in% input$selected_years)
    plot_ndvi_heatmap(filtered_data, input$selected_years, "grassland", "Grassland")  # Pass the filtered data
  })
  #####################
  output$ndvi_heatmap_uh <- renderPlot({
    req(input$selected_years)  # Ensure at least one year is selected
    
    filtered_data <- heatmap_data %>% filter(year %in% input$selected_years)
    plot_ndvi_heatmap(filtered_data, input$selected_years, "urban-high","Urban-High")  # Pass the filtered data
  })
  #####################
  output$ndvi_heatmap_um <- renderPlot({
    req(input$selected_years)  # Ensure at least one year is selected
    
    filtered_data <- heatmap_data %>% filter(year %in% input$selected_years)
    plot_ndvi_heatmap(filtered_data, input$selected_years, "urban-medium", "Urban-Medium")  # Pass the filtered data
  })
  #####################
  output$ndvi_heatmap_ul <- renderPlot({
    req(input$selected_years)  # Ensure at least one year is selected
    
    filtered_data <- heatmap_data %>% filter(year %in% input$selected_years)
    plot_ndvi_heatmap(filtered_data, input$selected_years,"urban-low", "Urban-Low")  # Pass the filtered data
  })
  #####################
  output$ndvi_heatmap_uo <- renderPlot({
    req(input$selected_years)  # Ensure at least one year is selected
    
    filtered_data <- heatmap_data %>% filter(year %in% input$selected_years)
    plot_ndvi_heatmap(filtered_data, input$selected_years,"urban-open", "Urban-Open")  # Pass the filtered data
  })
  ####################################################################################################################
}
