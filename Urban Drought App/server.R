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

# source("Graph_Plotting.R")
# source("Helper_Functions_Code.R")


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
# All data overview graph
all_data_graph <- function() {
  ggplot(NDVI_data, aes(x = date, y = NDVIReprojected, color = type)) +
    geom_line(size = 1) +
    scale_color_manual(values = c(
      "crop" = "#a50026",
      "forest" = "#d73027",
      "grassland" = "#f46d43",
      "urban-high" = "#fee090",
      "urban-medium" = "#74add1",
      "urban-low" = "#4575b4",
      "urban-open" = "#313695"
    )) +
    labs(
      x = "Date",
      y = "NDVI Value",
      title = "NDVI Trends Over Time for Selected Land Cover Types"
    ) +
    scale_x_date(
      date_breaks = "6 months",
      date_labels = "%b %Y"
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10)
    )
}


#!2 month overview graph 
twelve_month_graph <- function(start_date) {
  
  # Calculate end date (1 year after start date)
  end_date <- as.Date(start_date) %m+% years(1)
  
  # Filter the full data frame, not just the date column
  year_data <- NDVI_data %>%
    filter(date >= start_date & date <= end_date)
  
  # Check if year_data has rows
  if (nrow(year_data) == 0) {
    print("No data available for this date range.")
    return(NULL)
  }
  
  # Generate the plot
  ggplot(year_data, aes(x = date, y = NDVIReprojected, color = type)) +
    geom_line(size = 1) +
    scale_color_manual(values = c(
      "crop" = "#a50026",
      "forest" = "#d73027",
      "grass" = "#f46d43",
      "urban-high" = "#fee090",
      "urban-medium" = "#74add1",
      "urban-low" = "#4575b4",
      "urban-open" = "#313695"
    )) +
    labs(
      y = "NDVI Value",
      title = "NDVI Trends for Year Following Selected Start Date"
    ) +
    scale_x_date(
      breaks = seq(start_date, end_date, by = "months"),  
      date_labels = "%b %Y"
    )
}


#Monthly overview graph
monthly_graph <- function(mstart_date) {
  
  mstart_date <- as.Date(mstart_date)
  
  # Calculate end date (1 month after start date)
  mend_date <- mstart_date %m+% months(1)
  
  # Filter the full data frame, not just the date column
  month_data <- NDVI_data %>%
    filter(date >= mstart_date & date <= mend_date)
  
  
  # Generate the plot
  ggplot(month_data, aes(x = date, y = NDVIReprojected, color = type)) +
    geom_line(size = 1) +
    scale_color_manual(values = c(
      "crop" = "#a50026",
      "forest" = "#d73027",
      "grass" = "#f46d43",
      "urban-high" = "#fee090",
      "urban-medium" = "#74add1",
      "urban-low" = "#4575b4",
      "urban-open" = "#313695"
    )) +
    labs(
      x = "Date",
      y = "NDVI Value",
      title = "NDVI Trends for Month Following Selected Start Date"
    ) +
    scale_x_date(
      breaks = seq(mstart_date, mend_date, by = "7 days"),  
      labels = scales::date_format("%B %d")
    )
}



#Weekly Overview graph
weekly_graph <- function(wstart_date) {
  wstart_date <- as.Date(wstart_date)
  
  
  # Calculate end date (1 week after start date)
  wend_date <- wstart_date + 7
  
  # Filter the data
  week_data <- NDVI_data %>%
    filter(date >= wstart_date & date <= wend_date & !is.na(NDVI))
  
  
  ggplot(week_data, aes(x = date, y = NDVIReprojected, color = type)) +
    geom_line(size = 1) +
    scale_color_manual(values = c(
      "crop" = "#a50026",
      "forest" = "#d73027",
      "grassland" = "#f46d43",
      "urban-high" = "#fee090",
      "urban-medium" = "#74add1",
      "urban-low" = "#4575b4",
      "urban-open" = "#313695"
    )) +
    labs(
      x = "Date",
      y = "NDVI Value",
      title = "NDVI Trends for Week Following Selected Start Date"
    ) +
    scale_x_date(
      breaks = seq(wstart_date, wend_date, by = "1 day"),  
      labels = scales::date_format("%B %d")
    )
}

####################################################################################################################

#Working on 95% CI interval

#All 7 LC types 95% CI graph
all_LC_CI_graph <-function(){
  
  ggplot(CI_csv, aes(x = yday, y = mean, color = type)) + 
    geom_line(size = 1) +  
    geom_ribbon(aes(ymin = lwr, ymax = upr, fill = type), alpha = 0.2) + 
    scale_color_manual(values = c(
      "crop" = "#a50026",
      "forest" = "#d73027",
      "urban-high" = "#fee090",
      "urban-medium" = "#74add1",
      "urban-low" = "#4575b4",
      "urban-open" = "#313695",
      "grassland" = "#f46d43"
    )) +
    scale_fill_manual(values = c(
      "crop" = "#a50026",
      "forest" = "#d73027",
      "grassland" = "#f46d43",
      "urban-high" = "#fee090",
      "urban-medium" = "#74add1",
      "urban-low" = "#4575b4",
      "urban-open" = "#313695"
    )) +
    labs(title = "95% Confidence Intervals for All LC Type over 365 Days", x = "Day of Year", y = "Mean Value") +
    theme_minimal()
  
}

#95% CI for selected LC Types
selected_LC_CI_graph <- function(LC_types){
  LC_CI <- CI_csv %>%
    filter(type %in% LC_types)  # Filter multiple selected types
  
  ggplot(LC_CI, aes(x = yday, y = mean, color = type, fill = type)) + 
    geom_line(size = 1) +  
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +  
    scale_color_manual(values = c(
      "crop" = "#a50026",
      "forest" = "#d73027",
      "grassland" = "#f46d43",
      "urban-high" = "#fee090",
      "urban-medium" = "#74add1",
      "urban-low" = "#4575b4",
      "urban-open" = "#313695"
    )) +
    scale_fill_manual(values = c(
      "crop" = "#a50026",
      "forest" = "#d73027",
      "grassland" = "#f46d43",
      "urban-high" = "#fee090",
      "urban-medium" = "#74add1",
      "urban-low" = "#4575b4",
      "urban-open" = "#313695"
    )) +
    labs(title = "95% Confidence Intervals for Selected LC Type(s) Over 365 Days", 
         x = "Day of Year", 
         y = "Mean Value") +
    theme_minimal() +
    theme(legend.title = element_blank())  # Removes the legend title for better appearance
}

####################################################################################################################
###################################################
#Preliminary work

#putting NDVI_data in order by date
NDVI_data <-NDVI_data[order(as.Date(NDVI_data$date, format="%Y-%m-%d"), decreasing = TRUE), ]

head(NDVI_data)

#finding latest day & pulling date
latest_day<-head(NDVI_data, 1)
date_needed <-latest_day$date

#pulling any rows with matching date 
most_recent_data<- filter(NDVI_data, date == date_needed)
###################################################

#density plot
#Notes: we need 7 density plots with the most recent data display (latest day) and upper and lower bound shown and mean and then the lastest day NDVI as a point
# Distribution plot is of NDVI data, and updates as we get more NDVI data from satellites 

density_plot <- function(LCtype, naming, NDVI_data, CI_csv, most_recent_data) {
  
  NDVI_subset <- filter(NDVI_data, type == LCtype)
  CI_subset <- filter(CI_csv, type == LCtype)
  most_recent_subset <- filter(most_recent_data, type == LCtype)
  
  #finding yday
  recent_yday <- most_recent_subset$yday[1]
  
  CI_final_subset <- filter(CI_subset, yday == recent_yday)
  
  # Extract values for bounds and mean from the first row of CI_subset (or whichever logic you want to apply)
  lwr <- CI_final_subset$lwr[1]
  upr <- CI_final_subset$upr[1]
  mean_value <- CI_final_subset$mean[1]
  
  # Plot
  plot <- ggplot(NDVI_subset, aes(x = NDVIMissionPred)) + 
    geom_density(aes(y = after_stat(density) / max(after_stat(density))), fill = "#c2a5cf", alpha = 0.5)+
    
    # Add the bounds as dashed lines with legend
    geom_vline(aes(xintercept = lwr), linetype = "dashed", color = "#40004b", size = 1) +
    geom_vline(aes(xintercept = upr), linetype = "dashed", color = "#40004b", size = 1) +
    
    # Mean point
    geom_point(aes(x = mean_value, y = 0, shape = "Mean"), color = "#40004b", size = 4) +
    
    # Current NDVI point (diamond)
    geom_point(aes(x = most_recent_subset$NDVIMissionPred, y = 0, shape = "Current NDVI"), fill = "#1b7837", color = "#1b7837", size = 4) +
    
    # Labels
    labs(
      x = paste0(naming, " Density Plot"),  # Dynamic x-axis label using the 'naming' parameter
      y = "Density",
      linetype = "Bound Type",  # Legend title for the lines
      shape = "Point Type"      # Legend title for the points
    ) +
    
    # Manual legend adjustments
    scale_linetype_manual(values = c("Lower Bound" = "dashed", "Upper Bound" = "dashed")) +
    scale_shape_manual(values = c("Mean" = 16, "Current NDVI" = 23)) +
    
    theme_minimal()
  
  # Return the plot
  return(plot)
}

# Test the function with each land cover type (all inputs should now be lowercase)
#ul_plot <- density_plot("urban-low", "Urban-Low", NDVI_data, CI_csv, most_recent_data)
#print(ul_plot)

#forest_plot <- density_plot("forest", "Forest", NDVI_data, CI_csv, most_recent_data)
#print(forest_plot)

#grassland_plot <- density_plot("grassland", "Grassland", NDVI_data, CI_csv, most_recent_data)
#print(grassland_plot)


#Making sure status match to what I got when I codede everything the long way 
#uo_status <- round((uo_most_recent$NDVI - uo$mean), digits = 5)
#f_status <- round((forest_most_recent$NDVI - f$mean), digits = 5)

########################################################################################################################


####################################################################################################################
#Function to determine color of KPI Status Box for each LC Type 
#Purpose: Need to pull mean and most recent NDVI value, find difference, and set box color to reflect it's status 

###############################################################
#Used in Stats Change functions too
#putting NDVI_data in order by date
NDVI_data <-NDVI_data[order(as.Date(NDVI_data$date, format="%Y-%m-%d"), decreasing = TRUE), ]

head(NDVI_data)

#Pulling yday of latest data (most recent day) 

#finding latest day & pulling date
latest_day<-head(NDVI_data, 1)
date_needed <-as.Date(latest_day$date)

#pulling any rows with matching date 
most_recent_data<- filter(NDVI_data, date == date_needed)

#Status Boxes Function
LC_status <- function(LC_type, NDVI_data, CI_csv, most_recent_data) {
  
  # Ensure consistent use of LC_type in filter() calls
  NDVI_subset <- filter(NDVI_data, type == LC_type)
  CI_subset <- filter(CI_csv, type == LC_type)
  most_recent_subset <- filter(most_recent_data, type == LC_type)
  
  # Check if most_recent_subset has any data
  if (nrow(most_recent_subset) == 0) {
    return(NULL)  # Avoid errors if no data matches
  }
  
  # Extract recent yday
  recent_yday <- most_recent_subset$yday[1]
  CI_final_subset <- filter(CI_subset, yday == recent_yday)
  
  # Ensure CI_final_subset is not empty before extracting mean
  if (nrow(CI_final_subset) == 0) {
    return(NULL)  # Handle missing data
  }
  
  # Extract mean value
  mean_value <- CI_final_subset$mean[1]
  
  # Compute status
  status <- round((most_recent_subset$NDVIMissionPred - mean_value), digits = 5)
  
  # Determine color based on status
  color <- ifelse(most_recent_subset$NDVIMissionPred >= CI_final_subset$upr, "blue",
                  ifelse(most_recent_subset$NDVIMissionPred < CI_final_subset$upr & most_recent_subset$NDVIMissionPred >= CI_final_subset$lwr, "yellow", "orange"))
  
  
  return(list(status = status, color = color))
}


#Percentile function
ndvi_percentile <- function(LCtype, current_time_period, CI_csv, most_recent_data){
  
  NDVI_subset <- filter(current_time_period, type == LCtype)
  CI_subset <- filter(CI_csv, type == LCtype)
  most_recent_subset <- filter(most_recent_data, type == LCtype)
  
  # Compute the percentile of the current NDVI value
  current_NDVI <- most_recent_subset$NDVIReprojected[1]  # Extract current NDVI value
  ecdf_function <- ecdf(NDVI_subset$NDVIReprojected)  # Create empirical cumulative distribution function
  current_percentile <- ecdf_function(current_NDVI) * 100  # Convert to percentage
  
  return(current_percentile)
}
####################################################################################################################
#Function to generate change stats for density plot 
# Doesn't handle if one of the start or end values is NA

daily_change <- function(LC_type, date_needed, NDVI_data) {
  prev_day <- date_needed - 1
  
  # Filtering by LC type 
  NDVI_subset <- filter(NDVI_data, type == LC_type)
  
  # Filtering for days 
  daily_range <- filter(NDVI_subset, NDVI_subset$date == date_needed | NDVI_subset$date == prev_day)
  
  if (nrow(daily_range) < 2) {
    return("Insufficient data")  # Handle case where not enough data
  }
  
  most_recent_day <- daily_range[daily_range$date == date_needed, ]
  second_day <- daily_range[daily_range$date == prev_day, ]
  
  # Difference
  difference <- round((most_recent_day$NDVI - second_day$NDVI), 3)
  
  # Return daily difference as a string
  return(paste("Daily difference in NDVI: ", difference))
}
#####################################################
weekly_change <- function(LC_type, date_needed, NDVI_data) {
  # Ensure date_needed is a Date object
  date_needed <- as.Date(date_needed)  
  prev_week <- date_needed - 7  # Calculate the date one week before
  
  # Ensure NDVI_data has date as Date type
  NDVI_data <- NDVI_data %>%
    mutate(date = as.Date(date))
  
  # Filter by land cover type
  NDVI_subset <- NDVI_data %>%
    filter(type == LC_type)
  
  # Get closest available date on or before date_needed
  most_recent_day <- NDVI_subset %>%
    filter(date <= date_needed) %>%
    arrange(desc(date)) %>%
    slice(1)
  
  # Get closest available date on or before prev_week
  second_day <- NDVI_subset %>%
    filter(date <= prev_week) %>%
    arrange(desc(date)) %>%
    slice(1)
  
  # Check if both dates exist
  if (nrow(most_recent_day) == 0 | nrow(second_day) == 0) {
    return("Insufficient data")
  }
  
  # Compute NDVI difference
  difference <- round((most_recent_day$NDVI - second_day$NDVI), 3)
  
  # Return weekly difference as a string
  return(paste("Weekly difference in NDVI: ", difference))
}
#####################################################
monthly_change <- function(LC_type, date_needed, NDVI_data) {
  # Ensure date_needed is a Date object
  date_needed <- as.Date(date_needed)  
  prev_month <- date_needed - 30  # Get date 30 days before
  
  # Ensure NDVI_data$date is in Date format
  NDVI_data <- NDVI_data %>%
    mutate(date = as.Date(date))
  
  # Filter by land cover type
  NDVI_subset <- NDVI_data %>%
    filter(type == LC_type)
  
  # Get closest available date on or before date_needed
  most_recent_day <- NDVI_subset %>%
    filter(date <= date_needed) %>%
    arrange(desc(date)) %>%
    slice(1)
  
  # Get closest available date on or before prev_month
  second_day <- NDVI_subset %>%
    filter(date <= prev_month) %>%
    arrange(desc(date)) %>%
    slice(1)
  
  # Check if both dates exist
  if (nrow(most_recent_day) == 0 | nrow(second_day) == 0) {
    return("Insufficient data")
  }
  
  # Compute NDVI difference
  difference <- round((most_recent_day$NDVIReprojected - second_day$NDVIReprojected), 3)
  
  # Return monthly difference as a string
  return(paste("Monthly difference in NDVI: ", difference))
}
#####################################################
yearly_change <- function(LC_type, date_needed, NDVI_data) {
  # Ensure date_needed is a Date object
  date_needed <- as.Date(date_needed)  
  prev_year <- date_needed - 365  # Get date 1 year before
  
  # Ensure NDVI_data$date is in Date format
  NDVI_data <- NDVI_data %>%
    mutate(date = as.Date(date))
  
  # Filter by land cover type
  NDVI_subset <- NDVI_data %>%
    filter(type == LC_type)
  
  # Get closest available date on or before date_needed
  most_recent_day <- NDVI_subset %>%
    filter(date <= date_needed) %>%
    arrange(desc(date)) %>%
    slice(1)
  
  # Get closest available date on or before prev_year
  second_day <- NDVI_subset %>%
    filter(date <= prev_year) %>%
    arrange(desc(date)) %>%
    slice(1)
  
  # Check if both dates exist
  if (nrow(most_recent_day) == 0 | nrow(second_day) == 0) {
    return("Insufficient data")
  }
  
  # Compute NDVI difference
  difference <- round((most_recent_day$NDVIMissionPred - second_day$NDVIMissionPred), 3)
  
  # Return yearly difference as a string
  return(paste("Yearly difference in NDVI: ", difference))
}
####################################################################################################################
#Heat Map

plot_ndvi_heatmap <- function(filtered_data, selected_years, LC_type, naming) {
  if (length(selected_years) == 0) return(ggplot() + ggtitle("No years selected"))  
  
  # Ensure yday is calculated from the Date field
  filtered_data <- filtered_data %>%
    mutate(
      yday = as.numeric(format(date, "%j")),  # Ensure yday is numeric using the full date
      year = factor(year, levels = rev(unique(year))),  # Ensure proper year ordering
      status_category = factor(case_when(
        NDVIMissionPred >= upr ~ "Blue: At or Above Upper Bound",
        NDVIMissionPred >= lwr & NDVIMissionPred < upr ~ "Yellow: Within Confidence Interval",
        NDVIMissionPred < lwr ~ "Orange: Below Lower Bound"
      ), levels = c("Blue: At or Above Upper Bound",
                    "Yellow: Within Confidence Interval",
                    "Orange: Below Lower Bound")))  
  
  # Filter data based on selected years & LC_type
  filtered_data <- filtered_data %>% filter(year %in% selected_years) %>% filter(type == LC_type)
  
  ggplot(filtered_data, aes(x = yday, y = year)) +
    geom_tile(aes(fill = status_category), width = 1, height = 1) +  # Set width and height to 1 for pixel-like tiles
    scale_fill_manual(
      values = c("Blue: At or Above Upper Bound" = "blue",
                 "Yellow: Within Confidence Interval" = "yellow", 
                 "Orange: Below Lower Bound" = "orange"),
      name = "NDVI Category"
    ) +
    scale_x_continuous(
      expand = c(0, 0),
      breaks = seq(1, 366, by = 31),  # Cumulative days for each month
      labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    ) +
    scale_y_discrete(expand = c(0, 0)) +
    labs(x = "Month of Year", y = "Year", title = paste0(naming, " Heat Map")) +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
      axis.text.y = element_text(size = 8),
      axis.title.x = element_text(face = "bold", size = 10),
      axis.title.y = element_text(face = "bold", size = 10),
      plot.title = element_text(face = "bold", size = 12),
      legend.key.height = unit(1, "cm"),
      legend.position = "bottom",
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 10),
      # Set the background color of the plot area and entire plot
      panel.background = element_rect(fill = "gray99"),  # Background for the plot area
      plot.background = element_rect(fill = "gray99")    # Background for the entire plot
    )
}


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
