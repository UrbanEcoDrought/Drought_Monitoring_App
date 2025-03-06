####################################################################################################################

#Purpose: Include graphing functions for Urban_Drought_app.R, will include all NDVI graph functions & CI Graph Functions 

####################################################################################################################

library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(lubridate)
library(tidyr)
library(tidyverse)
library(plotly)

# path.UrbDrought <- "/Users/jocelyngarcia/Library/CloudStorage/GoogleDrive-jgarcia@mortonarb.org/Shared drives/Urban Ecological Drought"
# path.UrbDrought <- "~/Google Drive/Shared drives/Urban Ecological Drought/"

#NDVI file path (Using NDVI data from NDVI Drought Monitoring Workflow so they are fit to the spline)
NDVI_data <- read_csv("data/allNDVI_data.csv")%>%
  mutate(date = as.Date(date, format="%Y-%m-%d"))
NDVI_data$date <- as.Date(NDVI_data$date)


#CSV file path (Using CSV data from NDVI Drought Monitoring Workflow )
CI_csv <- read_csv("data/k=12_norms_all_LC_types.csv")

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
