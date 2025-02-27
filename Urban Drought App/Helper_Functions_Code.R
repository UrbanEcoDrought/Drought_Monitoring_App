####################################################################################################################

#Purpose: Include any helper functions & code for Urban_Drought_app.R

####################################################################################################################

library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(lubridate)
library(tidyverse)
library(tidyr)
library(tidyquant)
library(scales)

source("Graph_plotting.R") # some of the work is already done in this file 

NDVI_data <-read_csv("/Users/jocelyngarcia/Documents/GitHub/UrbanDrought_SpatialAnalysis_Chicago/Urban Drought App/drought_monitoring_csvs/raw_data_k=12.csv")%>%
  mutate(date = as.Date(date, format="%Y-%m-%d"))

CI_csv <- read_csv("/Users/jocelyngarcia/Documents/GitHub/UrbanDrought_SpatialAnalysis_Chicago/Urban Drought App/drought_monitoring_csvs/k=12_norms_all_LC_types.csv")

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
###############################################################
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
  status <- round((most_recent_subset$NDVI - mean_value), digits = 5)
  
  # Determine color based on status
  color <- ifelse(status >= 0, "green",
                  ifelse(status >= -0.01, "yellow",
                         ifelse(status >= -0.02, "orange", "red")))
  
  
  
  return(list(status = status, color = color))
}

#Percentile function
ndvi_percentile <- function(LCtype, NDVI_data, CI_csv, most_recent_data){
  
  NDVI_subset <- filter(NDVI_data, type == LCtype)
  CI_subset <- filter(CI_csv, type == LCtype)
  most_recent_subset <- filter(most_recent_data, type == LCtype)
  
  # Compute the percentile of the current NDVI value
  current_NDVI <- most_recent_subset$NDVI[1]  # Extract current NDVI value
  ecdf_function <- ecdf(NDVI_subset$NDVI)  # Create empirical cumulative distribution function
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
  return(paste("Daily difference in NDVI:", difference))
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
  return(paste("Weekly difference in NDVI:", difference))
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
  difference <- round((most_recent_day$NDVI - second_day$NDVI), 3)
  
  # Return monthly difference as a string
  return(paste("Monthly difference in NDVI:", difference))
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
  difference <- round((most_recent_day$NDVI - second_day$NDVI), 3)
  
  # Return yearly difference as a string
  return(paste("Yearly difference in NDVI:", difference))
}
####################################################################################################################
#Heat Map

plot_ndvi_heatmap <- function(data, selected_years) {
  if (length(selected_years) == 0) return(ggplot() + ggtitle("No years selected"))  # Prevents failure
  
  filtered_data <- data %>% filter(year %in% selected_years)
  
  ggplot(filtered_data, aes(x = yday, y = factor(year))) +
    geom_tile(aes(fill = difference), color = "gray40") +  # Slightly darker grid lines
    scale_fill_gradientn(
      colors = c("#4575b4", "#91bfdb", "#ffffbf", "#fdae61", "#d73027"), 
      values = rescale(c(-0.5, -0.2, 0, 0.2, 0.5)),  # Ensures smooth transition
      name = "NDVI Difference",
      limits = c(-0.5, 0.5)
    ) +
    scale_x_continuous(
      breaks = seq(0, 330, by = 30),
      labels = month.abb
    ) +
    labs(x = "Month of Year", y = "Year", title = "Heatmap of NDVI Differences") +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, color = "black"),
      axis.text.y = element_text(color = "black"),
      plot.title = element_text(face = "bold", size = 16, color = "black"),
      panel.background = element_rect(fill = "gray90"),  # Light gray background
      legend.key.height = unit(1, "cm")
    )
}
