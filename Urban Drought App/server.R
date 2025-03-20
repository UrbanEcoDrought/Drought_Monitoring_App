####################################################################################################################

#Purpose: Be a visualization portal for the Urban Drought Project, meant to be a real time portal that is updated regularly
#Pulls from NDVI_drought_monitoring workflow & UrbanDrought_SpatialAnalysis_Chicago workflow

####################################################################################################################

library(shiny);library(shinydashboard);library(leaflet);library(leaflet.extras);library(sf)
library(tidyverse);library(ggplot2);library(DT);library(lubridate);library(ggplot2);library(hrbrthemes)
library(dplyr);library(lubridate);library(tidyverse);library(tidyr);library(tidyquant);library(scales)
library(plotly);library(dplyr);library(bs4Dash);library(shinyBS);library(shinycssloaders)

#For documentation of this app
#https://docs.google.com/document/d/1I8WkmUjuPLf0SS_IF0F6P97xyH3aQhth8m9iYUQM4hs/edit?usp=sharing

####################################################################################################################
#for testing
#Putting in all filepaths like this for now
NDVIall_normals_modeled <-read_csv("/Users/jocelyngarcia/Documents/GitHub/Drought_Monitoring_App/Urban Drought App/data/NDVIall_normals_modeled.csv")
NDVIall_years_modeled<-read_csv("/Users/jocelyngarcia/Documents/GitHub/Drought_Monitoring_App/Urban Drought App/data/NDVIall_years_modeled.csv")
####################################################################################################################
#Palettes
paletteLC <- c("crop"="#ab6c28", "forest"="#68ab5f", "grassland"="#dfdfc2", "urban-high"="#ab0000", "urban-medium"="#eb0000", "urban-low"="#d99282", "urban-open"="#dec5c5")
heatmap_colors <-c("Significantly Browner than Normal"= "maroon" , "Slightly Browner than Normal"= "pink", "Normal"="gray", "Slightly Greener than Normal"= "olive","Significantly Greener than Normal"="sucess")
graphing_colors<-c("Significantly Browner than Normal"= "maroon" , "Slightly Browner than Normal"= "pink", "Normal"="gray", "Slightly Greener than Normal"= "#3d9970","Significantly Greener than Normal"="#28a745")

# path.UrbDrought <- "/Users/jocelyngarcia/Library/CloudStorage/GoogleDrive-jgarcia@mortonarb.org/Shared drives/Urban Ecological Drought"
# path.UrbDrought <- "~/Google Drive/Shared drives/Urban Ecological Drought/"

####################################################################################################################
#####Uncomment after testing 
####NDVI file path (Using NDVI data from NDVI Drought Monitoring Workflow so they are fit to the spline)
#NDVIall_years_modeled <- read_csv("data/allNDVIall_years_modeled.csv")%>%
# mutate(date = as.Date(date, format="%Y-%m-%d"))
#NDVIall_years_modeled$date <- as.Date(NDVIall_years_modeled$date)
#####CSV file path (Using CSV data from NDVI Drought Monitoring Workflow )
#NDVIall_normals_modeled <- read_csv("data/k=12_norms_all_LC_types.csv")
####################################################################################################################

####################################################################################################################
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


##################################################

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
#Functions
####################################################################################################################

# All data overview graph
all_data_graph <- function() {
  ggplot(NDVIall_years_modeled, aes(x = date , y = YrMean, color = type, fill=type)) +
    geom_point(size = 1) +
    geom_smooth(method="gam", formula=y~s(x, bs="cs", k=12*25)) +
    scale_color_manual(values = paletteLC) +
    scale_fill_manual(values = paletteLC) +
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
  year_data <- NDVIall_years_modeled %>%
    filter(date >= start_date & date <= end_date)
  
  # Check if year_data has rows
  if (nrow(year_data) == 0) {
    print("No data available for this date range.")
    return(NULL)
  }
  
  # Generate the plot
  ggplot(year_data, aes(x = date, y = YrMean, color = type, fill=type)) +
    geom_point(size = 1) +
    geom_smooth(method="gam", formula=y~s(x, bs="cs", k=12)) +
    scale_color_manual(values = paletteLC) +
    scale_fill_manual(values = paletteLC) +
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
  month_data <- NDVIall_years_modeled %>%
    filter(date >= mstart_date & date <= mend_date)
  
  
  # Generate the plot
  ggplot(month_data, aes(x = date, y = YrMean, color = type, fill=type)) +
    geom_point(size = 1) +
    geom_smooth(method="gam", formula=y~s(x, bs="cs", k=3)) +
    scale_color_manual(values = paletteLC) +
    scale_fill_manual(values = paletteLC) +
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
  week_data <- NDVIall_years_modeled %>%
    filter(date >= wstart_date & date <= wend_date)
  
  
  ggplot(week_data, aes(x = date, y = YrMean, color = type, fill=type)) +
    geom_point(size = 1) +
    geom_smooth(method="lm") +
    scale_color_manual(values = paletteLC) +
    scale_fill_manual(values = paletteLC) +
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
#DENSITY PLOT FUNCTIONS
#Notes: we need 7 density plots with the most recent data display (latest day) and upper and lower bound shown and mean and then the lastest day NDVI as a point
# Distribution plot is of NDVI data, and updates as we get more NDVI data from satellites 

density_plot <- function(LCtype, naming, NDVIall_normals_modeled, NDVIall_years_modeled, most_recent_data) {
  #pulling these for the norm CI & NDVI CI
  norm_subset <- filter(NDVIall_normals_modeled, type == LCtype)
  ndvi_subset <-filter(NDVIall_years_modeled, type == LCtype)
  most_recent_subset <- filter(most_recent_data, type == LCtype)
  
  most_recent_subset$YrMean <- as.numeric(most_recent_subset$YrMean)
  
  #finding yday
  recent_yday <- most_recent_subset$yday[1]
  
  norm_final_subset <- filter(norm_subset, yday == recent_yday)
  ndvi_final_subset <-filter(ndvi_subset, yday == recent_yday)
  
  # Extract values for bounds and mean from the first row of CI_subset (or whichever logic you want to apply)
  NormLwr_current <- norm_final_subset$NormLwr[1]
  NormUpr_current <- norm_final_subset$NormUpr[1]
  NormMean_current <- norm_final_subset$NormMean[1]
  
  NDVILwr <-filter(ndvi_final_subset, year == latest_year)$YrLwr[1]
  NDVIUpr <-filter(ndvi_final_subset, year == latest_year)$YrUpr[1]
  NDVIMean <-filter(ndvi_final_subset, year == latest_year)$YrMean[1]
  
  # Plot
  plot <- ggplot(norm_subset, aes(x = NormMean)) + 
    geom_density(aes(y = after_stat(density) / max(after_stat(density))), fill = "#c2a5cf", alpha = 0.5) +
    
    # Norm CI & point
    geom_vline(aes(xintercept = NormLwr_current, color = "normal"), linetype = "dashed", size = 1) +
    geom_vline(aes(xintercept = NormUpr_current, color = "normal"), linetype = "dashed", size = 1) +
    geom_point(aes(x = NormMean_current, y = 0, shape = "normal", color = "normal"), size = 4) +
    
    # NDVI CI & point
    geom_vline(aes(xintercept = NDVILwr, color = "current"), linetype = "dashed", size = 1) +
    geom_vline(aes(xintercept = NDVIUpr, color = "current"), linetype = "dashed", size = 1) +
    geom_point(aes(x = NDVIMean, y = 0, shape = "current", color = "current"), size = 4) +
    
    # Labels
    labs(
      x = paste0(naming, " Density Plot"),  # Ensure 'naming' is defined
      y = "Density",
      color = "Legend",
      shape = "Legend"# Legend title for points
    ) +
    
    # Manual legend adjustments
    scale_shape_manual(values = c("normal" = 16, "current" = 18)) +
    scale_color_manual(values = c("normal" = "#40004b", "current" = "#1b7837")) +
    #scale_fill_manual(values = c("normal" = "#40004b", "current" = "#1b7837")) +
    theme_minimal()
  
  # Return the plot
  return(plot)
}
####################################################################################################################
#STATUS BOXES FUNCTION
#Needs color to be separate function for value boxes to work 

# Main function calling both functions
LC_status <- function(LC_type, NDVIall_years_modeled, NDVIall_normals_modeled, most_recent_data) {
  
  # Ensure consistent use of LC_type in filter() calls
  NDVI_subset <- filter(NDVIall_years_modeled, type == LC_type)
  CI_subset <- filter(NDVIall_normals_modeled, type == LC_type)
  most_recent_subset <- filter(most_recent_data, type == LC_type)
  
  # Check if most_recent_subset has any data
  if (nrow(most_recent_subset) == 0) {
    return(NULL)  # Avoid errors if no data matches
  }
  
  # Extract recent yday
  recent_yday <- most_recent_subset$yday[1]
  CI_final_subset <- filter(CI_subset, yday == latest_yday)
  
  # Ensure CI_final_subset is not empty before extracting mean
  if (nrow(CI_final_subset) == 0) {
    return(NULL)  # Handle missing data
  }
  
  # Extract mean value
  mean_value <- CI_final_subset$NormMean[1]
  
  # Convert to numeric
  most_recent_subset$YrMean <- as.numeric(most_recent_subset$YrMean)
  CI_final_subset$NormUpr <- as.numeric(CI_final_subset$NormUpr)
  CI_final_subset$NormLwr <- as.numeric(CI_final_subset$NormLwr)
  
  status <- round((most_recent_subset$YrMean - mean_value), digits = 2)
  
  color <- heatmap_colors[most_recent_subset$FlagNDVI]
  
  
  return(list(status = status, color = color))
}

####################################################################################################################
#PERCENTILE FUNCTIONS
ndvi_percentile <- function(LCtype, current_time_period, NDVIall_normals_modeled, most_recent_data){
  
  NDVI_subset <- filter(current_time_period, type == LCtype)
  CI_subset <- filter(NDVIall_normals_modeled, type == LCtype)
  most_recent_subset <- filter(most_recent_data, type == LCtype)
  
  # Compute the percentile of the current NDVI value
  current_NDVI <- most_recent_subset$YrMean[1]  # Extract current NDVI value
  ecdf_function <- ecdf(NDVI_subset$YrMean)  # Create empirical cumulative distribution function
  current_percentile <- ecdf_function(current_NDVI) * 100  # Convert to percentage
  
  return(current_percentile)
}
####################################################################################################################
#Function to generate change stats for density plot 
# Doesn't handle if one of the start or end values is NA

daily_change <- function(LC_type, date_needed, NDVIall_years_modeled) {
  prev_day <- date_needed - 1
  
  # Filtering by LC type 
  NDVI_subset <- filter(NDVIall_years_modeled, type == LC_type)
  
  # Filtering for days 
  daily_range <- filter(NDVI_subset, NDVI_subset$date == date_needed | NDVI_subset$date == prev_day)
  
  if (nrow(daily_range) < 2) {
    return("Insufficient data")  # Handle case where not enough data
  }
  
  most_recent_day <- daily_range[daily_range$date == date_needed, ]
  second_day <- daily_range[daily_range$date == prev_day, ]
  
  # Difference
  difference <- round((most_recent_day$YrMean - second_day$YrMean), 3)
  
  # Return daily difference as a string
  return(paste("Daily Change: ", difference))
}
#####################################################
weekly_change <- function(LC_type, date_needed, NDVIall_years_modeled) {
  # Ensure date_needed is a Date object
  date_needed <- as.Date(date_needed)  
  prev_week <- date_needed - 7  # Calculate the date one week before
  
  # Ensure NDVIall_years_modeled has date as Date type
  NDVIall_years_modeled <- NDVIall_years_modeled %>%
    mutate(date = as.Date(date))
  
  # Filter by land cover type
  NDVI_subset <- NDVIall_years_modeled %>%
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
  difference <- round((most_recent_day$YrMean - second_day$YrMean), 3)
  
  # Return weekly difference as a string
  return(paste("Weekly change: ", difference))
}
#####################################################
monthly_change <- function(LC_type, date_needed, NDVIall_years_modeled) {
  # Ensure date_needed is a Date object
  date_needed <- as.Date(date_needed)  
  prev_month <- date_needed - 30  # Get date 30 days before
  
  # Ensure NDVIall_years_modeled$date is in Date format
  NDVIall_years_modeled <- NDVIall_years_modeled %>%
    mutate(date = as.Date(date))
  
  # Filter by land cover type
  NDVI_subset <- NDVIall_years_modeled %>%
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
  difference <- round((most_recent_day$YrMean - second_day$YrMean), 3)
  
  # Return monthly difference as a string
  return(paste("Monthly change: ", difference))
}
#####################################################
yearly_change <- function(LC_type, date_needed, NDVIall_years_modeled) {
  # Ensure date_needed is a Date object
  date_needed <- as.Date(date_needed)  
  prev_year <- date_needed - 365  # Get date 1 year before
  
  # Ensure NDVIall_years_modeled$date is in Date format
  NDVIall_years_modeled <- NDVIall_years_modeled %>%
    mutate(date = as.Date(date))
  
  # Filter by land cover type
  NDVI_subset <- NDVIall_years_modeled %>%
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
  difference <- round((most_recent_day$YrMean - second_day$YrMean), 3)
  
  # Return yearly difference as a string
  return(paste("Yearly Change: ", difference))
}
####################################################################################################################
#Heat Map
plot_ndvi_heatmap <- function(NDVIall_years_modeled, selected_years, LC_type, naming) {
  if (length(selected_years) == 0) return(ggplot() + ggtitle("No years selected"))  
  
  selected_years <- as.numeric(selected_years)
  
  # Filter data based on selected years & LC_type
  filtered_data <- NDVIall_years_modeled %>%
    filter(year %in% selected_years, type == LC_type)
  
  
  
  # Generate the heatmap
  ggplot(filtered_data, aes(x = yday, y = factor(year))) +
    geom_tile(aes(fill = FlagNDVI), width = 1, height = 1) +  
    scale_fill_manual(
      values = graphing_colors, 
      name = "NDVI Category",
      drop = FALSE  
    ) +
    scale_x_continuous(
      expand = c(0, 0),
      breaks = seq(1, 366, by = 31),
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
      panel.background = element_rect(fill = "gray99"),
      plot.background = element_rect(fill = "gray99")
    )
}

####################################################################################################################
#END OF FUNCTIONS
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
  # Status KPI Boxes for each LC Type
  output$cropBox <- renderUI({
    result <- LC_status("crop", NDVIall_years_modeled, NDVIall_normals_modeled, most_recent_data)
    
    if (is.null(result)) {
      return(valueBox(
        "No recent crop data available", 
        subtitle = "No Data", 
        icon = icon("exclamation-circle"),
        color = "gray",  
        width = 11
      ))
    }
    
    # If data is available, display the result
    valueBox(
       "Crop ",
      subtitle = paste("is", result$status, "from the norm"),
      icon = icon("tractor"),
      color = result$color,  
      width = 11
    )
  })
  
  output$forBox <- renderUI({
    result <- LC_status("forest", NDVIall_years_modeled, NDVIall_normals_modeled, most_recent_data)
    
    if (is.null(result)) {
      return(valueBox(
        "No recent forest data available", 
        subtitle = "No Data", 
        icon = icon("exclamation-circle"),
        color = "gray",  
        width = 11
      ))
    }
    
    # If data is available, display the result
    valueBox(
      "Forest", 
      subtitle = paste("is", result$status, "from the norm"),
      icon = icon("tree"),
      color = result$color,  
      width = 11
    )
  })
  
  output$grassBox <- renderValueBox({
    result <- LC_status("grassland", NDVIall_years_modeled, NDVIall_normals_modeled, most_recent_data)
    
    if (is.null(result)) {
      return(valueBox(
        "No recent grass data available", 
        subtitle = "No Data", 
        icon = icon("exclamation-circle"),
        color = "gray", 
        width = 11
      ))
    }
    
    # If data is available, display the result
    valueBox(
     "Grass", 
      subtitle = paste("is", result$status, "from the norm"),
      icon = icon("seedling"),
      color = result$color,  
      width = 11
    )
  })
  
  output$uhBox <- renderValueBox({
    result <- LC_status("urban-high", NDVIall_years_modeled, NDVIall_normals_modeled, most_recent_data)
    
    if (is.null(result)) {
      return(valueBox(
        "No recent urban-high data available", 
        subtitle = "No Data", 
        icon = icon("exclamation-circle"),
        color = "gray",  
        width = 11
      ))
    }
    
    # If data is available, display the result
    valueBox(
      "Urban-High", 
      subtitle = paste("is", result$status, "from the norm"),
      icon = icon("city"),
      color = result$color,  
      width = 11
    )
  })
  
  output$umBox <- renderValueBox({
    result <- LC_status("urban-medium", NDVIall_years_modeled, NDVIall_normals_modeled, most_recent_data)
    
    if (is.null(result)) {
      return(valueBox(
        "No recent urban-medium data available", 
        subtitle = "No Data", 
        icon = icon("exclamation-circle"),
        color = "gray",  
        width = 11
      ))
    }
    
    # If data is available, display the result
    valueBox(
     "Urban-Medium", 
      subtitle = paste(" is ", result$status, "from the norm"),
      icon = icon("building-columns"),
      color = result$color,  
      width = 11
    )
  })
  
  output$ulBox <- renderValueBox({
    result <- LC_status("urban-low", NDVIall_years_modeled, NDVIall_normals_modeled, most_recent_data)
    
    if (is.null(result)) {
      return(valueBox(
        "No recent urban-low data available", 
        subtitle = "No Data", 
        icon = icon("exclamation-circle"),
        color = "gray",  
        width = 11
      ))
    }
    
    # If data is available, display the result
    valueBox(
      "Urban-Low", 
      subtitle = paste("is", result$status, "from the norm"),
      icon = icon("house"),
      color = result$color,  
      width = 11
    )
  })
  
  output$uoBox <- renderValueBox({
    result <- LC_status("urban-open", NDVIall_years_modeled, NDVIall_normals_modeled, most_recent_data)
    
    if (is.null(result)) {
      return(valueBox(
        "No recent urban-open data available", 
        subtitle = "No Data", 
        icon = icon("exclamation-circle"),
        color = "gray",  
        width = 11
      ))
    }
    
    # If data is available, display the result
    valueBox(
      "Urban-Open", 
      subtitle = paste("is", result$status, "from the norm"),
      icon = icon("shop"),
      color = result$color,  
      width = 11
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
    req(NDVIall_years_modeled, NDVIall_normals_modeled, most_recent_data)
    
    # Calculate percentile
    percentile <- ndvi_percentile("crop", current_time_period, NDVIall_normals_modeled, most_recent_data)
    
    # Ensure it returns a valid value
    if (is.null(percentile) || is.na(percentile)) {
      return("Crop NDVI Percentile: Data Unavailable")
    } else {
      return(paste0("Crop NDVI Percentile: ", round(percentile, 1), "%"))
    }
  })

  output$percentile_for <- renderText({
    # Ensure necessary inputs exist
    req(NDVIall_years_modeled, NDVIall_normals_modeled, most_recent_data)
    
    # Calculate percentile
    percentile <- ndvi_percentile("forest", current_time_period, NDVIall_normals_modeled, most_recent_data)
    
    # Ensure it returns a valid value
    if (is.null(percentile) || is.na(percentile)) {
      return("Forest NDVI Percentile: Data Unavailable")
    } else {
      return(paste0("Forest NDVI Percentile: ", round(percentile, 1), "%"))
    }
  })
  
  output$percentile_grass <- renderText({
    # Ensure necessary inputs exist
    req(NDVIall_years_modeled, NDVIall_normals_modeled, most_recent_data)
    
    # Calculate percentile
    percentile <- ndvi_percentile("grassland", current_time_period, NDVIall_normals_modeled, most_recent_data)
    
    # Ensure it returns a valid value
    if (is.null(percentile) || is.na(percentile)) {
      return("Grassland NDVI Percentile: Data Unavailable")
    } else {
      return(paste0("Grassland NDVI Percentile: ", round(percentile, 1), "%"))
    }
  })
  
  output$percentile_uh <- renderText({
    # Ensure necessary inputs exist
    req(NDVIall_years_modeled, NDVIall_normals_modeled, most_recent_data)
    
    # Calculate percentile
    percentile <- ndvi_percentile("urban-high", current_time_period, NDVIall_normals_modeled, most_recent_data)
    
    # Ensure it returns a valid value
    if (is.null(percentile) || is.na(percentile)) {
      return("Urban-High NDVI Percentile: Data Unavailable")
    } else {
      return(paste0("Urban-High NDVI Percentile: ", round(percentile, 1), "%"))
    }
  })
  
  output$percentile_um <- renderText({
    # Ensure necessary inputs exist
    req(NDVIall_years_modeled, NDVIall_normals_modeled, most_recent_data)
    
    # Calculate percentile
    percentile <- ndvi_percentile("urban-medium", current_time_period, NDVIall_normals_modeled, most_recent_data)
    
    # Ensure it returns a valid value
    if (is.null(percentile) || is.na(percentile)) {
      return("Urban-Medium NDVI Percentile: Data Unavailable")
    } else {
      return(paste0("Urban-Medium NDVI Percentile: ", round(percentile, 1), "%"))
    }
  })
  
  output$percentile_ul <- renderText({
    # Ensure necessary inputs exist
    req(NDVIall_years_modeled, NDVIall_normals_modeled, most_recent_data)
    
    # Calculate percentile
    percentile <- ndvi_percentile("urban-low", current_time_period, NDVIall_normals_modeled, most_recent_data)
    
    # Ensure it returns a valid value
    if (is.null(percentile) || is.na(percentile)) {
      return("Urban-Low NDVI Percentile: Data Unavailable")
    } else {
      return(paste0("Urban-Low NDVI Percentile: ", round(percentile, 1), "%"))
    }
  })
  
  output$percentile_uo <- renderText({
    # Ensure necessary inputs exist
    req(NDVIall_years_modeled, NDVIall_normals_modeled, most_recent_data)
    
    # Calculate percentile
    percentile <- ndvi_percentile("urban-open", current_time_period, NDVIall_normals_modeled, most_recent_data)
    
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
    crop_plot <- density_plot("crop", "Crop", NDVIall_normals_modeled, NDVIall_years_modeled,most_recent_data)
    print(crop_plot)
  })
  
  output$forest_density_plot <- renderPlotly({
    forest_plot <- density_plot("forest", "Forest", NDVIall_normals_modeled, NDVIall_years_modeled, most_recent_data)
    print(forest_plot)
  })
  
  output$grassland_density_plot <- renderPlotly({
    grassland_plot <- density_plot("grassland", "Grassland", NDVIall_normals_modeled, NDVIall_years_modeled, most_recent_data)
    print(grassland_plot)
  })
  
  output$uh_density_plot <- renderPlotly({
    uh_plot <- density_plot("urban-high", "Urban-High", NDVIall_normals_modeled, NDVIall_years_modeled, most_recent_data)
    print(uh_plot)
  })
  
  output$um_density_plot <- renderPlotly({
    um_plot <- density_plot("urban-medium", "Urban-Medium", NDVIall_normals_modeled, NDVIall_years_modeled,most_recent_data)
    print(um_plot)  
  })
  
  output$ul_density_plot <- renderPlotly({
    ul_plot <- density_plot("urban-low", "Urban-Low", NDVIall_normals_modeled, NDVIall_years_modeled, most_recent_data)
    print(ul_plot)  
  })
  
  output$uo_density_plot <- renderPlotly({
    uo_plot <- density_plot("urban-open", "Urban-Open", NDVIall_normals_modeled, NDVIall_years_modeled, most_recent_data)
    print(uo_plot)
  })
  ####################################################################################################################
  #Change Stats
  output$crop_daily <- renderText({
    daily_diff <- daily_change("crop", date_needed, NDVIall_years_modeled)
    weekly_diff <- weekly_change("crop", date_needed, NDVIall_years_modeled)
    monthly_diff <-monthly_change("crop", date_needed, NDVIall_years_modeled)
    yearly_diff <-yearly_change("crop", date_needed, NDVIall_years_modeled)
    
    # Combine both outputs into a single string
    paste(daily_diff, " | ", weekly_diff," | ", monthly_diff," | ",yearly_diff)
    
  })
  
  output$for_daily<-renderText({
    daily_diff <- daily_change("forest", date_needed, NDVIall_years_modeled)
    weekly_diff <- weekly_change("forest", date_needed, NDVIall_years_modeled)
    monthly_diff <-monthly_change("forest", date_needed, NDVIall_years_modeled)
    yearly_diff <-yearly_change("forest", date_needed, NDVIall_years_modeled)
    
    # Combine both outputs into a single string
    paste(daily_diff, " | ", weekly_diff," | ", monthly_diff," | ",yearly_diff)
  })
  output$grass_daily<-renderText({
    daily_diff <- daily_change("grassland", date_needed, NDVIall_years_modeled)
    weekly_diff <- weekly_change("grassland", date_needed, NDVIall_years_modeled)
    monthly_diff <-monthly_change("grassland", date_needed, NDVIall_years_modeled)
    yearly_diff <-yearly_change("grassland", date_needed, NDVIall_years_modeled)
    
    # Combine both outputs into a single string
    paste(daily_diff, " | ", weekly_diff," | ", monthly_diff," | ",yearly_diff)
  })
  output$uh_daily<-renderText({
    daily_diff <- daily_change("urban-high", date_needed, NDVIall_years_modeled)
    weekly_diff <- weekly_change("urban-high", date_needed, NDVIall_years_modeled)
    monthly_diff <-monthly_change("urban-high", date_needed, NDVIall_years_modeled)
    yearly_diff <-yearly_change("urban-high", date_needed, NDVIall_years_modeled)
    
    # Combine both outputs into a single string
    paste(daily_diff, " | ", weekly_diff," | ", monthly_diff," | ",yearly_diff)
  })
  output$um_daily<-renderText({
    daily_diff <- daily_change("urban-medium", date_needed, NDVIall_years_modeled)
    weekly_diff <- weekly_change("urban-medium", date_needed, NDVIall_years_modeled)
    monthly_diff <-monthly_change("urban-medium", date_needed, NDVIall_years_modeled)
    yearly_diff <-yearly_change("urban-medium", date_needed, NDVIall_years_modeled)
    
    # Combine both outputs into a single string
    paste(daily_diff, " | ", weekly_diff," | ", monthly_diff," | ",yearly_diff)
  })
  output$ul_daily<-renderText({
    daily_diff <- daily_change("urban-low", date_needed, NDVIall_years_modeled)
    weekly_diff <- weekly_change("urban-low", date_needed, NDVIall_years_modeled)
    monthly_diff <-monthly_change("urban-low", date_needed, NDVIall_years_modeled)
    yearly_diff <-yearly_change("urban-low", date_needed, NDVIall_years_modeled)
    
    # Combine both outputs into a single string
    paste(daily_diff, " | ", weekly_diff," | ", monthly_diff," | ",yearly_diff)
  })
  output$uo_daily<-renderText({
    daily_diff <- daily_change("urban-open", date_needed, NDVIall_years_modeled)
    weekly_diff <- weekly_change("urban-open", date_needed, NDVIall_years_modeled)
    monthly_diff <-monthly_change("urban-open", date_needed, NDVIall_years_modeled)
    yearly_diff <-yearly_change("urban-open", date_needed, NDVIall_years_modeled)
    
    # Combine both outputs into a single string
    paste(daily_diff, " | ", weekly_diff," | ", monthly_diff," | ",yearly_diff)
  })
  ####################################################################################################################
  output$ndvi_heatmap_crop <- renderPlot({
    req(input$selected_years)
  
    plot_ndvi_heatmap(NDVIall_years_modeled, input$selected_years, "crop", "Crop")
  })

  #####################
  output$ndvi_heatmap_forest <- renderPlot({
    req(input$selected_years)  # Ensure at least one year is selected

   plot_ndvi_heatmap(NDVIall_years_modeled, input$selected_years, "forest", "Forest")  # Pass the filtered data
  })
  #####################
  output$ndvi_heatmap_grass <- renderPlot({
    req(input$selected_years)  # Ensure at least one year is selected
  
    plot_ndvi_heatmap(NDVIall_years_modeled, input$selected_years, "grassland", "Grassland")  # Pass the filtered data
  })
  #####################
  output$ndvi_heatmap_uh <- renderPlot({
    req(input$selected_years)  # Ensure at least one year is selected

   
    plot_ndvi_heatmap(NDVIall_years_modeled, input$selected_years, "urban-high","Urban-High")  # Pass the filtered data
  })
  #####################
  output$ndvi_heatmap_um <- renderPlot({
    req(input$selected_years)  # Ensure at least one year is selected

   plot_ndvi_heatmap(NDVIall_years_modeled, input$selected_years, "urban-medium", "Urban-Medium")  # Pass the filtered data
  })
  #####################
  output$ndvi_heatmap_ul <- renderPlot({
    req(input$selected_years)  # Ensure at least one year is selected

    plot_ndvi_heatmap(NDVIall_years_modeled, input$selected_years,"urban-low", "Urban-Low")  # Pass the filtered data
  })
  #####################
  output$ndvi_heatmap_uo <- renderPlot({
    req(input$selected_years)  # Ensure at least one year is selected
  
    plot_ndvi_heatmap(NDVIall_years_modeled, input$selected_years,"urban-open", "Urban-Open")  # Pass the filtered data
  })
  ####################################################################################################################
  # Observe when Heatmap tab is selected
# observe({
#   if (input$tabs == "Heatmap") {  # Replace "Heatmap" with the actual tab ID
#     shinyalert(
#       title = "Loading NDVI Heatmaps...",
#       text = "Please wait while the data is processed.",
#       type = "info",
#       showConfirmButton = FALSE,
#       closeOnClickOutside = FALSE
#     )
#   }
# })
# 
# # Reactive tracker to count when all plots are ready
# plots_ready <- reactiveVal(0)
# 
# plot_complete <- function() {
#   plots_ready(plots_ready() + 1)
#   if (plots_ready() == 7) {  # Total number of NDVI plots
#     closeAlert()  # Close the popup when all plots finish loading
#   }
# }
# 
# # Render NDVI Heatmaps
# output$ndvi_heatmap_crop <- renderPlot({
#   req(input$selected_years)
#   filtered_data <- heatmap_data %>% filter(year %in% input$selected_years)
#   plot_ndvi_heatmap(filtered_data, input$selected_years, "crop", "Crop")
#   plot_complete()
# })
# 
# output$ndvi_heatmap_forest <- renderPlot({
#   req(input$selected_years)
#   filtered_data <- heatmap_data %>% filter(year %in% input$selected_years)
#   plot_ndvi_heatmap(filtered_data, input$selected_years, "forest", "Forest")
#   plot_complete()
# })
# 
# output$ndvi_heatmap_grass <- renderPlot({
#   req(input$selected_years)
#   filtered_data <- heatmap_data %>% filter(year %in% input$selected_years)
#   plot_ndvi_heatmap(filtered_data, input$selected_years, "grassland", "Grassland")
#   plot_complete()
# })
# 
# output$ndvi_heatmap_uh <- renderPlot({
#   req(input$selected_years)
#   filtered_data <- heatmap_data %>% filter(year %in% input$selected_years)
#   plot_ndvi_heatmap(filtered_data, input$selected_years, "urban-high", "Urban-High")
#   plot_complete()
# })
# 
# output$ndvi_heatmap_um <- renderPlot({
#   req(input$selected_years)
#   filtered_data <- heatmap_data %>% filter(year %in% input$selected_years)
#   plot_ndvi_heatmap(filtered_data, input$selected_years, "urban-medium", "Urban-Medium")
#   plot_complete()
# })
# 
# output$ndvi_heatmap_ul <- renderPlot({
#   req(input$selected_years)
#   filtered_data <- heatmap_data %>% filter(year %in% input$selected_years)
#   plot_ndvi_heatmap(filtered_data, input$selected_years, "urban-low", "Urban-Low")
#   plot_complete()
# })
# 
# output$ndvi_heatmap_uo <- renderPlot({
#   req(input$selected_years)
#   filtered_data <- heatmap_data %>% filter(year %in% input$selected_years)
#   plot_ndvi_heatmap(filtered_data, input$selected_years, "urban-open", "Urban-Open")
#   plot_complete()
# })

  ####################################################################################################################
  #Popup messages
  shinyalert(
    title = "Welcome to the Urban Drought Dashboard!",
    text = "<b>A near real-time portal offering a comprehensive view of current conditions across seven land cover types, with additional tabs for deeper analysis and research.<br><br>
                <b>LC Types</b> = Landcover types (crop, forest, grass/grassland, urban-high, urban-medium, urban-low, urban-open)<br>
                <b>NDVI</b> = Normalized Difference Vegetation Index (used as a measure of green)<br><br>
                If you need to view this information again, check the <b>About Tab</b> under <b>Preliminary Information</b>.</h6>",
    type = "info",
    html = TRUE,  # This is the key setting!
    showConfirmButton = TRUE,
    confirmButtonText = "Close"
  )

  ####################################################################################################################
}
