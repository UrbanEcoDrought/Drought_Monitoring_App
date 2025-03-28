####################################################################################################################
#Setup
####################################################################################################################
library(dplyr);library(ggplot2);library(plotly);library(readr);library(lubridate)

paletteLC <- c("crop"="#ab6c28", "forest"="#68ab5f", "grassland"="#dfdfc2", "urban-high"="#ab0000", "urban-medium"="#eb0000", "urban-low"="#d99282", "urban-open"="#dec5c5")

dat.path <- ("~/Documents/GitHub/Drought_Monitoring_App/Urban Drought App/data/year_csvs")

NDVIall_normals_modeled <-read_csv("data/NDVIall_normals_modeled.csv")
NDVIall_years_modeled<-read_csv("data/NDVIall_years_modeled.csv")

NDVIall_years_modeled$year <- as.numeric(NDVIall_years_modeled$year)
latest_year <- max(NDVIall_years_modeled$year, na.rm = TRUE)

####################################################################################################################
#For Cross Checking things (figures, df, etc.)
####################################################################################################################


latest_yday <- max(NDVIall_years_modeled$yday[NDVIall_years_modeled$year == latest_year], na.rm = TRUE)

#pulling any rows with matching date 
most_recent_data<- filter(NDVIall_years_modeled, year == latest_year & yday == latest_yday)

####################################################################################################################
#Percentiles 

  #TEST CASE (forest , 03/06/2025)
  YrMean_distr_yday <- filter(NDVIall_years_modeled, yday == latest_yday & type == "forest") #all YrMean for most current yday
  most_recent_subset <- filter(most_recent_data, type == "forest")
  current_NDVI <- most_recent_subset$YrMean[1]  # Extract current NDVI value
  
  # Compute the percentile of the current NDVI value
  ecdf_function <- ecdf(YrMean_distr_yday$YrMean)  # Create empirical cumulative distribution function
  current_percentile <- ecdf_function(current_NDVI) * 100  # Convert to percentage
  
  #By Hand
  testing<-filter(YrMean_distr_yday, YrMean <= current_NDVI) #10 values under or equal to the current NDVI out of a total of 25 values
  #By hand I got --->  percentile is 40 %, matches with function
  
  #-----------------------------------------------------------------------------------------------------------------#
  #TEST CASE (UrbanOpen , 03/06/2025)
  YrMean_distr_yday <- filter(NDVIall_years_modeled, yday == latest_yday & type == "urban-open") #all YrMean for most current yday
  most_recent_subset <- filter(most_recent_data, type == "urban-open")
  current_NDVI <- most_recent_subset$YrMean[1]  # Extract current NDVI value
  
  # Compute the percentile of the current NDVI value
  ecdf_function <- ecdf(YrMean_distr_yday$YrMean)  # Create empirical cumulative distribution function
  current_percentile <- ecdf_function(current_NDVI) * 100  # Convert to percentage
  
  #By Hand
  testing<-filter(YrMean_distr_yday, YrMean <= current_NDVI)#1 values under or equal to the current NDVI out of a total of 25 values
  #By hand I got --->  percentile is 4 %, matches with function
  
#####################################################################################################################
#Separating all_years file into separate files for each year (hopefully to speed up loading time)
  #for all years 
  
  years <- unique(NDVIall_years_modeled$year)
  
  for (yr in years) {
    subset_data <- filter(NDVIall_years_modeled, year == yr)
    write_csv(subset_data, file.path(dat.path, paste0("NDVI_data_", yr, ".csv")))
    print(paste0(yr, " is done"))
  }
  
  cat("CSV files saved in:", dat.path, "\n")
  
  #only for newest year 
  for (yr in years) {
    subset_data <- filter(NDVIall_years_modeled, year == latest_year)
    write_csv(subset_data, file.path(dat.path, paste0("NDVI_data_", yr, ".csv")))
  }
#-----------------------------------------------------------------------------------------------------------------#
#Testing with Features  
  #NDVI Review Graphs
  
  
  # List all CSV files in the directory
  all_files <- list.files(dat.path, pattern = "\\.csv$", full.names = TRUE)
    
    # Extract the years from file names (assuming format like "data_2020.csv")
    file_years <- as.numeric(gsub(".*_(\\d{4})\\.csv$", "\\1", all_files))
    
    # Filter files that match the years in `years`
    matching_files <- all_files[file_years %in% years]
    print(length(matching_files))  # Check number of matched files
    print(matching_files)
    
    combined_years <- lapply(matching_files, function(f) {
      df <- read_csv(f, col_types = cols(.default = "c"))  # Read all columns as character to avoid mismatches
      df$source_file <- basename(f)  # Add filename as a column (optional for debugging)
      return(df)
    })
    
    # Combine into one dataframe
    combined_years <- bind_rows(combined_years)
    
    combined_years <- combined_years %>%
      mutate(date = as.Date(date))  
    
      combined_years <- combined_years %>%
        mutate(YrMean = round(as.numeric(YrMean), 3))  # Round to 3 decimal places
  #-----------------------------------------------------------------------------------------------------------------#
    #Testing the Plots 
      
      #Original 
    system.time({
    ggplot(combined_years, aes(x = year , y = YrMean, color = type, fill=type)) +
      geom_point(size = 1) +
      geom_smooth(method="gam", formula=y~s(x, bs="cs", k=12*25)) +
      scale_color_manual(values = paletteLC) +
      scale_fill_manual(values = paletteLC) +
      labs(
        x = "Year",
        y = "NDVI Value",
        title = "NDVI Trends Over Time for All Land Cover Types"
      ) +
      scale_x_date(
        date_breaks = "6 months",
        date_labels = "%Y"
      ) +
      scale_y_continuous(
        breaks = seq(min(combined_years$YrMean, na.rm = TRUE), 
                     max(combined_years$YrMean, na.rm = TRUE), 
                     length.out = 6)  # Adjust the number of y-axis labels
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10)
      )
      })
  
  #-----------------------------------------------------------------------------------------------------------------#
    #Without geom_smooth at all
  
  system.time({
    ggplot(combined_years, aes(x = date , y = YrMean, color = type, fill=type)) +
      geom_point(size = 1) +
      scale_color_manual(values = paletteLC) +
      scale_fill_manual(values = paletteLC) +
      labs(
        x = "Date",
        y = "NDVI Value",
        title = "NDVI Trends Over Time for All Land Cover Types"
      ) +
      scale_x_date(
        date_breaks = "6 months",
        date_labels = "%b %Y"
      ) +
      scale_y_continuous(
        breaks = seq(min(combined_years$YrMean, na.rm = TRUE), 
                     max(combined_years$YrMean, na.rm = TRUE), 
                     length.out = 6)  # Adjust the number of y-axis labels
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10)
      )
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
#-----------------------------------------------------------------------------------------------------------------#
  #!2 month overview graph 
  twelve_month_graph <- function(start_year,end_year) {
    
    start_year <- as.numeric(start_year)
    end_year <- as.numeric(end_year)
    
    # Filter data for the selected range
    yearly_filtered_data <- NDVIall_years_modeled %>%
      filter(lubridate::year(date) >= start_year & lubridate::year(date) <= end_year)
    
    jan_1_dates <- unique(yearly_filtered_data$date[lubridate::month(yearly_filtered_data$date) == 1 & 
                                                      lubridate::day(yearly_filtered_data$date) == 1])
    
    # Generate the plot
    ggplot(yearly_filtered_data, aes(x = date, y = YrMean)) +
      geom_ribbon(aes(ymin=YrLwr, ymax=YrUpr, fill=type), alpha=0.2) +
      geom_line(aes(color=type)) +
      # geom_ribbon(method = "gam", formula = y ~ s(x, bs = "cs", k = 12)) +
      geom_vline(xintercept = jan_1_dates, linetype = "dashed") + 
      scale_color_manual(values = paletteLC) +
      scale_fill_manual(values = paletteLC) +
      labs(
        y = "NDVI Value",
        title = paste("NDVI Trends for Year", start_year)
      ) +
      scale_x_date(date_labels = "%b %Y")
  }
  #-----------------------------------------------------------------------------------------------------------------#
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
  #-----------------------------------------------------------------------------------------------------------------#
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
  #-----------------------------------------------------------------------------------------------------------------#
  #Testing Load Times
  
  #all data
    all_data_graph()  
 
  
 #yearly (12 months)
    #for slider, but will input random values for now 
    start_year <- input$yearRange[1]
    end_year <- input$yearRange[2]
    
  
   twelve_month_graph(start_year, end_year)
  
  monthly_graph(input$mstart_date)

   weekly_graph(input$wstart_date)
    
  
  
  
  