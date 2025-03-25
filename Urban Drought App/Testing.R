####################################################################################################################
#For Cross Checking things (figures, df, etc.)
####################################################################################################################

NDVIall_normals_modeled <-read_csv("data/NDVIall_normals_modeled.csv")
NDVIall_years_modeled<-read_csv("data/NDVIall_years_modeled.csv")

NDVIall_years_modeled$year <- as.numeric(NDVIall_years_modeled$year)
latest_year <- max(NDVIall_years_modeled$year, na.rm = TRUE)

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
  
  ####################################################################################################################