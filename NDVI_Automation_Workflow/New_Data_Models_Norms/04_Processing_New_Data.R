##################### 
#End of Christy's second script 
##################### 
path.google <- "~/Google Drive/"
pathShare <- file.path(path.google, "Shared drives", "Urban Ecological Drought", "data", "UrbanEcoDrought_NDVI_LocalExtract-RAW")
NDVIsave <- ("My Drive/UrbanEcoDrought_NDVI_LocalExtract-RAW")
pathDat <- "../data_all"

# Check if files are being detected
fNDVI <- dir(file.path(path.google, NDVIsave))
print(fNDVI)  # Should list all files in that directory


ndviLatest <-read.csv(file.path(pathDat, "NDVIall_latest.csv"))
ndviLatest$date <- as.Date(ndviLatest$date)
summary(ndviLatest)

# date_needed <- max(ndviLatest$date)
# date_today <- as.Date(today())


day.labels <- data.frame(Date=seq.Date(as.Date("2023-01-01"), as.Date("2023-12-01"), by="month"))
day.labels$yday <- lubridate::yday(day.labels$Date)
day.labels$Text <- paste(lubridate::month(day.labels$Date, label=T), lubridate::day(day.labels$Date))
day.labels
summary(day.labels)

# Clunky code, but should pull the latest file
lcnames <- c("forest", "crop", "grassland", "urban-high", "urban-medium", "urban-low", "urban-open")

ndviAll <- data.frame()
for(LCTYPE in lcnames){
  fileL8 <- dir(file.path(path.google, NDVIsave), paste0("Landsat8_", LCTYPE))[length(dir(file.path(path.google, NDVIsave), paste0("Landsat8_", LCTYPE)))]
  fileL9 <- dir(file.path(path.google, NDVIsave), paste0("Landsat9_", LCTYPE))[length(dir(file.path(path.google, NDVIsave), paste0("Landsat9_", LCTYPE)))]
  
  if(!file.exists(file.path(pathShare, fileL8))) file.copy(from=file.path(path.google, NDVIsave, fileL8), to=file.path(pathShare, fileL8), overwrite=T, copy.mode=T)
  if(!file.exists(file.path(pathShare, fileL9))) file.copy(from=file.path(path.google, NDVIsave, fileL9), to=file.path(pathShare, fileL9), overwrite=T, copy.mode=T)
  
  landsat8 <- read.csv(file.path(path.google, NDVIsave, fileL8))
  landsat9 <- read.csv(file.path(path.google, NDVIsave, fileL9))
  
  landsat8$mission <- "landsat 8"
  landsat9$mission <- "landsat 9"
  
  landsatAll <- rbind(landsat8, landsat9)
  # landsatAll <- rbind(landsat8, landsat9)
  landsatAll$type <- LCTYPE
  
  ndviAll <- rbind(ndviAll, landsatAll)
}

summary(ndviAll)
head(ndviAll)

##################### 
#When running manually, pause here and wait for loop to finish 
##################### 

summary(ndviAll)
unique(ndviAll$mission)

ndviAll$date <- as.Date(ndviAll$date)
ndviAll$year <- lubridate::year(ndviAll$date)
ndviAll$yday <- lubridate::yday(ndviAll$date)
ndviAll$type <- factor(ndviAll$type, levels=rev(c("forest", "grassland", "crop", "urban-open", "urban-low", "urban-medium", "urban-high")))
head(ndviAll)
summary(ndviAll)
unique(ndviAll$type)

ndviAll <-ndviAll[order(as.Date(ndviAll$date, format="%Y-%m-%d"), decreasing = F), ]
tail(ndviAll)

summary(ndviLatest)
# Makes more sense to join old nvdi data and new ndvi data after new data is fit to spline since old data is already fit to spline
#steps: take new data and save as csv and run it through spline in next script, read in old data and then join 
#then run year specific adjustments

if(any(ndviAll$date > max(ndviLatest$date))) {
  new_NDVI_data <- ndviAll[ndviAll$date > max(ndviLatest$date),]
  
  ndviLatest <- rbind(ndviLatest, new_NDVI_data)
  write.csv(new_NDVI_data, file.path(pathDat, "new_NDVI_data.csv"), row.names = FALSE)
  write.csv(ndviLatest, file.path(pathShare, "NDVIall_latest.csv"), row.names = FALSE)
  write.csv(ndviLatest, file.path(pathDat, "NDVIall_latest.csv"), row.names = FALSE)
  
} else {
  message("No New Data")
  stop("No new NDVI data found. Stopping workflow.")
}

summary(ndviLatest[ndviLatest$year==max(ndviLatest$year),])

##################### 