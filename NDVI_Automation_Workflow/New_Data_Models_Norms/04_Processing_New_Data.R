##################### 
# Get data saved from Earth Engine and pull it together for analysis
##################### 
if(!"NDVI_Automation_Workflow" %in% dir()) setwd("../..")

path.google <- "~/Google Drive/"
pathShare <- file.path(path.google, "Shared drives", "Urban Ecological Drought", "data", "UrbanEcoDrought_NDVI_LocalExtract-RAW")
NDVIsave <- ("My Drive/UrbanEcoDrought_NDVI_LocalExtract-RAW")
pathDat <- "NDVI_Automation_Workflow/data_all"


# Check if files are being detected
fNDVI <- dir(file.path(path.google, NDVIsave))
tail(fNDVI)  # Should list all files in that directory


ndviLatest <-read.csv(file.path(pathDat, "NDVIall_latest.csv"))
ndviLatest$date <- as.Date(ndviLatest$date)
summary(ndviLatest)


dateLastL8 <- max(ndviLatest$date[ndviLatest$mission=="landsat 8"])
dateLastL9 <- max(ndviLatest$date[ndviLatest$mission=="landsat 9"])

# date_needed <- max(ndviLatest$date)
# date_today <- as.Date(today())


day.labels <- data.frame(Date=seq.Date(as.Date("2023-01-01"), as.Date("2023-12-01"), by="month"))
day.labels$yday <- lubridate::yday(day.labels$Date)
day.labels$Text <- paste(lubridate::month(day.labels$Date, label=T), lubridate::day(day.labels$Date))
day.labels
summary(day.labels)

# Clunky code, but should pull the latest file
lcnames <- c("forest", "forest-wet", "crop", "grassland", "urban-high", "urban-medium", "urban-low", "urban-open")

ndviAll <- data.frame()
for(LCTYPE in lcnames){
  fileL8 <- dir(file.path(path.google, NDVIsave), paste0("Landsat8_", LCTYPE, "_"))[length(dir(file.path(path.google, NDVIsave), paste0("Landsat8_", LCTYPE, "_")))]
  fileL9 <- dir(file.path(path.google, NDVIsave), paste0("Landsat9_", LCTYPE, "_"))[length(dir(file.path(path.google, NDVIsave), paste0("Landsat9_", LCTYPE, "_")))]
  
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
(ndviAll[ndviAll$type=="forest",])
(ndviAll[ndviAll$type=="forest-wet",])

ndviAll$type <- car::recode(ndviAll$type, "'forest-wet'='forest'")

summary(ndviAll)
unique(ndviAll$type)
##################### 
#When running manually, pause here and wait for loop to finish 
##################### 

summary(ndviAll)
unique(ndviAll$mission)
unique(ndviAll$type)

ndviAll$date <- as.Date(ndviAll$date)
ndviAll$year <- lubridate::year(ndviAll$date)
ndviAll$yday <- lubridate::yday(ndviAll$date)
ndviAll$type <- factor(ndviAll$type, levels=rev(c("forest", "grassland", "crop", "urban-open", "urban-low", "urban-medium", "urban-high")))
head(ndviAll)
summary(ndviAll)
unique(ndviAll$type)

summary(ndviAll[ndviAll$type=="forest",])


ndviAll <-ndviAll[order(as.Date(ndviAll$date, format="%Y-%m-%d"), decreasing = F), ]
tail(ndviAll)
summary(ndviAll[ndviAll$type=="forest",])
summary(ndviAll[ndviAll$type=="urban-medium",])

summary(ndviLatest)
# Makes more sense to join old nvdi data and new ndvi data after new data is fit to spline since old data is already fit to spline
#steps: take new data and save as csv and run it through spline in next script, read in old data and then join 
#then run year specific adjustments

if(any(ndviAll$date[ndviAll$mission=="landsat 8"] > dateLastL8) | any(ndviAll$date[ndviAll$mission=="landsat 9"] > dateLastL9)) {
  new_NDVI_data <- ndviAll[(ndviAll$mission=="landsat 8" & ndviAll$date > dateLastL8) | 
                             (ndviAll$mission=="landsat 9" & ndviAll$date > dateLastL9),]
  
  ndviLatest <- rbind(ndviLatest, new_NDVI_data)
  
  ndviLatest$type <- car::recode(ndviLatest$type, "'forest-wet'='forest'")
  new_NDVI_data$type <- car::recode(new_NDVI_data$type, "'forest-wet'='forest'")
  
  
  write.csv(new_NDVI_data, file.path(pathDat, "new_NDVI_data.csv"), row.names = FALSE)
  write.csv(ndviLatest, file.path(pathShare, "NDVIall_latest.csv"), row.names = FALSE)
  write.csv(ndviLatest, file.path(pathDat, "NDVIall_latest.csv"), row.names = FALSE)
  
} else {
  message("No New Data")
  stop("No new NDVI data found. Stopping workflow.")
}

summary(ndviLatest[ndviLatest$year==max(ndviLatest$year),])
summary(ndviLatest[ndviLatest$type=="forest",])
##################### 