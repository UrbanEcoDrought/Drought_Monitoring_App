##################### 
# Get data saved from Earth Engine and pull it together for analysis
##################### 
# if(!"NDVI_Automation_Workflow" %in% dir()) setwd("../..")

path.google <- "/Users/crollinson/Google Drive/"
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

# Clunky code, but should pull the latest file.
# NOTE: only "forest-wet" is exported (it gets recoded to "forest" below). There is
# no "Landsat*_forest_" file, so do NOT list a bare "forest" here -- doing so made
# dir() return character(0), fileL8/fileL9 became character(0), and the file.exists()
# check below threw "argument is of length zero" and killed the whole run.
lcnames <- c("forest-wet", "crop", "grassland", "urban-high", "urban-medium", "urban-low", "urban-open")

# Helper: grab the most recent downloaded file matching a prefix, or NA if none yet.
latestFile <- function(prefix){
  hits <- dir(file.path(path.google, NDVIsave), prefix)
  if(length(hits)==0) return(NA_character_)
  hits[length(hits)]  # filenames carry _YYYY_MM_DD_..., so last alphabetically = newest
}

ndviAll <- data.frame()
for(LCTYPE in lcnames){
  fileL8 <- latestFile(paste0("Landsat8_", LCTYPE, "_"))
  fileL9 <- latestFile(paste0("Landsat9_", LCTYPE, "_"))

  # Just open whatever the last file we downloaded is. If a mission has no file yet
  # (e.g. Drive hasn't synced it), skip it rather than crashing -- the new-data
  # check further down decides whether there's actually anything new to process.
  landsatAll <- data.frame()
  if(!is.na(fileL8)){
    if(!file.exists(file.path(pathShare, fileL8))) file.copy(from=file.path(path.google, NDVIsave, fileL8), to=file.path(pathShare, fileL8), overwrite=T, copy.mode=T)
    landsat8 <- read.csv(file.path(path.google, NDVIsave, fileL8))
    landsat8$mission <- "landsat 8"
    landsatAll <- rbind(landsatAll, landsat8)
  }
  if(!is.na(fileL9)){
    if(!file.exists(file.path(pathShare, fileL9))) file.copy(from=file.path(path.google, NDVIsave, fileL9), to=file.path(pathShare, fileL9), overwrite=T, copy.mode=T)
    landsat9 <- read.csv(file.path(path.google, NDVIsave, fileL9))
    landsat9$mission <- "landsat 9"
    landsatAll <- rbind(landsatAll, landsat9)
  }

  if(nrow(landsatAll)==0){
    message(paste("No NDVI files found yet for", LCTYPE, "- skipping"))
    next
  }

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
summary(ndviLatest[!is.na(ndviLatest$NDVI),])

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