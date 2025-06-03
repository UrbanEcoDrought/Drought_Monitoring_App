####################################################################################################################
#Purpose of New_Data_Models_Norms Workflow: Build off of existing scripts to form a workflow that will be run weekly
#                                           for data updates & year specific splines.
#Purpose of Script: Pull new landsat data & join it to existing csv. Also to readjust year specific spline
# Original scripts written by Christy Rollinson and Juliana Harr, workflow put together by Jocelyn Garcia
####################################################################################################################

#Same code as the pulling all data script, except it doesn't include Landsat 5 or 7 

# Tried only using reticulate - didnt work 
#ee <- import("ee")
# Authenticate using the Earth Engine API (if not authenticated already)
#ee$Authenticate()
# Initialize the Earth Engine API
#ee$Initialize(project = "urbanecodrought")
#assetHome <- "projects/urbanecodrought/assets/UHI-analysis"
if(!"NDVI_Automation_Workflow" %in% dir()) setwd("../..")

library(rgee); library(raster); library(terra); library(dplyr); library(tidyverse)
# ee_check() # For some reason, it's important to run this before initializing right now
# user.ee <- "jgarcia@mortonarb.org"
user.ee <- "crollinson@mortonarb.org"

rgee::ee_Initialize(user =user.ee, drive=T, project = "urbanecodrought")
path.google.CR <- "~/Google Drive/My Drive/UrbanEcoDrought/"
path.google.share <- "~/Google Drive/Shared drives/Urban Ecological Drought/"
assetHome <- ee_get_assethome()
NDVIsave <- ("UrbanEcoDrought_NDVI_LocalExtract-RAW")
# pathDat <- "../data_all"
pathDat <- "NDVI_Automation_Workflow/data_all"


##################### 
# 0. Read in helper functions ----
##################### 
# source("../Baseline_Data_Models_Norms/00_EarthEngine_HelperFunctions copy.R")
source("NDVI_Automation_Workflow/Baseline_Data_Models_Norms/00_EarthEngine_HelperFunctions copy.R")

####################################################################################################################
#Reading in previous data to get latest date, will need later
if(!file.exists(file.path(pathDat, "NDVIall_latest.csv"))) file.copy(from=file.path(pathDat, "NDVIall_baseline.csv"), to=file.path(pathDat, "NDVIall_latest.csv"), copy.date = T)


#After first run, change the file path to the allNDVI_data.csv, needed this file path for first run because its the baseline
ndviLatest <-read_csv(file.path(pathDat, "NDVIall_latest.csv"))%>%
  mutate(date = as.Date(date, format="%Y-%m-%d"))

dateLastL8 <- max(ndviLatest$date[ndviLatest$mission=="landsat 8"])
dateLastL9 <- max(ndviLatest$date[ndviLatest$mission=="landsat 9"])
date_today <- as.Date(today())

filesCheck <- list(landsat8 = vector(), landsat9 = vector())
####################################################################################################################


##################### 
# Color Palette etc. ----
##################### 
# Setting the center point for the Arb because I think we'll have more variation
Map$setCenter(-88.04526, 41.81513, 11);

# Adding Landcover Classes!
nlcdPalette = c(
  '#5475A8', # Open Water (11)
  # '#d1def8', # Ice/Snow (12)
  '#dec5c5', # Developed, Open Space (21)
  '#d99282', # Developed, Low Intensity (22)
  '#eb0000', # Developed, Medium Intensity (23)
  '#ab0000', # Developed High Intensity (24)
  '#b3ac9f', # Barren Land (31)
  '#68ab5f', # Deciduous Forest (41)
  '#1c5f2c', # Evergreen Forest (42)
  '#b5c58f', # Mixed Forest (43)
  # '#af963c', # Dwarf Shrub/Scrub (51); Alaska Only
  '#ccb879', # Shrub/Scrub (52)
  '#dfdfc2', # Grassland/Herbaceous (71)
  # '#d1d182', # Sedge/herbaceous (72); Alaska Only
  # '#a3cc51', # lichens (73); Alaska Only
  # '#82ba9e', # Moss (74); Alaska Only
  '#dcd939', # Pasture/Hay (81)
  '#ab6c28', # Cultivated Crops (82)
  '#b8d9eb', # Woody Wetlands (90)
  '#6c9fb8' # Emergent Herbaceous Wetlands (95)
);

nlcdvis = list(
  min= 0,
  max= 95,
  palette= nlcdPalette
);

##################### 
# Read in base layers ----
##################### 
#Chicago = ee$FeatureCollection("projects/ee-jgarcia/assets/SevenCntyChiReg") 
#ee_print(Chicago)
# Access a feature collection
# Chicago <- ee$FeatureCollection("projects/ee-jgarcia/assets/SevenCntyChiReg")
Chicago = ee$FeatureCollection("projects/breidyee/assets/SevenCntyChiReg") 
print(Chicago$getInfo())

chiBounds <- Chicago$geometry()$bounds()
chiBBox <- ee$Geometry$BBox(-88.70738, 41.20155, -87.52453, 42.49575)

##################### 
# Read in Landcover Masks ----
##################### 

# Landcover names and mask ----
lcnames <- c("forest", "crop", "grassland", "urban-high", "urban-medium", "urban-low", "urban-open")


######################For updating data, START HERE##################### 

forMask <- ee$Image(file.path(assetHome, 'NLCD-Chicago_2000-2025_Forest'))
grassMask <- ee$Image(file.path(assetHome, 'NLCD-Chicago_2000-2025_Grass'))
cropMask <- ee$Image(file.path(assetHome, 'NLCD-Chicago_2000-2025_Crop'))
urbOMask <- ee$Image(file.path(assetHome, 'NLCD-Chicago_2000-2025_Urban-Open'))
urbLMask <-ee$Image(file.path(assetHome, 'NLCD-Chicago_2000-2025_Urban-Low'))
urbMMask <- ee$Image(file.path(assetHome, 'NLCD-Chicago_2000-2025_Urban-Medium'))
urbHMask <- ee$Image(file.path(assetHome, 'NLCD-Chicago_2000-2025_Urban-High'))

##################### 
# Read in & Format Landsat 8 ----
##################### 
# "LANDSAT/LC08/C02/T1_RT"
# Load MODIS NDVI data; attach month & year
# https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LC08_C02_T1_L2
# Filtering by date so we can only pull a bit of data if we need it
landsat8 <- ee$ImageCollection("LANDSAT/LC08/C02/T1_L2")$filterBounds(Chicago)$filterDate(as.character(dateLastL8), as.character(date_today))$map(function(image){
  return(image$clip(Chicago))
})$map(function(img){
  d= ee$Date(img$get('system:time_start'));
  dy= d$get('day');    
  m= d$get('month');
  y= d$get('year');
  
  # # Add masks 
  img <- applyLandsatBitMask(img)
  
  # #scale correction; doing here & separating form NDVI so it gets saved on the image
  lAdj = img$select(c('SR_B1', 'SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B6', 'SR_B7'))$multiply(0.0000275)$add(-0.2);
  lst_k = img$select('ST_B10')$multiply(0.00341802)$add(149);
  
  # img3 = img2$addBands(srcImg=lAdj, overwrite=T)$addBands(srcImg=lst_k, overwrite=T)$set('date',d, 'day',dy, 'month',m, 'year',y)
  return(img$addBands(srcImg=lAdj, overwrite=T)$addBands(srcImg=lst_k, overwrite=T)$set('date',d, 'day',dy, 'month',m, 'year',y))
})$select(c('SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B6', 'SR_B7', 'ST_B10'),c('blue', 'green', 'red', 'nir', 'swir1', 'swir2', 'LST_K'))$map(addNDVI)
# Map$addLayer(landsat8$first()$select('NDVI'), ndviVis, "NDVI - First")
# ee_print(landsat8)
# Map$addLayer(landsat8$first()$select('NDVI'))

# Need to check to see if the latest image is one we already have (if we don't do -1, it bonks because there's nothign enw)
# imlist = landsat8$toList(landsat8$size())
# imlist$getInfo()

# Note: needed to specify the ee_utils_pyfunc since it's not an image collection
# unique_dates <- landsat8$map(function(img) {
#   img$get(date()$format("YYYY-MM-dd")
#           # ee$Date(img$get('system:time_start'))
# })$distinct()
# # dates_list$getInfo()
# "LANDSAT/LC08/C02/T1_L2"
# landsat8 <- ee$ImageCollection("LANDSAT/LC08/C02/T1_L2")$
#   filterDate("2020-01-01", "2020-12-31")$
#   filterBounds(ee$Geometry$Point(c(-122.262, 37.8719))) # Example location
# ee_print(landsat8)

get_dates <- function(image) {
  date <- ee$Date(image$get("system:time_start"))$format("YYYY-MM-dd")
  ee$Feature(NULL, list(date = date))
}

l8dates <- landsat8$map(get_dates)
l8dates_list <- l8dates$getInfo()$features
l8dates_vector <- sapply(l8dates_list, function(x) x$properties$date)

if(max(l8dates_vector)>dateLastL8){
  # Mask NDVI by Landcover & condense to regional means
  for(LCTYPE in lcnames){
    # print(LCTYPE)
    extractByLC(imcol=landsat8, landcover=LCTYPE, outfolder=NDVIsave, fileNamePrefix=paste0("Landsat8_", LCTYPE))
  filesCheck$landsat8 <- c(filesCheck$landsat8, LCTYPE)
  }
  
} else {
  print("No New Landsat 8 Data")
  filesCheck$landsat8 <- "none"
}

##################### 

##################### 
# Read in & Format Landsat 9 ----
##################### 
# "LANDSAT/LC09/C02/T1_L2"
# Load MODIS NDVI data; attach month & year
# https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LC09_C02_T1_L2
landsat9 <- ee$ImageCollection("LANDSAT/LC09/C02/T1_L2")$filterBounds(Chicago)$filterDate(as.character(dateLastL9), as.character(date_today))$map(function(image){
  return(image$clip(Chicago))
})$map(function(img){
  d= ee$Date(img$get('system:time_start'));
  dy= d$get('day');    
  m= d$get('month');
  y= d$get('year');
  
  # # Add masks 
  img <- applyLandsatBitMask(img)
  
  # #scale correction; doing here & separating form NDVI so it gets saved on the image
  lAdj = img$select(c('SR_B1', 'SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B6', 'SR_B7'))$multiply(0.0000275)$add(-0.2);
  lst_k = img$select('ST_B10')$multiply(0.00341802)$add(149);
  
  # img3 = img2$addBands(srcImg=lAdj, overwrite=T)$addBands(srcImg=lst_k, overwrite=T)$set('date',d, 'day',dy, 'month',m, 'year',y)
  return(img$addBands(srcImg=lAdj, overwrite=T)$addBands(srcImg=lst_k, overwrite=T)$set('date',d, 'day',dy, 'month',m, 'year',y))
})$select(c('SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B6', 'SR_B7', 'ST_B10'),c('blue', 'green', 'red', 'nir', 'swir1', 'swir2', 'LST_K'))$map(addNDVI)
# ee_print(landsat9)


l9dates <- landsat9$map(get_dates)
l9dates_list <- l9dates$getInfo()$features
l9dates_vector <- sapply(l9dates_list, function(x) x$properties$date)

if(max(l9dates_vector)>dateLastL9){
  # Mask NDVI by Landcover & condense to regional means
  for(LCTYPE in lcnames){
    # print(LCTYPE)
    extractByLC(imcol=landsat9, landcover=LCTYPE, outfolder=NDVIsave, fileNamePrefix=paste0("Landsat9_", LCTYPE))
  filesCheck$landsat9 <- c(filesCheck$landsat9, LCTYPE)
  }
  
  
  
} else {
  print("No New Landsat 9 Data")
  filesCheck$landsat9 <- "none"
}



