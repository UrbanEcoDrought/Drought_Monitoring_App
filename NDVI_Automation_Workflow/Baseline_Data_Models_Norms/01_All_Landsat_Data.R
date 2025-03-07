####################################################################################################################
#Purpose of Baseline_Data_Models_Norms Workflow: Reorganize and add to existing scripts to form a workflow that will
#                                                be run once to create models & norms based on all data.
#Purpose of Script: Pull all landsat data & store as CSV in shared google drive 
# Original scripts written by Christy Rollinson and Juliana Harr, workflow put together by Jocelyn Garcia
####################################################################################################################
library(rgee); library(raster); library(terra)
# user.ee <- "jgarcia@mortonarb.org"
user.ee <- "crollinson@mortonarb.org"

ee_check() # For some reason, it's important to run this before initializing right now
rgee::ee_Initialize(user = user.ee, drive=T, project = "urbanecodrought")
path.google.CR <- "~/Google Drive/My Drive/UrbanEcoDrought/"
path.google.share <- "~/Google Drive/Shared drives/Urban Ecological Drought/"
assetHome <- ee_get_assethome()
NDVIsave <- ("My Drive/UrbanEcoDrought_NDVI_LocalExtract")

##################### 
# 0. Read in helper functions ----
##################### 
source("00_EarthEngine_HelperFunctions copy.R")
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
Chicago = ee$FeatureCollection("projects/ee-jgarcia/assets/SevenCntyChiReg") 
ee_print(Chicago)

chiBounds <- Chicago$geometry()$bounds()
chiBBox <- ee$Geometry$BBox(-88.70738, 41.20155, -87.52453, 42.49575)


####################################################################################################################
# Setting the center point for the Arb because I think we'll have more variation
Map$setCenter(-88.04526, 41.81513, 11);

ndviVis = list(
  min= 0.0,
  max= 1,
  palette= c(
    '#FFFFFF', '#CE7E45', '#DF923D', '#F1B555', '#FCD163', '#99B718', '#74A901',
    '#66A000', '#529400', '#3E8601', '#207401', '#056201', '#004C00', '#023B01',
    '#012E01', '#011D01', '#011301'
  )
)
##################### 
# Read in Landcover Masks ----
##################### 

# Landcover names and mask ----
lcnames <- c("forest", "crop", "grassland", "urban-high", "urban-medium", "urban-low", "urban-open")

forMask <- ee$Image('users/jgarcia/NLCD-Chicago_2000-2024_Forest')

grassMask <- ee$Image('users/jgarcia/NLCD-Chicago_2000-2024_Grass')

cropMask <- ee$Image('users/jgarcia/NLCD-Chicago_2000-2024_Crop')

urbOMask <- ee$Image('users/jgarcia/NLCD-Chicago_2000-2024_Urban-Open')

urbLMask <- ee$Image('users/jgarcia/NLCD-Chicago_2000-2024_Urban-Low')

urbMMask <- ee$Image('users/jgarcia/NLCD-Chicago_2000-2024_Urban-Medium')

urbHMask <- ee$Image('users/jgarcia/NLCD-Chicago_2000-2024_Urban-High')

##################### 
# Read in & Format Landsat 8 ----
##################### 
# "LANDSAT/LC08/C02/T1_RT"
# Load MODIS NDVI data; attach month & year
# https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LC08_C02_T1_L2
landsat8 <- ee$ImageCollection("LANDSAT/LC08/C02/T1_L2")$filterBounds(Chicago)$map(function(image){
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

l8Mosaic = mosaicByDate(landsat8, 7)$select(c('blue_median', 'green_median', 'red_median', 'nir_median', 'swir1_median', 'swir2_median', 'LST_K_median', "NDVI_median"),c('blue', 'green', 'red', 'nir', 'swir1', 'swir2', 'LST_K', "NDVI"))$sort("date")
# ee_print(l8Mosaic, "landsat8-Mosaic")
# Map$addLayer(l8Mosaic$first()$select('NDVI'), ndviVis, "NDVI - First")

# Mask NDVI by Landcover & condense to regional means
for(LCTYPE in lcnames){
  # print(LCTYPE)
  extractByLC(imcol=l8Mosaic, landcover=LCTYPE, outfolder=NDVIsave, fileNamePrefix=paste0("Landsat8_", LCTYPE))
}

##################### 

##################### 
# Read in & Format Landsat 9 ----
##################### 
# "LANDSAT/LC09/C02/T1_L2"
# Load MODIS NDVI data; attach month & year
# https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LC09_C02_T1_L2
landsat9 <- ee$ImageCollection("LANDSAT/LC09/C02/T1_L2")$filterBounds(Chicago)$map(function(image){
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

l9Mosaic = mosaicByDate(landsat9, 7)$select(c('blue_median', 'green_median', 'red_median', 'nir_median', 'swir1_median', 'swir2_median', 'LST_K_median', "NDVI_median"),c('blue', 'green', 'red', 'nir', 'swir1', 'swir2', 'LST_K', "NDVI"))$sort("date")

# Mask NDVI by Landcover & condense to regional means
for(LCTYPE in lcnames){
  # print(LCTYPE)
  extractByLC(imcol=l9Mosaic, landcover=LCTYPE, outfolder=NDVIsave, fileNamePrefix=paste0("Landsat9_", LCTYPE))
}

##################### 

##################### 
# Read in & Format Landsat 7 ----
##################### 
# ""LANDSAT/LE07/C02/T1_L2""
# Load MODIS NDVI data; attach month & year
# https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LE07_C02_T1_L2
landsat7 <- ee$ImageCollection("LANDSAT/LE07/C02/T1_L2")$filterBounds(Chicago)$filterDate("2001-01-01", "2024-01-19")$map(function(image){
  return(image$clip(Chicago))
})$map(function(img){
  d= ee$Date(img$get('system:time_start'));
  dy= d$get('day');    
  m= d$get('month');
  y= d$get('year');
  
  # # Add masks 
  img <- applyLandsatBitMask(img)
  
  # #scale correction; doing here & separating form NDVI so it gets saved on the image
  lAdj = img$select(c('SR_B1', 'SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B7'))$multiply(0.0000275)$add(-0.2);
  lst_k = img$select('ST_B6')$multiply(0.00341802)$add(149);
  
  # img3 = img2$addBands(srcImg=lAdj, overwrite=T)$addBands(srcImg=lst_k, overwrite=T)$set('date',d, 'day',dy, 'month',m, 'year',y)
  return(img$addBands(srcImg=lAdj, overwrite=T)$addBands(srcImg=lst_k, overwrite=T)$set('date',d, 'day',dy, 'month',m, 'year',y))
})$select(c('SR_B1', 'SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B7', 'ST_B6'),c('blue', 'green', 'red', 'nir', 'swir1', 'swir2', 'LST_K'))$map(addNDVI)


l7Mosaic = mosaicByDate(landsat7, 7)$select(c('blue_median', 'green_median', 'red_median', 'nir_median', 'swir1_median', 'swir2_median', 'LST_K_median', "NDVI_median"),c('blue', 'green', 'red', 'nir', 'swir1', 'swir2', 'LST_K', "NDVI"))$sort("date")

# Mask NDVI by Landcover & condense to regional means
for(LCTYPE in lcnames){
  # print(LCTYPE)
  extractByLC(imcol=l7Mosaic, landcover=LCTYPE, outfolder=NDVIsave, fileNamePrefix=paste0("Landsat7_", LCTYPE))
}

##################### 


##################### 
# Read in & Format Landsat 5 ----
##################### 
# "LANDSAT_LT05_C02_T1_L2"
# Load MODIS NDVI data; attach month & year
# https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LT05_C02_T1_L2
landsat5 <- ee$ImageCollection("LANDSAT/LT05/C02/T1_L2")$filterBounds(Chicago)$filterDate("2001-01-01", "2022-12-31")$map(function(image){
  return(image$clip(Chicago))
})$map(function(img){
  d= ee$Date(img$get('system:time_start'));
  dy= d$get('day');    
  m= d$get('month');
  y= d$get('year');
  
  # # Add masks 
  img <- applyLandsatBitMask(img)
  
  # #scale correction; doing here & separating form NDVI so it gets saved on the image
  lAdj = img$select(c('SR_B1', 'SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B7'))$multiply(0.0000275)$add(-0.2);
  lst_k = img$select('ST_B6')$multiply(0.00341802)$add(149);
  
  # img3 = img2$addBands(srcImg=lAdj, overwrite=T)$addBands(srcImg=lst_k, overwrite=T)$set('date',d, 'day',dy, 'month',m, 'year',y)
  return(img$addBands(srcImg=lAdj, overwrite=T)$addBands(srcImg=lst_k, overwrite=T)$set('date',d, 'day',dy, 'month',m, 'year',y))
})$select(c('SR_B1', 'SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B7', 'ST_B6'),c('blue', 'green', 'red', 'nir', 'swir1', 'swir2', 'LST_K'))$map(addNDVI)

l5Mosaic = mosaicByDate(landsat5, 7)$select(c('blue_median', 'green_median', 'red_median', 'nir_median', 'swir1_median', 'swir2_median', 'LST_K_median', "NDVI_median"),c('blue', 'green', 'red', 'nir', 'swir1', 'swir2', 'LST_K', "NDVI"))$sort("date")

# Mask NDVI by Landcover & condense to regional means
for(LCTYPE in lcnames){
  # print(LCTYPE)
  extractByLC(imcol=l5Mosaic, landcover=LCTYPE, outfolder=NDVIsave, fileNamePrefix=paste0("Landsat5_", LCTYPE))
}
#End of Christy's second script 
##################### 
path.google <- "/Users/jocelyngarcia/Library/CloudStorage/GoogleDrive-jgarcia@mortonarb.org"
pathShare <- file.path(path.google, "Shared drives", "Urban Ecological Drought", "data", "UrbanEcoDrought_NDVI_LocalExtract")
NDVIsave <- ("My Drive/UrbanEcoDrought_NDVI_LocalExtract")

# Check if files are being detected
fNDVI <- dir(file.path(path.google, NDVIsave))
print(fNDVI)  # Should list all files in that directory

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
  fileL7 <- dir(file.path(path.google, NDVIsave), paste0("Landsat7_", LCTYPE))[length(dir(file.path(path.google, NDVIsave), paste0("Landsat7_", LCTYPE)))]
  fileL5 <- dir(file.path(path.google, NDVIsave), paste0("Landsat5_", LCTYPE))[length(dir(file.path(path.google, NDVIsave), paste0("Landsat5_", LCTYPE)))]
  
  if(!file.exists(file.path(pathShare, fileL8))) file.copy(from=file.path(path.google, NDVIsave, fileL8), to=file.path(pathShare, fileL8), overwrite=T, copy.mode=T)
  if(!file.exists(file.path(pathShare, fileL9))) file.copy(from=file.path(path.google, NDVIsave, fileL9), to=file.path(pathShare, fileL9), overwrite=T, copy.mode=T)
  if(!file.exists(file.path(pathShare, fileL7))) file.copy(from=file.path(path.google, NDVIsave, fileL7), to=file.path(pathShare, fileL7), overwrite=T, copy.mode=T)
  if(!file.exists(file.path(pathShare, fileL5))) file.copy(from=file.path(path.google, NDVIsave, fileL5), to=file.path(pathShare, fileL5), overwrite=T, copy.mode=T)
  
  landsat8 <- read.csv(file.path(path.google, NDVIsave, fileL8))
  landsat9 <- read.csv(file.path(path.google, NDVIsave, fileL9))
  landsat7 <- read.csv(file.path(path.google, NDVIsave, fileL7))
  landsat5 <- read.csv(file.path(path.google, NDVIsave, fileL5))
  
  landsat8$mission <- "landsat 8"
  landsat9$mission <- "landsat 9"
  landsat7$mission <- "landsat 7"
  landsat5$mission <- "landsat 5"
  
  landsatAll <- rbind(landsat8, landsat9, landsat7, landsat5)
  # landsatAll <- rbind(landsat8, landsat9)
  landsatAll$type <- LCTYPE
  
  ndviAll <- rbind(ndviAll, landsatAll)
}
summary(ndviAll)
unique(ndviAll$mission)

ndviAll$date <- as.Date(ndviAll$date)
ndviAll$year <- lubridate::year(ndviAll$date)
ndviAll$yday <- lubridate::yday(ndviAll$date)
ndviAll$type <- factor(ndviAll$type, levels=rev(c("forest", "grassland", "crop", "urban-open", "urban-low", "urban-medium", "urban-high")))
head(ndviAll)
summary(ndviAll)
unique(ndviAll$type)

write.csv(ndviAll, file.path(pathShare, "NDVIall_latest.csv"), row.names=F)

##################### 