####################################################################################################################
#Purpose of Baseline_Data_Models_Norms Workflow: Reorganize and add to existing scripts to form a workflow that will
#                                                be run once to create models & norms based on all data.
#Purpose of Script: Pull all landsat data & store as CSV in shared google drive 
# Original scripts written by Christy Rollinson and Juliana Harr, workflow put together by Jocelyn Garcia
####################################################################################################################
library(rgee); library(raster); library(terra)
ee_check() # For some reason, it's important to run this before initializing right now
rgee::ee_Initialize(user = 'jgarcia@mortonarb.org', drive=T, project = "urbanecodrought")
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

# https://developers.google.com/earth-engine/datasets/catalog/USGS_NLCD_RELEASES_2019_REL_NLCD
nlcdChi <- ee$ImageCollection('USGS/NLCD_RELEASES/2019_REL/NLCD')$select('landcover')$map(function(img){
  d <- ee$Date(ee$Number(img$get('system:time_start')));
  y <- ee$Number(d$get('year'));
  return(img$clip(Chicago)$set('year', y))
})

# https://developers.google.com/earth-engine/datasets/catalog/USGS_NLCD_RELEASES_2021_REL_NLCD#description
nlcdChi21 <- ee$ImageCollection('USGS/NLCD_RELEASES/2021_REL/NLCD')$select('landcover')$map(function(img){
  d <- ee$Date(ee$Number(img$get('system:time_start')));
  y <- ee$Number(d$get('year'));
  return(img$clip(Chicago)$set('year', y))
})

lcVals = nlcdChi$first()$reduceRegion(ee$Reducer$frequencyHistogram(), Chicago, maxPixels=1e12)
lcVals$getInfo()

# ee_print(nlcdChi) # Note: the nlcd is giving me a strsplit code error, but it does map!
Map$addLayer(nlcdChi$first()$select('landcover'), nlcdvis, 'NLCD Land Cover',);
Map$addLayer(nlcdChi21$first()$select('landcover'), nlcdvis, 'NLCD Land Cover',);


projNLCD = nlcdChi$select("landcover")$first()$projection()
projNLCD$getInfo() # Note that this is really messy
projCRS = "EPSG:4326" # This seems to be what works
projTransform <- unlist(projNLCD$getInfo()$transform) # I saved this, but using it in the export causes really weird things

nlcdProj = "EPSG:4326"
nlcdTransform = c(30, 0, -2493045, 0, -30, 3310005)

##################### 
# Create annually-resolved image ----
##################### 
lcChi2001 <- nlcdChi$filter(ee$Filter$eq('system:index', '2001'))$first();
lcChi2004 <- nlcdChi$filter(ee$Filter$eq('system:index', '2004'))$first();
lcChi2006 <- nlcdChi$filter(ee$Filter$eq('system:index', '2006'))$first();
lcChi2008 <- nlcdChi$filter(ee$Filter$eq('system:index', '2008'))$first();
lcChi2011 <- nlcdChi$filter(ee$Filter$eq('system:index', '2011'))$first();
lcChi2013 <- nlcdChi$filter(ee$Filter$eq('system:index', '2013'))$first();
lcChi2016 <- nlcdChi$filter(ee$Filter$eq('system:index', '2016'))$first();
lcChi2019 <- nlcdChi$filter(ee$Filter$eq('system:index', '2019'))$first();
lcChi2021 <- nlcdChi21$filter(ee$Filter$eq('system:index', '2021'))$first();

# Creating duped layers for each year for our sanity
lcChi2000 <- lcChi2001$set('system:time_start', ee$Date$fromYMD(2000, 1, 1))$set('year',2000);
# lcChi2001 <- nlcdChi$filter(ee$Filter$eq('system:index', '2001'))$first();
lcChi2002 <- lcChi2001$set('system:time_start', ee$Date$fromYMD(2002, 1, 1))$set('year',2002);
lcChi2003 <- lcChi2004$set('system:time_start', ee$Date$fromYMD(2003, 1, 1))$set('year',2003);
# lcChi2004 <- nlcdChi$filter(ee$Filter$eq('system:index', '2004'))$first();
lcChi2005 <- lcChi2006$set('system:time_start', ee$Date$fromYMD(2005, 1, 1))$set('year',2005);
# lcChi2006 <- nlcdChi$filter(ee$Filter$eq('system:index', '2006'))$first();
lcChi2007 <- lcChi2008$set('system:time_start', ee$Date$fromYMD(2007, 1, 1))$set('year',2007);
# lcChi2008 <- nlcdChi$filter(ee$Filter$eq('system:index', '2008'))$first();
lcChi2009 <- lcChi2008$set('system:time_start', ee$Date$fromYMD(2009, 1, 1))$set('year',2009);
lcChi2010 <- lcChi2011$set('system:time_start', ee$Date$fromYMD(2010, 1, 1))$set('year',2010);
# lcChi2011 <- nlcdChi$filter(ee$Filter$eq('system:index', '2011'))$first();
lcChi2012 <- lcChi2013$set('system:time_start', ee$Date$fromYMD(2012, 1, 1))$set('year',2012);
# lcChi2013 <- nlcdChi$filter(ee$Filter$eq('system:index', '2013'))$first();
lcChi2014 <- lcChi2013$set('system:time_start', ee$Date$fromYMD(2014, 1, 1))$set('year',2014);
lcChi2015 <- lcChi2016$set('system:time_start', ee$Date$fromYMD(2015, 1, 1))$set('year',2015);
# lcChi2016 <- nlcdChi$filter(ee$Filter$eq('system:index', '2016'))$first();
lcChi2017 <- lcChi2016$set('system:time_start', ee$Date$fromYMD(2017, 1, 1))$set('year',2017);
lcChi2018 <- lcChi2019$set('system:time_start', ee$Date$fromYMD(2018, 1, 1))$set('year',2018);
# lcChi2019 <- nlcdChi$filter(ee$Filter$eq('system:index', '2019'))$first();
lcChi2020 <- lcChi2019$set('system:time_start', ee$Date$fromYMD(2020, 1, 1))$set('year',2020);
lcChi2021 <- lcChi2021$set('system:time_start', ee$Date$fromYMD(2021, 1, 1))$set('year',2021);
lcChi2022 <- lcChi2021$set('system:time_start', ee$Date$fromYMD(2022, 1, 1))$set('year',2022);
lcChi2023 <- lcChi2021$set('system:time_start', ee$Date$fromYMD(2023, 1, 1))$set('year',2023);
lcChi2024 <- lcChi2021$set('system:time_start', ee$Date$fromYMD(2024, 1, 1))$set('year',2024);
lcChi2025 <- lcChi2021$set('system:time_start', ee$Date$fromYMD(2025, 1, 1))$set('year',2025);

collAnn <- ee$ImageCollection(c(lcChi2001, lcChi2002, lcChi2003, lcChi2004, lcChi2005, lcChi2006, lcChi2007, lcChi2008, lcChi2009, lcChi2010, lcChi2011, lcChi2012, lcChi2013, lcChi2014, lcChi2015, lcChi2016, lcChi2017, lcChi2018, lcChi2019, lcChi2020, lcChi2021, lcChi2022, lcChi2023, lcChi2024,lcChi2025))

# Saving will be much easier if it's a single year with multiple bands
yrLC <- ee$List(collAnn$aggregate_array("year"))$distinct()
# yrLC$getInfo()
yrString <- ee$List(paste0("YR", yrLC$getInfo()))

lcChiAnn <- ee$ImageCollection$toBands(collAnn)$rename(yrString)
# ee_print(lcChiAnn)
Map$addLayer(lcChiAnn$select("YR2012"), nlcdvis, 'NLCD Land Cover');

saveLandCover <- ee_image_to_asset(lcChiAnn, description="Save_NLCD-Chicago_AnnualDupe_2000-2024", assetId=file.path(assetHome, "NLCD-Chicago_AnnualDupe_2000-2024"), maxPixels = 10e9, scale=30, region = chiBounds, crs=projCRS, overwrite=T)
saveLandCover$start()

##################### 
# # Define the land cover classes and their corresponding values

# # Forest: 41,42,43 ----
classFor = collAnn$map(function(image) {
  d = ee$Date(ee$Number(image$get('system:time_start')))
  lcMask = image$select('landcover')$eq(41)$Or(image$select('landcover')$eq(42))$Or(image$select('landcover')$eq(43));
  return(image$updateMask(lcMask)$set('class', 'Forest')$set('year', d$get('year')));
});

forMask <- ee$ImageCollection$toBands(classFor)$rename(yrString)
# ee_print(forMask)
# Map$addLayer(forMask$select("YR2012"));

saveForMask <- ee_image_to_asset(forMask, description="Save_lcMask-Forest_2000-2024", assetId=file.path(assetHome, "NLCD-Chicago_2000-2024_Forest"), maxPixels = 10e9, scale=30, region = chiBounds, crs="EPSG:4326", overwrite=T)
saveForMask$start()
# nlcdProj

# # Grassland/Savanna/Grass: 51,52,71,72 ----
classGrass = collAnn$map(function(image) {
  d = ee$Date(ee$Number(image$get('system:time_start')))
  lcMask = image$select('landcover')$eq(51)$Or(image$select('landcover')$eq(52))$Or(image$select('landcover')$eq(71))$Or(image$select('landcover')$eq(72));
  return(image$updateMask(lcMask)$set('class', 'Grassland')$set('year', d$get('year')));
});

grassMask <- ee$ImageCollection$toBands(classGrass)$rename(yrString)

saveGrassMask <- ee_image_to_asset(grassMask, description="Save_lcMask-Grass_2000-2024", assetId=file.path(assetHome, "NLCD-Chicago_2000-2024_Grass"), maxPixels = 10e9, scale=30, region = chiBounds, crs="EPSG:4326", overwrite=T)
saveGrassMask$start()

# # Crop = 81,82 ----
classCrop = collAnn$map(function(image) {
  d = ee$Date(ee$Number(image$get('system:time_start')))
  lcMask = image$select('landcover')$eq(81)$Or(image$select('landcover')$eq(82));
  return(image$updateMask(lcMask)$set('class', 'Crop')$set('year', d$get('year')));
});

cropMask <- ee$ImageCollection$toBands(classCrop)$rename(yrString)

saveCropMask <- ee_image_to_asset(cropMask, description="Save_lcMask-Crop_2000-2024", assetId=file.path(assetHome, "NLCD-Chicago_2000-2024_Crop"), maxPixels = 10e9, scale=30, region = chiBounds, crs="EPSG:4326", overwrite=T)
saveCropMask$start()


# # Open Urban: 21 ----
classUrbO = collAnn$map(function(image) {
  d = ee$Date(ee$Number(image$get('system:time_start')))
  lcMask = image$select('landcover')$eq(21);
  return(image$updateMask(lcMask)$set('class', 'Urban-Open')$set('year', d$get('year')));
});

urbOMask <- ee$ImageCollection$toBands(classUrbO)$rename(yrString)

saveUrbOMask <- ee_image_to_asset(urbOMask, description="Save_lcMask-Urban-Open_2000-2024", assetId=file.path(assetHome, "NLCD-Chicago_2000-2024_Urban-Open"), maxPixels = 10e9, scale=30, region = chiBounds, crs="EPSG:4326", overwrite=T)
saveUrbOMask$start()


# # Low Intensity Urban: 22 ----
classUrbL = collAnn$map(function(image) {
  d = ee$Date(ee$Number(image$get('system:time_start')))
  lcMask = image$select('landcover')$eq(22);
  return(image$updateMask(lcMask)$set('class', 'Urban-Low')$set('year', d$get('year')));
});

urbLMask <- ee$ImageCollection$toBands(classUrbL)$rename(yrString)

saveUrbLMask <- ee_image_to_asset(urbLMask, description="Save_lcMask-Urban-Low_2000-2024", assetId=file.path(assetHome, "NLCD-Chicago_2000-2024_Urban-Low"), maxPixels = 10e9, scale=30, region = chiBounds, crs="EPSG:4326", overwrite=T)
saveUrbLMask$start()


# # Medium Intensity Urban: 23 ----
classUrbM = collAnn$map(function(image) {
  d = ee$Date(ee$Number(image$get('system:time_start')))
  lcMask = image$select('landcover')$eq(23);
  return(image$updateMask(lcMask)$set('class', 'Urban-Medium')$set('year', d$get('year')));
});

urbMMask <- ee$ImageCollection$toBands(classUrbM)$rename(yrString)

saveUrbMMask <- ee_image_to_asset(urbMMask, description="Save_lcMask-Urban-Medium_2000-2024", assetId=file.path(assetHome, "NLCD-Chicago_2000-2024_Urban-Medium"), maxPixels = 10e9, scale=30, region = chiBounds, crs="EPSG:4326", overwrite=T)
saveUrbMMask$start()


# # High Intenstity Urban: 24 ----
classUrbH = collAnn$map(function(image) {
  d = ee$Date(ee$Number(image$get('system:time_start')))
  lcMask = image$select('landcover')$eq(24);
  return(image$updateMask(lcMask)$set('class', 'Urban-Low')$set('year', d$get('year')));
});

urbHMask <- ee$ImageCollection$toBands(classUrbH)$rename(yrString)

saveUrbHMask <- ee_image_to_asset(urbHMask, description="Save_lcMask-Urban-High_2000-2024", assetId=file.path(assetHome, "NLCD-Chicago_2000-2024_Urban-High"), maxPixels = 10e9, scale=30, region = chiBounds, crs="EPSG:4326", overwrite=T)
saveUrbHMask$start()
#End of Christy's first script
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