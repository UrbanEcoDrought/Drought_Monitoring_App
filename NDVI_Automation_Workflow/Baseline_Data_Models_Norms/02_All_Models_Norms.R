####################################################################################################################
#Purpose of Script: Feed all landsat data into NDVI Drought Monitoring workflow scripts (1-3) to generate models and normals & save as objects
# Original scripts written by Christy Rollinson and Juliana Harr, workflow put together by Jocelyn Garcia
####################################################################################################################
library(mgcv) #load packages
library(ggplot2)
library(tibble)
library(dplyr)
library(MASS)

#Sys.setenv(GOOGLE_DRIVE = "~/Google Drive/Shared drives/Urban Ecological Drought")
rgee::ee_Initialize(user = 'jgarcia@mortonarb.org', drive=T)
google.drive <- Sys.getenv("GOOGLE_DRIVE")
#path.google <- ("~/Google Drive/My Drive/")
path.google<-("~/Library/CloudStorage/GoogleDrive-jgarcia@mortonarb.org/Shared drives/Urban Ecological Drought")
pathShare <- file.path(path.google, "/data/NDVI_drought_monitoring")

######################
#loading in and formatting latest NDVI data
######################

ndvi.latest <- read.csv(file.path(path.google, "data/UrbanEcoDrought_NDVI_LocalExtract/NDVIall_latest.csv"))
ndvi.latest$date <- as.Date(ndvi.latest$date)
ndvi.latest$type <- as.factor(ndvi.latest$type)
ndvi.latest$mission <- as.factor(ndvi.latest$mission)
summary(ndvi.latest)

######################
#crop
######################

ndvicrop=ndvi.latest[ndvi.latest$type=="crop",]

gamcrop <- gam(NDVI ~ s(yday, k=12, by=mission) + mission-1, data=ndvicrop) #k=1.5 months in a year
summary(gamcrop)
AIC(gamcrop)

ndvicrop$NDVIMissionPred <- predict(gamcrop, newdata=ndvicrop)
ndvicrop$MissionResid <- ndvicrop$NDVI - ndvicrop$NDVIMissionPred

# Going to "reproject" the predicted mean/normal
ndvicropDupe <- ndvicrop
ndvicropDupe$mission <- "landsat 8"

ndvicrop$ReprojPred <- predict(gamcrop, newdata=ndvicropDupe)
ndvicrop$NDVIReprojected <- ndvicrop$MissionResid + ndvicrop$ReprojPred

summary(ndvicrop)

######################
#forest
######################

ndviforest=ndvi.latest[ndvi.latest$type=="forest",]

gamforest <- gam(NDVI ~ s(yday, k=12, by=mission) + mission-1, data=ndviforest) #k=1.5 months in a year
summary(gamforest)
AIC(gamforest)

ndviforest$NDVIMissionPred <- predict(gamforest, newdata=ndviforest)
ndviforest$MissionResid <- ndviforest$NDVI - ndviforest$NDVIMissionPred

# Going to "reproject" the predicted mean/normal
ndviforestDupe <- ndviforest
ndviforestDupe$mission <- "landsat 8"

ndviforest$ReprojPred <- predict(gamforest, newdata=ndviforestDupe)
ndviforest$NDVIReprojected <- ndviforest$MissionResid + ndviforest$ReprojPred
summary(ndviforest)

######################
#grassland
######################

ndvigrass <- ndvi.latest[ndvi.latest$type=="grassland",]

gamgrass <- gam(NDVI ~ s(yday, k=12, by=mission) + mission-1, data=ndvigrass)
summary(gamgrass)
AIC(gamgrass)

ndvigrass$NDVIMissionPred <- predict(gamgrass, newdata=ndvigrass)
ndvigrass$MissionResid <- ndvigrass$NDVI - ndvigrass$NDVIMissionPred

# Going to "reproject" the predicted mean/normal
ndvigrassDupe <- ndvigrass
ndvigrassDupe$mission <- "landsat 8"

ndvigrass$ReprojPred <- predict(gamgrass, newdata=ndvigrassDupe)
ndvigrass$NDVIReprojected <- ndvigrass$MissionResid + ndvigrass$ReprojPred
summary(ndvigrass)

######################
#urban-high
######################

ndviUrbHigh <- ndvi.latest[ndvi.latest$type=="urban-high",]

gamUrbHigh <- gam(NDVI ~ s(yday, k=12, by=mission) + mission-1, data=ndviUrbHigh)
summary(gamUrbHigh)
AIC(gamUrbHigh)

ndviUrbHigh$NDVIMissionPred <- predict(gamUrbHigh, newdata=ndviUrbHigh)
ndviUrbHigh$MissionResid <- ndviUrbHigh$NDVI - ndviUrbHigh$NDVIMissionPred

# Going to "reproject" the predicted mean/normal
ndviUrbHighDupe <- ndviUrbHigh
ndviUrbHighDupe$mission <- "landsat 8"

ndviUrbHigh$ReprojPred <- predict(gamUrbHigh, newdata=ndviUrbHighDupe)
ndviUrbHigh$NDVIReprojected <- ndviUrbHigh$MissionResid + ndviUrbHigh$ReprojPred
summary(ndviUrbHigh)

######################
#urban-medium
######################

ndviUrbMed <- ndvi.latest[ndvi.latest$type=="urban-medium",]

gamUrbMed <- gam(NDVI ~ s(yday, k=12, by=mission) + mission-1, data=ndviUrbMed)
summary(gamUrbMed)
AIC(gamUrbMed)

ndviUrbMed$NDVIMissionPred <- predict(gamUrbMed, newdata=ndviUrbMed)
ndviUrbMed$MissionResid <- ndviUrbMed$NDVI - ndviUrbMed$NDVIMissionPred

# Going to "reproject" the predicted mean/normal
ndviUrbMedDupe <- ndviUrbMed
ndviUrbMedDupe$mission <- "landsat 8"

ndviUrbMed$ReprojPred <- predict(gamUrbMed, newdata=ndviUrbMedDupe)
ndviUrbMed$NDVIReprojected <- ndviUrbMed$MissionResid + ndviUrbMed$ReprojPred
summary(ndviUrbMed)

######################
#urban-low
######################

ndviUrbLow <- ndvi.latest[ndvi.latest$type=="urban-low",]

gamUrbLow <- gam(NDVI ~ s(yday, k=12, by=mission) + mission-1, data=ndviUrbLow)
summary(gamUrbLow)
AIC(gamUrbLow)

ndviUrbLow$NDVIMissionPred <- predict(gamUrbLow, newdata=ndviUrbLow)
ndviUrbLow$MissionResid <- ndviUrbLow$NDVI - ndviUrbLow$NDVIMissionPred

# Going to "reproject" the predicted mean/normal
ndviUrbLowDupe <- ndviUrbLow
ndviUrbLowDupe$mission <- "landsat 8"

ndviUrbLow$ReprojPred <- predict(gamUrbLow, newdata=ndviUrbLowDupe)
ndviUrbLow$NDVIReprojected <- ndviUrbLow$MissionResid + ndviUrbLow$ReprojPred
summary(ndviUrbLow)

######################
#urban-open
######################

ndviUrbOpen <- ndvi.latest[ndvi.latest$type=="urban-open",]

gamUrbOpen <- gam(NDVI ~ s(yday, k=12, by=mission) + mission-1, data=ndviUrbOpen)
summary(gamUrbOpen)
AIC(gamUrbOpen)

ndviUrbOpen$NDVIMissionPred <- predict(gamUrbOpen, newdata=ndviUrbOpen)
ndviUrbOpen$MissionResid <- ndviUrbOpen$NDVI - ndviUrbOpen$NDVIMissionPred

# Going to "reproject" the predicted mean/normal
ndviUrbOpenDupe <- ndviUrbOpen
ndviUrbOpenDupe$mission <- "landsat 8"

ndviUrbOpen$ReprojPred <- predict(gamUrbOpen, newdata=ndviUrbOpenDupe)
ndviUrbOpen$NDVIReprojected <- ndviUrbOpen$MissionResid + ndviUrbOpen$ReprojPred
summary(ndviUrbOpen)

######################
#combine into one large dataframe & save
######################

raw_data <- rbind(ndvicrop, ndviforest, ndvigrass, ndviUrbHigh, ndviUrbMed, ndviUrbLow, ndviUrbOpen)
write.csv(raw_data, file.path(pathShare, "raw_data_k=12.csv"), row.names=F)
#End of Juliana's first script
####################################################################################################################
#Saving models as objects
save(gamcrop, gamforest, gamgrass, gamUrbHigh, gamUrbLow, gamUrbMed, gamUrbOpen, file = "gam_models.RData")
####################################################################################################################
source("0_Calculate_GAMM_Posteriors_Updated_Copy.R")

######################
#loading in and formatting raw data from 01_raw_data.R
######################

raw.data <- read.csv(file.path(path.google, "data/NDVI_drought_monitoring/raw_data_k=12.csv"))
newDF <- data.frame(yday=seq(1:365)) #create new data frame with column to represent day of year sequence

######################
#crop
######################

gamcrop_norm <- gam(NDVIReprojected ~ s(yday, k=12), data=raw.data[raw.data$type=="crop",])
NDVIcrop_norm <- predict(gamcrop_norm, newdata=newDF) #normal crop values for a year
crop_norm <- post.distns(model.gam=gamcrop_norm, newdata=newDF, vars="yday")
crop_norm$type <- "crop"
#crop_norm$NDVIpred <- NDVIcrop_norm

######################
#forest
######################

gamforest_norm <- gam(NDVIReprojected ~ s(yday, k=12), data=raw.data[raw.data$type=="forest",])
NDVIforest_norm <- predict(gamforest_norm, newdata=newDF)
forest_norm <- post.distns(model.gam = gamforest_norm, newdata = newDF, vars="yday")
forest_norm$type <- "forest"
#forest_norm$NDVIpred <- NDVIforest_norm

######################
#grassland
######################

gamgrass_norm <- gam(NDVIReprojected ~ s(yday, k=12), data=raw.data[raw.data$type=="grassland",])
NDVIgrass_norm <- predict(gamgrass_norm, newdata=newDF)
grass_norm <- post.distns(model.gam = gamgrass_norm, newdata = newDF, vars="yday")
grass_norm$type <- "grassland"
#grass_norm$NDVIpred <- NDVIgrass_norm

######################
#urban-high
######################

gamUrbHigh_norm <- gam(NDVIReprojected ~ s(yday, k=12), data=raw.data[raw.data$type=="urban-high",])
NDVIUrbHigh_norm <- predict(gamUrbHigh_norm, newdata=newDF)
UrbHigh_norm <- post.distns(model.gam = gamUrbHigh_norm, newdata = newDF, vars="yday")
UrbHigh_norm$type <- "urban-high"
#UrbHigh_norm$NDVIpred <- NDVIUrbHigh_norm

######################
#urban-medium
######################

gamUrbMed_norm <- gam(NDVIReprojected ~ s(yday, k=12), data=raw.data[raw.data$type=="urban-medium",])
NDVIUrbMed_norm <- predict(gamUrbMed_norm, newdata=newDF)
UrbMed_norm <- post.distns(model.gam = gamUrbMed_norm, newdata = newDF, vars="yday")
UrbMed_norm$type <- "urban-medium"
#UrbMed_norm$NDVIpred <- NDVIUrbMed_norm

######################
#urban-low
######################

gamUrbLow_norm <- gam(NDVIReprojected ~ s(yday, k=12), data=raw.data[raw.data$type=="urban-low",])
NDVIUrbLow_norm <- predict(gamUrbLow_norm, newdata=newDF)
UrbLow_norm <- post.distns(model.gam = gamUrbLow_norm, newdata = newDF, vars="yday")
UrbLow_norm$type <- "urban-low"
#UrbLow_norm$NDVIpred <- NDVIUrbLow_norm

######################
#urban-open
######################

gamUrbOpen_norm <- gam(NDVIReprojected ~ s(yday, k=12), data=raw.data[raw.data$type=="urban-open",])
NDVIUrbOpen_norm <- predict(gamUrbOpen_norm, newdata=newDF)
UrbOpen_norm <- post.distns(model.gam = gamUrbOpen_norm, newdata = newDF, vars="yday")
UrbOpen_norm$type <- "urban-open"
#UrbOpen_norm$NDVIpred <- NDVIUrbOpen_norm

######################
#combine into one large dataframe & save
######################

norms <- rbind(crop_norm, forest_norm, grass_norm, UrbHigh_norm, UrbMed_norm, UrbLow_norm, UrbOpen_norm)
write.csv(norms, file.path(pathShare, "k=12_norms_all_LC_types.csv"), row.names=F)
#End of Juliana's second script

####################################################################################################################
#Saving norms as objects
save(crop_norm, forest_norm, grass_norm, UrbHigh_norm, UrbMed_norm, UrbLow_norm, UrbOpen_norm, file = "norms.RData")
####################################################################################################################