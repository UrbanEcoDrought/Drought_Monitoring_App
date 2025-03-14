####################################################################################################################
#Purpose of Script: Feed all landsat data into NDVI Drought Monitoring workflow scripts (1-3) to generate models and normals & save as objects
# Original scripts written by Christy Rollinson and Juliana Harr, workflow put together by Jocelyn Garcia
####################################################################################################################
library(mgcv) #load packages
library(ggplot2)
library(tibble)
library(dplyr)
library(MASS)

# #Sys.setenv(GOOGLE_DRIVE = "~/Google Drive/Shared drives/Urban Ecological Drought")
# rgee::ee_Initialize(user = 'jgarcia@mortonarb.org', drive=T)
# google.drive <- Sys.getenv("GOOGLE_DRIVE")
# path.google <- ("~/Google Drive/My Drive/")
# path.google<-("~/Library/CloudStorage/GoogleDrive-jgarcia@mortonarb.org/Shared drives/Urban Ecological Drought")
path.google <- "~/Google Drive/"
pathShare <- file.path(path.google, "Shared drives", "Urban Ecological Drought", "data", "UrbanEcoDrought_NDVI_LocalExtract-RAW")
pathDat <- "../data_all"
pathMods <- "../gam_models"
if(!dir.exists(pathDat)) dir.create(pathDat, recursive=T)
if(!dir.exists(pathMods)) dir.create(pathMods, recursive = T)

source("0_Calculate_GAMM_Posteriors_Updated_Copy.R")


######################
#loading in and formatting latest NDVI data
######################
if(!file.exists(file.path(pathDat, "NDVIall_baseline.csv")) & file.exists(file.path(pathShare, "NDVIall_baseline.csv"))){
  file.copy(from=file.path(pathShare, "NDVIall_baseline.csv"), to=file.path(pathDat, "NDVIall_baseline.csv"), overwrite=T, copy.mode=T)
}
ndvi.base <- read.csv(file.path(pathDat, "NDVIall_baseline.csv"))
ndvi.base$date <- as.Date(ndvi.base$date)
ndvi.base$type <- as.factor(ndvi.base$type)
ndvi.base$mission <- as.factor(ndvi.base$mission)
# ndvi.base <- ndvi.base[ndvi.base$year < max(ndvi.base$year),]
summary(ndvi.base)

ndvi.norms <- data.frame(yday=1:365, type=rep(unique(ndvi.base$type), each=365), NormMean=NA, NormLwr=NA, NormUpr=NA)
ndviyrs <- data.frame(yday=rep(1:365), year=rep(unique(ndvi.base$year), each=365), type=rep(unique(ndvi.base$type), each=365*length(unique(ndvi.base$year))), YrMean=NA, YrLwr=NA, YrUpr=NA)

for(LC in unique(ndvi.base$type)){
  print(LC)
  
  rowsLC <- which(ndvi.base$type==LC) 
  # 1. Satellite correction
  # 1.a. estimate the shape & intercept of each satellite
  gamLC <- gam(NDVI ~ s(yday, k=12, by=mission) + mission-1, data=ndvi.base[rowsLC,]) #k=1 per month 
  summary(gamLC)
  AIC(gamLC)
  
  saveRDS(gamLC, file.path(pathMods, paste0("GAM-Mission_", LC, ".RDS")))
  
  ndvi.base[rowsLC, "NDVIMissionPred"] <- predict(gamLC, newdata=ndvi.base[rowsLC,])
  ndvi.base[rowsLC, "MissionResid"] <- ndvi.base$NDVI[rowsLC] - ndvi.base$NDVIMissionPred[rowsLC]
  
  # 1.b. harmonize by "reprojecting" the into what it would be like if it were landsat 8 data
  ndviLCDupe <- ndvi.base[rowsLC,]
  ndviLCDupe$mission <- "landsat 8"
  
  ndvi.base[rowsLC, "ReprojPred"] <- predict(gamLC, newdata=ndviLCDupe)
  ndvi.base[rowsLC, "NDVIReprojected"] <- ndvi.base$MissionResid[rowsLC] + ndvi.base$ReprojPred[rowsLC]
  
  # 2. Calculate "normal" for the whole time series we have
  gamNorm <- gam(NDVIReprojected ~ s(yday, k=12), data=ndvi.base[rowsLC,])
  # ndvi.norms$NormMean[ndvi.norms$type==LC] <- predict(gamNorm, newdata=ndvi.norms[ndvi.norms$type==LC,])
  LCpost <- post.distns(model.gam = gamNorm, newdata = ndvi.norms[ndvi.norms$type==LC,], vars="yday")
  ndvi.norms[ndvi.norms$type==LC, c("NormMean", "NormLwr", "NormUpr")] <- LCpost[,c("mean", "lwr", "upr")]
  # summary(ndvi.norms)
  saveRDS(gamNorm, file.path(pathMods, paste0("GAM-Normal_", LC, ".RDS")))
  
  # 3. calculate the year-by-year trends 
  if(!dir.exists(file.path(pathMods, LC))) dir.create(file.path(pathMods, LC))
  for(YR in unique(ndvi.base$year[rowsLC])){
    gamYRs <-gam(NDVIReprojected ~ s(yday, k=12)  , data=ndvi.base[ndvi.base$type==LC & ndvi.base$year==YR,])
    # plot(gamYRs)
    YRpost <- post.distns(model.gam = gamYRs, newdata = ndviyrs[ndviyrs$type==LC & ndviyrs$year==YR,], vars=c("yday", "year"))
    # summary(YRpost)
    ndviyrs[ndviyrs$type==LC & ndviyrs$year==YR, c("YrMean", "YrLwr", "YrUpr")] <- YRpost[,c("mean", "lwr", "upr")]
    saveRDS(gamYRs, file.path(pathMods, LC, paste0("GAM-Years-Baseline_", LC, "_", YR, ".RDS")))
    
  }
}

summary(ndvi.base)
summary(ndvi.norms)
summary(ndviyrs)

ndviyrs <- ndviyrs[ndviyrs$year<max(ndvi.base$year) | (ndviyrs$year==max(ndvi.base$year) & ndviyrs$yday<=lubridate::yday(max(ndvi.base$date))),]

write.csv(ndvi.base, file.path(pathDat, "NDVIall_baseline_modeled.csv"), row.names=F)
write.csv(ndvi.norms, file.path(pathDat, "NDVIall_normals_modeled.csv"), row.names=F)
write.csv(ndviyrs, file.path(pathDat, "NDVIall_years_modeled.csv"), row.names=F)
