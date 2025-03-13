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
ndvi.base <- ndvi.base[ndvi.base$year < max(ndvi.base$year),]
summary(ndvi.base)

ndvi.norms <- data.frame(yday=1:365, type=rep(unique(ndvi.base$type), each=365), NormMean=NA, NormLwr=NA, NormUpr=NA)
ndviyrs <- data.frame(yday=rep(1:365), year=rep(unique(ndvi.base$year), each=365), type=rep(unique(ndvi.base$type), each=365*length(unique(ndvi.base$year))), YrMean=NA, YrLwr=NA, YrUpr=NA)

for(LC in unique(ndvi.base$type)){
  print(LC)
  
  rowsLC <- which(ndvi.base$type==LC) 
  gamLC <- gam(NDVI ~ s(yday, k=12, by=mission) + mission-1, data=ndvi.base[rowsLC,]) #k=1 per month 
  summary(gamLC)
  AIC(gamLC)
  
  saveRDS(gamLC, file.path(pathMods, paste0("GAM-Mission_", LC, ".RDS")))
  
  ndvi.base[rowsLC, "NDVIMissionPred"] <- predict(gamLC, newdata=ndvi.base[rowsLC,])
  ndvi.base[rowsLC, "MissionResid"] <- ndvi.base$NDVI[rowsLC] - ndvi.base$NDVIMissionPred[rowsLC]
  
  # Going to "reproject" the predicted mean/normal
  ndviLCDupe <- ndvi.base[rowsLC,]
  ndviLCDupe$mission <- "landsat 8"
  
  ndvi.base[rowsLC, "ReprojPred"] <- predict(gamLC, newdata=ndviLCDupe)
  ndvi.base[rowsLC, "NDVIReprojected"] <- ndvi.base$MissionResid[rowsLC] + ndvi.base$ReprojPred[rowsLC]
  
  gamNorm <- gam(NDVIReprojected ~ s(yday, k=12), data=ndvi.base[rowsLC,])
  # ndvi.norms$NormMean[ndvi.norms$type==LC] <- predict(gamNorm, newdata=ndvi.norms[ndvi.norms$type==LC,])
  LCpost <- post.distns(model.gam = gamNorm, newdata = ndvi.norms[ndvi.norms$type==LC,], vars="yday")
  ndvi.norms[ndvi.norms$type==LC, c("NormMean", "NormLwr", "NormUpr")] <- LCpost[,c("mean", "lwr", "upr")]
  # summary(ndvi.norms)
  saveRDS(gamNorm, file.path(pathMods, paste0("GAM-Normal_", LC, ".RDS")))
  
  gamYRs <-gam(NDVIReprojected ~ s(yday, k=12, by=as.factor(year)) + as.factor(year), data=ndvi.base[rowsLC,])
  # plot(gamYRs)
  YRpost <- post.distns(model.gam = gamYRs, newdata = ndviyrs[ndviyrs$type==LC,], vars=c("yday", "year"))
  # summary(YRpost)
  ndviyrs[ndviyrs$type==LC, c("YrMean", "YrLwr", "YrUpr")] <- YRpost[,c("mean", "lwr", "upr")]
  saveRDS(gamNorm, file.path(pathMods, paste0("GAM-Years-Baseline_", LC, ".RDS")))
}

summary(ndvi.base)
summary(ndvi.norms)
summary(ndviyrs)

write.csv(ndvi.base, file.path(pathDat, "NDVIall_baseline_modeled.csv"), row.names=F)
write.csv(ndvi.norms, file.path(pathDat, "NDVIall_normals_modeled.csv"), row.names=F)
write.csv(ndvi.norms, file.path(pathDat, "NDVIall_years_modeled.csv"), row.names=F)
