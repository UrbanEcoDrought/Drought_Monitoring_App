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
source("0_Calculate_GAMM_Derivs_Copy.R")


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
  
  # 2. Calculate "normal" for the whole time series we have --> using a cyclic spline so start & end match
  gamNorm <- gam(NDVIReprojected ~ s(yday, k=12, bs="cc"), data=ndvi.base[rowsLC,])
  # ndvi.norms$NormMean[ndvi.norms$type==LC] <- predict(gamNorm, newdata=ndvi.norms[ndvi.norms$type==LC,])
  #2.1 Getting the mean & 95 CI on the predicted norms
  LCpost <- post.distns(model.gam = gamNorm, newdata = ndvi.norms[ndvi.norms$type==LC,], vars="yday")
  ndvi.norms[ndvi.norms$type==LC, c("NormMean", "NormLwr", "NormUpr")] <- LCpost[,c("mean", "lwr", "upr")]
  
  #2.2 Getting the mean & 95%CI on the trend
  LCpostDeriv <- calc.derivs(model.gam = gamNorm, newdata = ndvi.norms[ndvi.norms$type==LC,], vars="yday")
  ndvi.norms[ndvi.norms$type==LC, c("NormDerivMean", "NormDerivLwr", "NormDerivUpr")] <- LCpostDeriv[,c("mean", "lwr", "upr")]
  # summary(LCpostDeriv)
  # summary(ndvi.norms)
  
  saveRDS(gamNorm, file.path(pathMods, paste0("GAM-Normal_", LC, ".RDS")))
  
  # 3. calculate the year-by-year trends 
  if(!dir.exists(file.path(pathMods, LC))) dir.create(file.path(pathMods, LC))
  for(YR in unique(ndvi.base$year[rowsLC])){
    datTmp <- ndvi.base[ndvi.base$type==LC & ndvi.base$year==YR,]
    kYr=12
    
    # If not our first year, add in the previous Dec
    if(YR>min(ndvi.base$year[rowsLC])){
      datTmpDec <- ndvi.base[ndvi.base$type==LC & ndvi.base$year==YR-1 & ndvi.base$yday>=365-31,]
      datTmpDec$yday <- datTmpDec$yday-365
      
      datTmp <- rbind(datTmp, datTmpDec)
      kYr=kYr+1
    }
    if(YR<max(ndvi.base$year[rowsLC])){
      datTmpJan <- ndvi.base[ndvi.base$type==LC & ndvi.base$year==YR+1 & ndvi.base$yday<=31,]
      datTmpJan$yday <- datTmpJan$yday+365
      
      datTmp <- rbind(datTmp, datTmpJan)
      kYr=kYr+1
      
    }
    
    gamYRs <-gam(NDVIReprojected ~ s(yday, k=kYr), data=datTmp)
    # plot(gamYRs)
    # 3.1 mean & 95%CI on predicted NDVI
    YRpost <- post.distns(model.gam = gamYRs, newdata = ndviyrs[ndviyrs$type==LC & ndviyrs$year==YR,], vars=c("yday", "year"))
    # summary(YRpost)
    ndviyrs[ndviyrs$type==LC & ndviyrs$year==YR, c("YrMean", "YrLwr", "YrUpr")] <- YRpost[,c("mean", "lwr", "upr")]
    
    # 3.2 mean & 95%CI on NDVI trend
    YrPostDeriv <- calc.derivs(model.gam = gamYRs, newdata = ndviyrs[ndviyrs$type==LC & ndviyrs$year==YR, ], vars=c("yday"))
    ndviyrs[ndviyrs$type==LC & ndviyrs$year==YR, c("YrDerivMean", "YrDerivLwr", "YrDerivUpr")] <- YrPostDeriv[,c("mean", "lwr", "upr")]
    
    # 3.3 add tag on trend
    ndviyrs[ndviyrs$type==LC & ndviyrs$year==YR & ndviyrs$YrDerivUpr<0, "YrDerivTrend"] <- "Getting Browner"
    ndviyrs[ndviyrs$type==LC & ndviyrs$year==YR & ndviyrs$YrDerivLwr>0, "YrDerivTrend"] <- "Getting Greener"
    ndviyrs[ndviyrs$type==LC & ndviyrs$year==YR & ndviyrs$YrDerivLwr<0 & ndviyrs$YrDerivUpr>0, "YrDerivTrend"] <- "No Change"
    summary(ndviyrs)
    
    saveRDS(gamYRs, file.path(pathMods, LC, paste0("GAM-Years-Baseline_", LC, "_", YR, ".RDS")))
    
    # 4. compare years to norms to flag -- could probably do this wihtout a loop, but looping to be safe
    # NDVI Flags
    #   Significantly Browner than norm = yr CI < norm CI
    #   Slightly Browner than norm = yr mean < norm CI, but yr CI overlaps
    #   Normal = yr mean in norm CI
    #   Slightly Greener than norm = yr mean > norm CI, but yr CI overlaps
    #   Significantly Greener than norm = year CI > norm CI
    # Trend Flag
    #   Greening faster than normal -- both yr & norm pos; yr > norm
    #   Greening slower than normal -- both yr & norm pos; abs(yr < norm)
    #   Browning faster than normal -- both yr & norm neg; yr < norm
    #   Browning slower than normal -- both yr & norm neg; abs(yr < norm)
    #   Abnormal greening - yr pos & norm not pos
    #   Abnormal browning - yr neg & norm not neg
    for(YDAY in unique(ndviyrs$yday[ndviyrs$type==LC & ndviyrs$year==YR])){
      rowInd <- which(ndviyrs$type==LC & ndviyrs$year==YR & ndviyrs$yday==YDAY)
      Norm <- ndvi.norms[ndvi.norms$type==LC & ndvi.norms$yday==YDAY,]
      # ndviyrs[rowInd,]
      
      ndviFlag <- ifelse(ndviyrs$YrLwr[rowInd] > Norm$NormUpr, "Significantly Greener than Normal",
                         ifelse(ndviyrs$YrUpr[rowInd] < Norm$NormLwr, "Significantly Browner than Normal",
                                ifelse(ndviyrs$YrMean[rowInd] > Norm$NormUpr, "Slightly Greener than Normal",
                                       ifelse(ndviyrs$YrMean[rowInd] < Norm$NormLwr, "Slightly Browner than Normal",
                                              "Normal"))))
      
      
      trendFlag <- ifelse(ndviyrs$YrDerivLwr[rowInd] > Norm$NormDerivUpr & 
                            all(c(ndviyrs$YrDerivLwr[rowInd], Norm$NormDerivLwr)>0), "Greening Faster than Normal",
                         ifelse(ndviyrs$YrDerivUpr[rowInd] < Norm$NormDerivLwr & 
                                   all(c(ndviyrs$YrDerivLwr[rowInd], Norm$NormDerivLwr)>0), "Greening Slower than Normal",
                         ifelse(ndviyrs$YrDerivUpr[rowInd] < Norm$NormDerivLwr & 
                                  all(c(ndviyrs$YrDerivUpr[rowInd], Norm$NormDerivUpr)<0), "Browning Faster than Normal",
                                ifelse(ndviyrs$YrDerivUpr[rowInd] > Norm$NormDerivLwr & 
                                         all(c(ndviyrs$YrDerivUpr[rowInd], Norm$NormDerivUpr)<0), "Browning Slower than Normal",
                                ifelse(ndviyrs$YrDerivLwr[rowInd] > 0 & 
                                         ((Norm$NormDerivUpr>0 & Norm$NormDerivLwr<0) | 
                                            (Norm$NormDerivUpr<0 & Norm$NormDerivLwr<0)), "Abnormal Greening",
                                       ifelse(ndviyrs$YrDerivUpr[rowInd] < 0 & 
                                                ((Norm$NormDerivUpr>0 & Norm$NormDerivLwr<0) | 
                                                   (Norm$NormDerivUpr>0 & Norm$NormDerivLwr>0)), "Abnormal Browning",
                                              "Normal"))))))
      
      ndviyrs[rowInd,c("FlagNDVI", "FlagTrend")] <- c(ndviFlag, trendFlag)
      
    }# end YDAY loop
    
  } # End Yr loop
}

summary(ndvi.base)
summary(ndvi.norms)
summary(ndviyrs)

ndvi.base$type <- car::recode(ndvi.base$type, "'forest-wet'='forest'")
ndvi.norms$type <- car::recode(ndvi.norms$type, "'forest-wet'='forest'")
ndviyrs$type <- car::recode(ndviyrs$type, "'forest-wet'='forest'")

ndviyrs <- ndviyrs[ndviyrs$year<max(ndvi.base$year) | (ndviyrs$year==max(ndvi.base$year) & ndviyrs$yday<=lubridate::yday(max(ndvi.base$date))),]
# ndviyrs$date <- strptime(paste(ndviyrs$year, ndviyrs$yday, sep="-"), format=c("%Y-%j"))
summary(ndviyrs$date)
head(ndviyrs[is.na(ndviyrs$date),])

# Merge our actual observations into the ndviyrs file
ndvi.baseAgg <- aggregate(NDVIReprojected ~ type + year + yday + date, data=ndvi.base, FUN=mean, na.rm=T)
summary(ndvi.baseAgg)
dim(ndvi.baseAgg)

dim(ndviyrs)
ndviyrs <- merge(ndviyrs, ndvi.baseAgg[!is.na(ndvi.base$NDVIReprojected),c("type", "year", "yday", "NDVIReprojected")], all.x=T)
dim(ndviyrs)

summary(as.factor(ndviyrs$YrDerivTrend))
summary(as.factor(ndviyrs$FlagNDVI))
summary(as.factor(ndviyrs$FlagTrend))
# summary(ndviyrs)

# Doing a quick graph to test
ggplot(data=ndviyrs[ndviyrs$year==2012,]) +
  ggtitle("Year = 2012") +
  facet_wrap(~type) +
  geom_ribbon(data=ndvi.norms, aes(x=yday, ymin=NormLwr, ymax=NormUpr), fill="black", alpha=0.2) +
  geom_line(data=ndvi.norms, aes(x=yday, y=NormMean), color="black") +
  geom_ribbon(aes(x=yday, ymin=YrLwr, ymax=YrUpr), fill="blue", alpha=0.2) +
  geom_line(aes(x=yday, y=YrMean), color="blue") +
  geom_point(aes(x=yday, y=YrMean, color=FlagNDVI), size=0.2)

ggplot(data=ndviyrs[ndviyrs$year==2024,]) +
  ggtitle("Year = 2014") +
  facet_wrap(~type) +
  geom_ribbon(data=ndvi.norms, aes(x=yday, ymin=NormLwr, ymax=NormUpr), fill="black", alpha=0.2) +
  geom_line(data=ndvi.norms, aes(x=yday, y=NormMean), color="black") +
  geom_ribbon(aes(x=yday, ymin=YrLwr, ymax=YrUpr), fill="blue", alpha=0.2) +
  geom_line(aes(x=yday, y=YrMean), color="blue") +
  geom_point(aes(x=yday, y=YrMean, color=FlagNDVI), size=0.2)


write.csv(ndvi.base, file.path(pathDat, "NDVIall_baseline_modeled.csv"), row.names=F)
write.csv(ndvi.norms, file.path(pathDat, "NDVIall_normals_modeled.csv"), row.names=F)
write.csv(ndviyrs, file.path(pathDat, "NDVIall_years_modeled.csv"), row.names=F)
