####################################################################################################################
#Purpose of Script: Readjust year specific spline using models & norms from Baseline_Data_Models_Norms Workflow
# Original scripts written by Christy Rollinson and Juliana Harr, workflow put together by Jocelyn Garcia
####################################################################################################################
if(!"NDVI_Automation_Workflow" %in% dir()) setwd("../..")

library(mgcv) #load packages
library(ggplot2)
library(tibble)
library(dplyr)
library(MASS)

#file paths
# Sys.setenv(GOOGLE_DRIVE = "~/Google Drive/Shared drives/Urban Ecological Drought")
path.UrbDrought <- "/Users/crollinson/Google Drive/"
pathShare <- file.path(path.google, "Shared drives/Urban Ecological Drought/data/NDVI_drought_monitoring")
pathDat <- "NDVI_Automation_Workflow/data_all"
pathMods <- "NDVI_Automation_Workflow/gam_models"

source("NDVI_Automation_Workflow/Baseline_Data_Models_Norms/0_Calculate_GAMM_Posteriors_Updated_Copy.R")
source("NDVI_Automation_Workflow/Baseline_Data_Models_Norms/0_Calculate_GAMM_Derivs_Copy.R")

#Reading in old data(fit to spline already) and new NDVI Data (not fit to spline yet)
ndviLatest <- read.csv(file.path(pathDat, "NDVIall_latest.csv"))
ndviNew <- read.csv(file.path(pathDat, "new_NDVI_data.csv"))

# previously processed files
ndvi.base <- read.csv(file.path(pathDat, "NDVIall_baseline_modeled.csv"))
ndviYrs <- read.csv(file.path(pathDat, "NDVIall_years_modeled.csv"))
ndvi.norms <- read.csv(file.path(pathDat, "NDVIall_normals_modeled.csv"))

ndviNew$date <- as.Date(ndviNew$date)
ndviNew$type <- as.factor(ndviNew$type)
ndviNew$mission <- as.factor(ndviNew$mission)
summary(ndviNew)
summary(ndviNew[ndviNew$type=="forest",])


ndviLatest$date <- as.Date(ndviLatest$date)
ndviLatest$type <- as.factor(ndviLatest$type)
ndviLatest$mission <- as.factor(ndviLatest$mission)
summary(ndviLatest)
summary(ndviLatest[ndviLatest$type=="forest" & !is.na(ndviLatest$NDVI),])


ndvi.base$date <- as.Date(ndvi.base$date)
ndvi.base$type <- as.factor(ndvi.base$type)
ndvi.base$mission <- as.factor(ndvi.base$mission)
summary(ndvi.base)

ndviYrs$type <- as.factor(ndviYrs$type)
ndviYrs <- ndviYrs[ndviYrs$year<max(ndviNew$year),] # Getting rid of the data for the year we're updating
summary(ndviYrs)


# ndviYrs$date <- as.Date(paste(ndviYrs$year, ndviYrs$yday, sep="-"))

################################################################
#Harmonizing data from Landsat 8 & 9 for new_NDVI_data, then joining it with existing data

######################
#formatting latest NDVI data
######################
# for each landcover type
# 1. correct for the satellite
# 1.a. figure our the resid now
# 2. re-fit the year spline
yrNow <- max(ndviNew$year)
if(max(ndviNew$yday[ndviNew$yday<30])) yrNow <- yrNow - 1 # Getting weird if we haven't finished last year!
ydayMax <- max(ndviNew$yday[ndviNew$year==yrNow])

ndviNew <- ndviNew[ndviNew$year==yrNow,]
yrNew <- data.frame(yday=rep(1:ydayMax), year=rep(yrNow, each=ydayMax), type=rep(unique(ndviNew$type), each=ydayMax*length(yrNow)), YrMean=NA, YrLwr=NA, YrUpr=NA)

kYr <- max(round(ydayMax/30)+2,3) # use 3 or the number of months we have plus 2: one for prev. Dec & 1 for added current-year flexibility

for(LC in unique(ndviNew$type)){
  rowsLC <- which(ndviNew$type==LC)
  
  # 1. correct for the satellite
  # 1.a. load our satellite model
  lcPull <- ifelse(LC=="forest", "forest-wet", "forest")
  gamLC <- readRDS(file.path(pathMods, paste0("GAM-Mission_", lcPull, ".RDS")))
  summary(gamLC)

  # 1.b. figure our the resid now
  ndviNew[rowsLC, "NDVIMissionPred"] <- predict(gamLC, newdata=ndviNew[rowsLC,])
  ndviNew[rowsLC, "MissionResid"] <- ndviNew$NDVI[rowsLC] - ndviNew$NDVIMissionPred[rowsLC]

  # 1.c. Now reprojecting
  ndviLCDupe <- ndviNew[rowsLC,]
  ndviLCDupe$mission <- "landsat 8"

  ndviNew[rowsLC, "ReprojPred"] <- predict(gamLC, newdata=ndviLCDupe)
  ndviNew[rowsLC, "NDVIReprojected"] <- ndviNew$MissionResid[rowsLC] + ndviNew$ReprojPred[rowsLC]

  # Now add this to our "base" data so we have it
  ndvi.base <- rbind(ndvi.base, ndviNew[rowsLC,])

  # 2. re-fit the year spline
  datTmp <- ndvi.base[ndvi.base$type==LC & ndvi.base$year==yrNow,]
  datTmpDec <- ndvi.base[ndvi.base$type==LC & ndvi.base$year==yrNow-1 & ndvi.base$yday>=365-31,]
  datTmpDec$yday <- datTmpDec$yday-365
  
  datTmp <- rbind(datTmp, datTmpDec)
  
  gamYRs <-gam(NDVIReprojected ~ s(yday, k=kYr)  , data=datTmp)
  # plot(gamYRs)
  YRpost <- post.distns(model.gam = gamYRs, newdata = yrNew[yrNew$type==LC & yrNew$year==yrNow,], vars=c("yday", "year"))
  # summary(YRpost)
  yrNew[yrNew$type==LC & yrNew$year==yrNow, c("YrMean", "YrLwr", "YrUpr")] <- YRpost[,c("mean", "lwr", "upr")]
  YrPostDeriv <- calc.derivs(model.gam = gamYRs, newdata = yrNew[yrNew$type==LC & yrNew$year==yrNow, ], vars=c("yday"))
  yrNew[yrNew$type==LC & yrNew$year==yrNow, c("YrDerivMean", "YrDerivLwr", "YrDerivUpr")] <- YrPostDeriv[,c("mean", "lwr", "upr")]
  
  # 3.3 add tag on trend
  yrNew[yrNew$type==LC & yrNew$year==yrNow & yrNew$YrDerivUpr<0, "YrDerivTrend"] <- "Getting Browner"
  yrNew[yrNew$type==LC & yrNew$year==yrNow & yrNew$YrDerivLwr>0, "YrDerivTrend"] <- "Getting Greener"
  yrNew[yrNew$type==LC & yrNew$year==yrNow & yrNew$YrDerivLwr<0 & yrNew$YrDerivUpr>0, "YrDerivTrend"] <- "No Change"
  summary(yrNew)
  
  
  saveRDS(gamYRs, file.path(pathMods, LC, paste0("GAM-Years-Baseline_", LC, "_", yrNow, ".RDS")))
  
  # 4. compare years to norms to flag -- could probably do this wihtout a loop, but looping to be safe
  # NDVI Flags
  #   Significantly Browner than norm = yr CI < norm CI
  #   Slightly Browner than norm = yr mean < norm CI, but yr CI overlaps
  #   Normal = yr mean in norm CI
  #   Slightly Greener than norm = yr mean > norm CI, but yr CI overlaps
  #   Significantly Greener than norm = year CI > norm CI
  # Trend Flag
  #   Greening faster than normal -- both yr & norm pos; yr > norm
  #   Greening slower than normal -- norm pos &  yr not neg; abs(yr < norm)
  #   Browning faster than normal -- both yr & norm neg; yr < norm
  #   Browning slower than normal -- norm neg & yr not pos; abs(yr < norm)
  #   Abnormal greening - yr pos & norm not pos
  #   Abnormal browning - yr neg & norm not neg
  for(YDAY in unique(yrNew$yday[yrNew$type==LC & yrNew$year==yrNow])){
    rowInd <- which(yrNew$type==LC & yrNew$year==yrNow & yrNew$yday==YDAY)
    Norm <- ndvi.norms[ndvi.norms$type==LC & ndvi.norms$yday==YDAY,]
    # yrNew[rowInd,]
    
    ndviFlag <- ifelse(yrNew$YrLwr[rowInd] > Norm$NormUpr, "Significantly Greener than Normal",
                       ifelse(yrNew$YrUpr[rowInd] < Norm$NormLwr, "Significantly Browner than Normal",
                              ifelse(yrNew$YrMean[rowInd] > Norm$NormUpr, "Slightly Greener than Normal",
                                     ifelse(yrNew$YrMean[rowInd] < Norm$NormLwr, "Slightly Browner than Normal",
                                            "Normal"))))
    
    
    trendFlag <- ifelse(yrNew$YrDerivLwr[rowInd] > Norm$NormDerivUpr & 
                          all(c(yrNew$YrDerivLwr[rowInd], Norm$NormDerivLwr)>0), "Greening Faster than Normal",
                        ifelse(yrNew$YrDerivUpr[rowInd] < Norm$NormDerivLwr & 
                                 all(c(yrNew$YrDerivUpr[rowInd], Norm$NormDerivLwr)>0), "Greening Slower than Normal",
                               ifelse(yrNew$YrDerivUpr[rowInd] < Norm$NormDerivLwr & 
                                        all(c(yrNew$YrDerivUpr[rowInd], Norm$NormDerivUpr)<0), "Browning Faster than Normal",
                                      ifelse(yrNew$YrDerivUpr[rowInd] > Norm$NormDerivLwr & 
                                               all(c(yrNew$YrDerivLwr[rowInd], Norm$NormDerivUpr)<0), "Browning Slower than Normal",
                                             ifelse(yrNew$YrDerivLwr[rowInd] > 0 & 
                                                      ((Norm$NormDerivUpr>0 & Norm$NormDerivLwr<0) | 
                                                         (Norm$NormDerivUpr<0 & Norm$NormDerivLwr<0)), "Abnormal Greening",
                                                    ifelse(yrNew$YrDerivUpr[rowInd] < 0 & 
                                                             ((Norm$NormDerivUpr>0 & Norm$NormDerivLwr<0) | 
                                                                (Norm$NormDerivUpr>0 & Norm$NormDerivLwr>0)), "Abnormal Browning",
                                                           "Normal"))))))
    
    yrNew[rowInd,c("FlagNDVI", "FlagTrend")] <- c(ndviFlag, trendFlag)
    
  }# end YDAY loop
  
}
summary(ndvi.base)
summary(ndviYrs)
summary(yrNew)

summary(ndvi.base[ndvi.base$type=="forest" & !is.na(ndvi.base$NDVIReprojected),]) # Has forest
summary(yrNew[yrNew$type=="forest",]) # Has forest

# add the new obs in here; note because we got rid of the old data in the ndviYrs, we need to add it back here
ndvi.newAgg <- aggregate(NDVIReprojected ~ type + year + yday + date, data=ndvi.base[ndvi.base$year==yrNow,], FUN=mean, na.rm=T)
summary(ndvi.newAgg)
dim(ndvi.newAgg)

dim(yrNew)
yrNew <- merge(yrNew, ndvi.newAgg[!is.na(ndvi.newAgg$NDVIReprojected),c("type", "year", "yday", "NDVIReprojected")], all.x=T)
dim(yrNew)

# merge any new points
dim(ndviYrs)
ndviYrs <- rbind(ndviYrs, yrNew)
dim(ndviYrs)

summary(ndvi.base[ndvi.base$type=="forest",])
summary(ndviYrs[ndviYrs$type=="forest",])

# 1.c. 
write.csv(ndvi.base, file.path(pathDat, "NDVIall_baseline_modeled.csv"), row.names=F)
write.csv(ndviYrs, file.path(pathDat, "NDVIall_years_modeled.csv"), row.names=F)

pushData=T
print("Data Adjusted & saved!")

