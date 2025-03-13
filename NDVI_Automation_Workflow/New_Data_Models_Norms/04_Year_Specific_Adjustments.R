####################################################################################################################
#Purpose of Script: Readjust year specific spline using models & norms from Baseline_Data_Models_Norms Workflow
# Original scripts written by Christy Rollinson and Juliana Harr, workflow put together by Jocelyn Garcia
####################################################################################################################

library(mgcv) #load packages
library(ggplot2)
library(tibble)
library(dplyr)
library(MASS)

#file paths
Sys.setenv(GOOGLE_DRIVE = "~/Google Drive/Shared drives/Urban Ecological Drought")
google.drive <- Sys.getenv("GOOGLE_DRIVE")
# path.google <- ("/Users/jocelyngarcia/Library/CloudStorage/GoogleDrive-jgarcia@mortonarb.org")
path.google <- "~/Google Drive/"
pathShare <- file.path(path.google, "Shared drives/Urban Ecological Drought/data/NDVI_drought_monitoring")
pathDat <- "../data_all"
pathMods <- "../gam_models"

source("../Baseline_Data_Models_Norms/0_Calculate_GAMM_Posteriors_Updated_Copy.R")
# source("~/Documents/GitHub/NDVI_drought_monitoring/0_Calculate_GAMM_Posteriors_Updated_Copy.R")
# source("~/Documents/GitHub/NDVI_drought_monitoring/0_Calculate_GAMM_Derivs_Copy.R")

#Reading in old data(fit to spline already) and new NDVI Data (not fit to spline yet)
ndviLatest <- read.csv("../data_all/NDVIall_latest.csv")
ndviNew <- read.csv("../data_all/new_NDVI_data.csv")

# previously processed files
ndvi.base <- read.csv(file.path(pathDat, "NDVIall_baseline_modeled.csv"))
ndviYrs <- read.csv("../data_all/NDVIall_years_modeled.csv")

ndviNew$date <- as.Date(ndviNew$date)
ndviNew$type <- as.factor(ndviNew$type)
ndviNew$mission <- as.factor(ndviNew$mission)
summary(ndviNew)

ndviLatest$date <- as.Date(ndviLatest$date)
ndviLatest$type <- as.factor(ndviLatest$type)
ndviLatest$mission <- as.factor(ndviLatest$mission)
summary(ndviLatest)


ndvi.base$date <- as.Date(ndvi.base$date)
ndvi.base$type <- as.factor(ndvi.base$type)
ndvi.base$mission <- as.factor(ndvi.base$mission)
summary(ndviLatest)

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
ydayMax <- max(ndviNew$yday)
yrNew <- data.frame(yday=rep(1:ydayMax), year=rep(yrNow, each=ydayMax), type=rep(unique(ndviNew$type), each=ydayMax*length(yrNow)), YrMean=NA, YrLwr=NA, YrUpr=NA)

kYr <- max(round(ydayMax/30),3) # use 3 or the number of months we have

for(LC in unique(ndviNew$type)){
  rowsLC <- which(ndviNew$type==LC)
  
  # 1. correct for the satellite
  # 1.a. load our satellite model
  gamLC <- readRDS(file.path(pathMods, paste0("GAM-Mission_", LC, ".RDS")))
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
  gamYRs <-gam(NDVIReprojected ~ s(yday, k=kYr)  , data=ndvi.base[ndvi.base$type==LC & ndvi.base$year==yrNow,])
  # plot(gamYRs)
  YRpost <- post.distns(model.gam = gamYRs, newdata = yrNew[yrNew$type==LC & yrNew$year==yrNow,], vars=c("yday", "year"))
  # summary(YRpost)
  yrNew[yrNew$type==LC & yrNew$year==yrNow, c("YrMean", "YrLwr", "YrUpr")] <- YRpost[,c("mean", "lwr", "upr")]
  saveRDS(gamYRs, file.path(pathMods, LC, paste0("GAM-Years-Baseline_", LC, "_", yrNow, ".RDS")))
  
  ndviYrs <- rbind(ndviYrs, yrNew[yrNew$type==LC & yrNew$year==yrNow,])
}
summary(ndvi.base)
summary(ndviYrs)
# 1.c. 
write.csv(ndvi.base, file.path(pathDat, "NDVIall_baseline_modeled.csv"), row.names=F)
write.csv(ndviYrs, file.path(pathDat, "NDVIall_years_modeled.csv"), row.names=F)




# saveRDS(gamLC, file.path(pathMods, paste0("GAM-Mission_", LC, ".RDS")))



# ################################################################
# #Compare NDVI & Derivative for current year spline with norm
# current_year <- format(Sys.Date(), "%Y")
# 
# # Ensure nmonths is defined
# if (!exists("nmonths") || !is.numeric(nmonths)) {
#   nmonths <- 12
# }
# 
# # Pull NDVI data for the current year
# current_year_NDVI <- updated_NDVI_data[updated_NDVI_data$year == current_year,]
# 
# # Initialize empty dataframe before the loop
# df <- data.frame()
# 
# for (LC in unique(updated_NDVI_data$type)) {
#   datLC <- updated_NDVI_data[updated_NDVI_data$type == LC,]
#   
#   for (yr in unique(datLC$year)) {
#     datyr <- datLC[datLC$year == yr,]
#     
#     # Ensure the GAM model runs correctly
#     if (yr == current_year) {
#       gamyr <- gam(NDVIReprojected ~ s(yday, k=nmonths), data=datyr)
#     } else {
#       gamyr <- gam(NDVIReprojected ~ s(yday, k=12), data=datyr)
#     }
#     
#     # Compute derivatives and check output
#     derivs <- calc.derivs(model.gam=gamyr, newdata=newDF, vars="yday")
#     
#     if (!is.data.frame(derivs)) {
#       stop("❌ `calc.derivs()` is not returning a data frame! Check function output.")
#     }
#     
#     derivs$type <- LC
#     derivs$year <- yr
#     
#     df <- rbind(df, derivs)
#   }
# }
# 
# # Now filter for the current year (AFTER the loop)
# df <- df[df$year == current_year,]
# 
# # Save CSV
# write.csv(df, file.path(pathShare, "year_specific_deriv.csv"), row.names = FALSE)
# ################################################################