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
path.google <- ("/Users/jocelyngarcia/Library/CloudStorage/GoogleDrive-jgarcia@mortonarb.org")
pathShare <- file.path(path.google, "Shared drives/Urban Ecological Drought/data/NDVI_drought_monitoring")


#Loading in models & norms
load("/Users/jocelyngarcia/Documents/GitHub/UrbanDrought_SpatialAnalysis_Chicago/NDVI_Automation_Workflow/Baseline_Data_Models_Norms/gam_models.RData")
load("/Users/jocelyngarcia/Documents/GitHub/UrbanDrought_SpatialAnalysis_Chicago/NDVI_Automation_Workflow/Baseline_Data_Models_Norms/norms.RData")

#Reading in old data(fit to spline already) and new NDVI Data (not fit to spline yet)
new_NDVI_data <-read_csv("/Users/jocelyngarcia/Library/CloudStorage/GoogleDrive-jgarcia@mortonarb.org/Shared drives/Urban Ecological Drought/data/UrbanEcoDrought_NDVI_LocalExtract/new_NDVI_data.csv")
GAM_fit_NDVI<-read_csv("/Users/jocelyngarcia/Library/CloudStorage/GoogleDrive-jgarcia@mortonarb.org/Shared drives/Urban Ecological Drought/data/UrbanEcoDrought_NDVI_LocalExtract/allNDVI_data.csv")

################################################################
#Harmonizing data from Landsat 8 & 9 for new_NDVI_data, then joining it with existing data

######################
#formatting latest NDVI data
######################

new_NDVI_data$date <- as.Date(new_NDVI_data$date)
new_NDVI_data$type <- as.factor(new_NDVI_data$type)
new_NDVI_data$mission <- as.factor(new_NDVI_data$mission)

######################
#crop
######################

ndvicropNew=new_NDVI_data[new_NDVI_data$type=="crop",]
# Going to "reproject" the predicted mean/normal
ndvicropDupe <- ndvicropNew
ndvicropDupe$mission <- "landsat 8"

ndvicropNew$ReprojPred <- predict(gamcrop, newdata=ndvicropDupe)
ndvicropNew$NDVIReprojected <- ndvicrop$MissionResid + ndvicrop$ReprojPred

summary(ndvicropNew)

######################
#forest
######################

ndviforestNew =new_NDVI_data[new_NDVI_data$type=="forest",]
# Going to "reproject" the predicted mean/normal
ndviforestDupe <- ndviforestNew
ndviforestDupe$mission <- "landsat 8"

ndviforestNew$ReprojPred <- predict(gamforest, newdata=ndviforestDupe)
ndviforestNew$NDVIReprojected <- ndviforest$MissionResid + ndviforest$ReprojPred
summary(ndviforestNew)

######################
#grassland
######################

ndvigrassNew <- new_NDVI_data[new_NDVI_data$type=="grassland",]
# Going to "reproject" the predicted mean/normal
ndvigrassDupe <- ndvigrassNew
ndvigrassDupe$mission <- "landsat 8"

ndvigrassNew$ReprojPred <- predict(gamgrass, newdata=ndvigrassDupe)
ndvigrassNew$NDVIReprojected <- ndvigrass$MissionResid + ndvigrass$ReprojPred
summary(ndvigrassNew)

######################
#urban-high
######################

ndviUrbHighNew <- new_NDVI_data[new_NDVI_data$type=="urban-high",]
# Going to "reproject" the predicted mean/normal
ndviUrbHighDupe <- ndviUrbHighNew
ndviUrbHighDupe$mission <- "landsat 8"

ndviUrbHighNew$ReprojPred <- predict(gamUrbHigh, newdata=ndviUrbHighDupe)
ndviUrbHighNew$NDVIReprojected <- ndviUrbHigh$MissionResid + ndviUrbHigh$ReprojPred
summary(ndviUrbHighNew)

######################
#urban-medium
######################

ndviUrbMedNew <- new_NDVI_data[new_NDVI_data$type=="urban-medium",]
# Going to "reproject" the predicted mean/normal
ndviUrbMedDupe <- ndviUrbMedNew
ndviUrbMedDupe$mission <- "landsat 8"

ndviUrbMedNew$ReprojPred <- predict(gamUrbMed, newdata=ndviUrbMedDupe)
ndviUrbMedNew$NDVIReprojected <- ndviUrbMed$MissionResid + ndviUrbMed$ReprojPred
summary(ndviUrbMedNew)

######################
#urban-low
######################

ndviUrbLowNew <- new_NDVI_data[new_NDVI_data$type=="urban-low",]
# Going to "reproject" the predicted mean/normal
ndviUrbLowDupe <- ndviUrbLowNew
ndviUrbLowDupe$mission <- "landsat 8"

ndviUrbLowNew$ReprojPred <- predict(gamUrbLow, newdata=ndviUrbLowDupe)
ndviUrbLowNew$NDVIReprojected <- ndviUrbLow$MissionResid + ndviUrbLow$ReprojPred
summary(ndviUrbLowNew)

######################
#urban-open
######################

ndviUrbOpenNew <- new_NDVI_data[new_NDVI_data$type=="urban-open",]
# Going to "reproject" the predicted mean/normal
ndviUrbOpenDupe <- ndviUrbOpenNew
ndviUrbOpenDupe$mission <- "landsat 8"

ndviUrbOpenNew$ReprojPred <- predict(gamUrbOpen, newdata=ndviUrbOpenDupe)
ndviUrbOpenNew$NDVIReprojected <- ndviUrbOpen$MissionResid + ndviUrbOpen$ReprojPred
summary(ndviUrbOpenNew)

######################
#combine into one large dataframe & save
######################

new_gam_fit_data <- rbind(ndvicropNew, ndviforestNew, ndvigrassNew, ndviUrbHighNew, ndviUrbMedNew, ndviUrbLowNew, ndviUrbOpenNew)

updated_NDVI_data <- bind_rows(new_gam_fit_data, GAM_fit_NDVI) %>%
  group_by(across(-c(NDVIMissionPred, MissionResid, NDVIReprojected))) %>%
  filter(row_number() == 1 | 
           !is.na(NDVIMissionPred) | 
           !is.na(MissionResid) | 
           !is.na(NDVIReprojected)) %>%
  ungroup()



#Will overwrite old allNDVI_data.csv with updated joined data as allNDVI_data.csv - will be the data for the update distribution plot
write.csv(updated_NDVI_data, file.path(pathShare, "allNDVI_data.csv"), row.names = FALSE)
######################

################################################################
#Calculating year specific Spline

#Needed for code (From Juliana's Workflow)
newDF <- data.frame(yday=seq(1:365)) #create new data frame with column to represent day of year sequence
source("~/Documents/GitHub/NDVI_drought_monitoring/0_Calculate_GAMM_Posteriors_Updated_Copy.R")
source("~/Documents/GitHub/NDVI_drought_monitoring/0_Calculate_GAMM_Derivs_Copy.R")

######################
#loading in and filtering for data from current year
######################

# Order data by date in descending order
updated_NDVI_data <- updated_NDVI_data[order(as.Date(updated_NDVI_data$date, format="%Y-%m-%d"), decreasing = TRUE), ]

# Extract the latest year
latest_row <- head(updated_NDVI_data, 1)
current_year <-as.numeric(format(latest_row$date, "%Y"))
# Filter data for the current year
current_year_NDVI_data <- updated_NDVI_data %>% filter(format(date, "%Y") == as.character(current_year))


######################
#Setting Variable for k based on max yday
######################
#putting only current year's data in descending order
current_year_NDVI_data <- current_year_NDVI_data[order(as.Date(current_year_NDVI_data$date, format="%Y-%m-%d"), decreasing = TRUE), ]
# Extract the latest yday & saving as variable 
last_row <- head(current_year_NDVI_data, 1)
k_val <-(round(((last_row$yday)/30), 0))


######################
#crop
######################

gamcrop_year_specific <- gam(NDVIReprojected ~ s(yday, k= k_val), data=current_year_NDVI_data[current_year_NDVI_data$type=="crop",])
NDVIcrop_year_specific <- predict(gamcrop_year_specific, newdata=newDF) #normal crop values for a year
crop_year_specific <- post.distns(model.gam=gamcrop_year_specific, newdata=newDF, vars="yday")
crop_year_specific$type <- "crop"

######################
#forest
######################

gamforest_year_specific <- gam(NDVIReprojected ~ s(yday, k=k_val), data=current_year_NDVI_data[current_year_NDVI_data$type=="forest",])
NDVIforest_year_specific <- predict(gamforest_year_specific, newdata=newDF)
forest_year_specific <- post.distns(model.gam = gamforest_year_specific, newdata = newDF, vars="yday")
forest_year_specific$type <- "forest"

######################
#grassland
######################

gamgrass_year_specific <- gam(NDVIReprojected ~ s(yday, k= k_val), data=current_year_NDVI_data[current_year_NDVI_data$type=="grassland",])
NDVIgrass_year_specific <- predict(gamgrass_year_specific, newdata=newDF)
grass_year_specific <- post.distns(model.gam = gamgrass_year_specific, newdata = newDF, vars="yday")
grass_year_specific$type <- "grassland"
#grass_norm$NDVIpred <- NDVIgrass_norm

######################
#urban-high
######################

gamUrbHigh_year_specific <- gam(NDVIReprojected ~ s(yday, k= k_val), data=current_year_NDVI_data[current_year_NDVI_data$type=="urban-high",])
NDVIUrbHigh_year_specific <- predict(gamUrbHigh_year_specific, newdata=newDF)
UrbHigh_year_specific <- post.distns(model.gam = gamUrbHigh_year_specific, newdata = newDF, vars="yday")
UrbHigh_year_specific$type <- "urban-high"
#UrbHigh_norm$NDVIpred <- NDVIUrbHigh_norm

######################
#urban-medium
######################

gamUrbMed_year_specific <- gam(NDVIReprojected ~ s(yday, k= k_val), data=current_year_NDVI_data[current_year_NDVI_data$type=="urban-medium",])
NDVIUrbMed_year_specific <- predict(gamUrbMed_year_specific, newdata=newDF)
UrbMed_year_specific <- post.distns(model.gam = gamUrbMed_year_specific, newdata = newDF, vars="yday")
UrbMed_year_specific$type <- "urban-medium"
#UrbMed_norm$NDVIpred <- NDVIUrbMed_norm

######################
#urban-low
######################

gamUrbLow_year_specific <- gam(NDVIReprojected ~ s(yday, k= k_val), data=current_year_NDVI_data[current_year_NDVI_data$type=="urban-low",])
NDVIUrbLow_year_specific <- predict(gamUrbLow_year_specific, newdata=newDF)
UrbLow_year_specific <- post.distns(model.gam = gamUrbLow_year_specific, newdata = newDF, vars="yday")
UrbLow_year_specific$type <- "urban-low"
#UrbLow_norm$NDVIpred <- NDVIUrbLow_norm

######################
#urban-open
######################

gamUrbOpen_year_specific <- gam(NDVIReprojected ~ s(yday, k = k_val), data=current_year_NDVI_data[current_year_NDVI_data$type=="urban-open",])
NDVIUrbOpen_year_specific <- predict(gamUrbOpen_year_specific, newdata=newDF)
UrbOpen_year_specific <- post.distns(model.gam = gamUrbOpen_year_specific, newdata = newDF, vars="yday")
UrbOpen_year_specific$type <- "urban-open"
#UrbOpen_norm$NDVIpred <- NDVIUrbOpen_norm

######################
#combine into one large dataframe & save
######################

year_specifics <- rbind(crop_year_specific, forest_year_specific, grass_year_specific, UrbHigh_year_specific, UrbMed_year_specific, UrbLow_year_specific, UrbOpen_year_specific)
write.csv(year_specifics, file.path(pathShare, "year_specific_spline.csv"), row.names=F)
######################

################################################################
#Compare NDVI & Derivative for current year spline with norm
current_year <- format(Sys.Date(), "%Y")

# Ensure nmonths is defined
if (!exists("nmonths") || !is.numeric(nmonths)) {
  nmonths <- 12
}

# Pull NDVI data for the current year
current_year_NDVI <- updated_NDVI_data[updated_NDVI_data$year == current_year,]

# Initialize empty dataframe before the loop
df <- data.frame()

for (LC in unique(updated_NDVI_data$type)) {
  datLC <- updated_NDVI_data[updated_NDVI_data$type == LC,]
  
  for (yr in unique(datLC$year)) {
    datyr <- datLC[datLC$year == yr,]
    
    # Ensure the GAM model runs correctly
    if (yr == current_year) {
      gamyr <- gam(NDVIReprojected ~ s(yday, k=nmonths), data=datyr)
    } else {
      gamyr <- gam(NDVIReprojected ~ s(yday, k=12), data=datyr)
    }
    
    # Compute derivatives and check output
    derivs <- calc.derivs(model.gam=gamyr, newdata=newDF, vars="yday")
    
    if (!is.data.frame(derivs)) {
      stop("❌ `calc.derivs()` is not returning a data frame! Check function output.")
    }
    
    derivs$type <- LC
    derivs$year <- yr
    
    df <- rbind(df, derivs)
  }
}

# Now filter for the current year (AFTER the loop)
df <- df[df$year == current_year,]

# Save CSV
write.csv(df, file.path(pathShare, "year_specific_deriv.csv"), row.names = FALSE)
################################################################