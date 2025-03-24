# source("NDVI_Automation_Workflow/Baseline_Data_Models_Norms/0_Calculate_GAMM_Derivs_Copy.R")
# source("NDVI_Automation_Workflow/Baseline_Data_Models_Norms/0_Calculate_GAMM_Posteriors_Updated_Copy.R")

path.google <- "~/Google Drive/My Drive/"
# path.google.share <- "~/Google Drive/Shared drives/Urban Ecological Drought/"
# assetHome <- ee_get_assethome()
NDVIsave <- ("UrbanEcoDrought_NDVI_LocalExtract-RAW")
lcnames <- c("forest", "crop", "grassland", "urban-high", "urban-medium", "urban-low", "urban-open")

# Start the Landsat Data extraction
source("NDVI_Automation_Workflow/New_Data_Models_Norms/03_Pulling_New_Landsat_Data.R")


# Check for new files to be donel; wait up to one hour
flook <- unlist(filesCheck)
strToday <- gsub("-", "_", Sys.Date())
timeEnd <- Sys.time() + 60*30 # Give things 30 minutes to finish

while(!all(flook == "none") |  length(grep(strToday, dir(file.path(path.google, NDVIsave)))) < length(flook[!flook=="none"]) | Sys.time()<timeEnd){
  Sys.sleep(60) # Wait 60 seconds before checking again
}

if(all(flook)=="none"){
  print("No new data")
  stop()
} else {
  # Execute next steps
  source("NDVI_Automation_Workflow/New_Data_Models_Norms/04_Processing_New_Data.R")
  source("NDVI_Automation_Workflow/New_Data_Models_Norms/05_Year_Specific_Adjustments.R")
  
  if(pushData){
    source("Deploy_Drought_App.R")
  }
}

