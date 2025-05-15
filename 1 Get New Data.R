# source("NDVI_Automation_Workflow/Baseline_Data_Models_Norms/0_Calculate_GAMM_Derivs_Copy.R")
# source("NDVI_Automation_Workflow/Baseline_Data_Models_Norms/0_Calculate_GAMM_Posteriors_Updated_Copy.R")
# library(rgee);
# # ee_check() # For some reason, it's important to run this before initializing right now
# # user.ee <- "jgarcia@mortonarb.org"
# user.ee <- "crollinson@mortonarb.org"
# 
# rgee::ee_Initialize(user =user.ee, drive=T, project = "urbanecodrought")
pushData <- F # Put these here so we don't push data if we don't have anything new


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

dir(file.path(path.google, NDVIsave, ".."), NDVIsave)

if(!all(flook == "none")){
  while(length(grep(strToday, dir(file.path(path.google, NDVIsave)))) < length(flook[!flook=="none"]) & Sys.time()<timeEnd){
    if (length(dir(file.path(path.google, NDVIsave, ".."), NDVIsave))>1){
      dirsDupe <- dir(file.path(path.google, NDVIsave, ".."), NDVIsave)
      dirsDupe <- dirsDupe[dirsDupe!=NDVIsave]
      
      for(i in 1:length(dirsDupe)){
        fCP <- dir(file.path(path.google, NDVIsave, "..", dirsDupe[i]))

        for(j in seq_along(fCP)){
          file.copy(from=file.path(path.google, NDVIsave, "..", dirsDupe[i], fCP[j]), to=file.path(path.google, NDVIsave, fCP[j]), overwrite=T, copy.mode=T)
          file.remove(file.path(path.google, NDVIsave, "..", dirsDupe[i], fCP[j]))
        }
        file.remove(file.path(file.path(path.google, NDVIsave, ".."), dirsDupe[i]))
      }
      # file.copy(from=file.path(path.google, NDVIsave, fileL8), to=file.path(pathShare, fileL8), overwrite=T, copy.mode=T)
    }
    
    print("waiting 60 sec")
    Sys.sleep(60) # Wait 60 seconds before checking again
    # flook <- unlist(filesCheck)
    
  }
} 

if(all(flook=="none")){
  print("No new data")
  
} else {
  # Execute next steps
  source("NDVI_Automation_Workflow/New_Data_Models_Norms/04_Processing_New_Data.R")
  source("NDVI_Automation_Workflow/New_Data_Models_Norms/05_Year_Specific_Adjustments.R")
  
  if(pushData){
    source("Deploy_Drought_App.R")
  }
}

