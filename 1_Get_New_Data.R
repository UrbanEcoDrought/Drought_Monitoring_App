# Add to beginning of main script (1 Get New Data.R)
# Ensure we're in the correct working directory
# pathLocal <- "/Users/crollinson//Desktop/Research/UrbanDrought/Drought_Monitoring_App/"
# setwd(pathLocal)


script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
if(length(script_dir) == 0 || script_dir == "") {
  # Fallback for non-interactive execution
  script_dir <- getwd()
}
setwd(script_dir)

# Verify we're in the right place
if(!"NDVI_Automation_Workflow" %in% dir()) {
  stop("Could not find NDVI_Automation_Workflow directory. Check working directory.")
}

message(paste("Working directory set to:", getwd()))


library(rgee); 

# Add error handling for authentication
user.ee <- "crollinson@mortonarb.org"
tryCatch({
  rgee::ee_Initialize(user = user.ee, drive = T, project = "urbanecodrought")
  message("Google Earth Engine initialized successfully")
}, error = function(e) {
  stop(paste("Failed to initialize Google Earth Engine:", e$message))
})

pushData <- F # Put these here so we don't push data if we don't have anything new

path.google <- "/Users/crollinson//Google Drive/My Drive/"
# path.google.share <- "~/Google Drive/Shared drives/Urban Ecological Drought/"
# assetHome <- ee_get_assethome()
NDVIsave <- ("UrbanEcoDrought_NDVI_LocalExtract-RAW")

# Start the Landsat Data extraction
# Initialize filesCheck before use
filesCheck <- list(landsat8 = character(0), landsat9 = character(0))

source("NDVI_Automation_Workflow/New_Data_Models_Norms/03_Pulling_New_Landsat_Data.R")

lcnames <- c("forest", "crop", "grassland", "urban-high", "urban-medium", "urban-low", "urban-open")

# Check for new files to be donel; wait up to one hour
flook <- unlist(filesCheck)
strToday <- gsub("-", "_", Sys.Date())
timeEnd <- Sys.time() + 60*30 # Give things 30 minutes to finish

# dir(file.path(path.google, NDVIsave, ".."), NDVIsave)
# dir(file.path(path.google, NDVIsave))

waitForFiles <- function(expected_files, max_wait_minutes = 30) {
  timeout <- Sys.time() + (max_wait_minutes * 60)
  
  while(Sys.time() < timeout) {
    
    if (length(dir(file.path(path.google, NDVIsave, ".."), NDVIsave))>1){
      dirsDupe <- dir(file.path(path.google, NDVIsave, ".."), NDVIsave)
      dirsDupe <- dirsDupe[dirsDupe!=NDVIsave]
      
      # Fixing dupe dirs
      for(i in 1:length(dirsDupe)){
        fCP <- dir(file.path(path.google, NDVIsave, "..", dirsDupe[i]))
        
        # Move files from extra directories to the right one
        for(j in seq_along(fCP)){
          file.copy(from=file.path(path.google, NDVIsave, "..", dirsDupe[i], fCP[j]), to=file.path(path.google, NDVIsave, fCP[j]), overwrite=T, copy.mode=T)
          file.remove(file.path(path.google, NDVIsave, "..", dirsDupe[i], fCP[j]))
        }
        file.remove(file.path(file.path(path.google, NDVIsave, ".."), dirsDupe[i]))
      }
    
    existing_files <- dir(file.path(path.google, NDVIsave))
    expected_pattern <- paste0(strToday, "_")
    
    matching_files <- length(grep(expected_pattern, existing_files))
    
    if(matching_files >= length(expected_files[expected_files != "none"])) {
      message(paste("Found", matching_files, "expected files"))
      return(TRUE)
    } # end matching files good
    
    message("Waiting for files... Found:", matching_files, "Expected:", length(expected_files[expected_files != "none"]))
    Sys.sleep(60)
  } # End check for dupes
  
  warning(paste("Timeout reached. Only found", matching_files, "of", length(expected_files[expected_files != "none"]), "expected files"))
  return(FALSE)
  } # End while
} # End function

if(!all(flook == none)){
  files_ready <- waitForFiles(flook)
  if(!files_ready){
    warning("Not all expected files were created within timeout period")
  }
}

  
if(all(flook=="none")){
  print("No new data")
  
} else {
  print("Running next steps!")
  # Execute next steps
  source("NDVI_Automation_Workflow/New_Data_Models_Norms/04_Processing_New_Data.R")
  source("NDVI_Automation_Workflow/New_Data_Models_Norms/05_Year_Specific_Adjustments.R")
  source("NDVI_Automation_Workflow/New_Data_Models_Norms/06_Optional_CheckFigs.R")
  
  if(pushData){
    source("Deploy_Drought_App.R")
  }
}
  

