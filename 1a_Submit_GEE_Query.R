# 1a_Submit_GEE_Query.R
# Step 1 of the split daily workflow: submit the Google Earth Engine export tasks
# and exit. The GEE exports run asynchronously on Google's servers and then sync to
# the local Google Drive folder; the companion script 1b_Process_New_Data.R runs ~30
# minutes later (via a separate launchd job) to do the analysis once the files arrive.
# This replaces the in-process waitForFiles() loop, which kept timing out under launchd.
library(rgee)

# Ensure we're in the correct working directory
if (Sys.getenv("RSTUDIO") == "1") {
  script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
} else {
  # Running via Rscript (launchd, terminal) — get path from command args
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("--file=", args, value = TRUE)
  script_dir <- if (length(file_arg) > 0) dirname(normalizePath(sub("--file=", "", file_arg))) else ""
}
if (length(script_dir) == 0 || !nzchar(script_dir)) {
  script_dir <- getwd()
}
setwd(script_dir)

# Verify we're in the right place
if(!"NDVI_Automation_Workflow" %in% dir()) {
  stop("Could not find NDVI_Automation_Workflow directory. Check working directory.")
}

message(paste("Working directory set to:", getwd()))

# Simple logging helper — writes timestamped messages to workflow_log.txt
log_file <- file.path(getwd(), "workflow_log.txt")
log_msg <- function(msg) {
  cat(paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", msg, "\n"),
      file = log_file, append = TRUE)
}
log_msg("=== Submit job (1a) started ===")


# Add error handling for authentication
user.ee <- "crollinson@mortonarb.org"
tryCatch({
  rgee::ee_Initialize(user = user.ee, drive = F, project = "urbanecodrought")
  message("Google Earth Engine initialized successfully")
  log_msg("GEE initialized successfully")
}, error = function(e) {
  log_msg(paste("ERROR: Failed to initialize GEE:", e$message))
  stop(paste("Failed to initialize Google Earth Engine:", e$message))
})

NDVIsave <- ("UrbanEcoDrought_NDVI_LocalExtract-RAW")

# Start the Landsat Data extraction
# Initialize filesCheck before use
filesCheck <- list(landsat8 = character(0), landsat9 = character(0))

source("NDVI_Automation_Workflow/New_Data_Models_Norms/03_Pulling_New_Landsat_Data.R")

# 03 submits the GEE export tasks and populates filesCheck with the landcover types
# exported for each mission (or "none" if there was no new data for that mission).
flook <- unlist(filesCheck)
strToday <- gsub("-", "_", Sys.Date())

# Persist state so the process job (1b) knows what to expect without re-querying GEE.
state_file <- file.path("NDVI_Automation_Workflow/data_all", "submit_state.rds")
saveRDS(list(filesCheck = filesCheck,
             flook = flook,
             strToday = strToday,
             submitted = Sys.time()),
        state_file)

if(all(flook == "none")){
  log_msg("Submit job complete: no new data found. Process job (1b) will stop.")
} else {
  log_msg(paste("Submit job complete: export tasks submitted for",
                sum(flook != "none"), "landcover/mission combinations.",
                "Process job (1b) should run after Drive sync."))
}
log_msg("=== Submit job (1a) finished ===")
