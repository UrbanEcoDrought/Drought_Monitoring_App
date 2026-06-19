# 1b_Process_New_Data.R
# Step 2 of the split daily workflow: run the analysis once the GEE exports submitted
# by 1a_Submit_GEE_Query.R have finished exporting and synced down via Google Drive.
# Scheduled (via a separate launchd job) ~30 minutes after 1a. Needs NO GEE auth —
# scripts 04/05/06 read the synced CSVs from the local Drive folder.

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
log_msg("=== Process job (1b) started ===")

pushData <- F # Put this here so we don't push data if we don't have anything new

path.google <- "/Users/crollinson//Google Drive/My Drive/"
NDVIsave <- ("UrbanEcoDrought_NDVI_LocalExtract-RAW")

# ----------------------------------------------------------------------------------
# 1. Read the state written by the submit job (1a)
# ----------------------------------------------------------------------------------
state_file <- file.path("NDVI_Automation_Workflow/data_all", "submit_state.rds")
if(!file.exists(state_file)) {
  log_msg("ERROR: submit_state.rds not found. Did the submit job (1a) run? Stopping.")
  warning("submit_state.rds not found — submit job (1a) may not have run. Stopping.")
  quit(save = "no", status = 0)
}

state <- readRDS(state_file)
filesCheck <- state$filesCheck
flook      <- state$flook
strToday   <- state$strToday

# Guard against stale state (submit job didn't run today, or already processed)
todayStr <- gsub("-", "_", Sys.Date())
if(is.null(strToday) || strToday != todayStr) {
  log_msg(paste("WARNING: submit_state.rds is stale (state date", strToday,
                "!= today", todayStr, "). Submit job (1a) may not have run today. Stopping."))
  warning("submit_state.rds is from a different day — submit job (1a) may not have run today. Stopping.")
  quit(save = "no", status = 0)
}

# ----------------------------------------------------------------------------------
# 1b. Already-processed guard — this job runs several times each morning (1:30/3/5/7)
#     so a late Drive sync still gets caught. Once a run succeeds it drops a marker;
#     later runs that morning see it and exit quietly instead of re-running 04 (which
#     would otherwise stop() with "No new NDVI data found" and log a scary ERROR).
# ----------------------------------------------------------------------------------
processed_marker <- file.path("NDVI_Automation_Workflow/data_all", "processed_state.rds")
if(file.exists(processed_marker)){
  pm <- readRDS(processed_marker)
  if(!is.null(pm$date) && pm$date == todayStr){
    log_msg("Today's data was already processed on an earlier run. Nothing to do.")
    log_msg("=== Process job (1b) finished ===")
    quit(save = "no", status = 0)
  }
}

# ----------------------------------------------------------------------------------
# 2. No-new-data case
# ----------------------------------------------------------------------------------
if(all(flook == "none")){
  print("No new data")
  log_msg("No new data found (per submit job). Workflow stopped.")
  log_msg("=== Process job (1b) finished ===")
  quit(save = "no", status = 0)
}

# ----------------------------------------------------------------------------------
# 3. Readiness check — confirm today's expected files have synced & are readable
#    (single pass, not a blocking loop). If anything is missing, abort with a warning
#    rather than letting script 04 silently reprocess an older file.
# ----------------------------------------------------------------------------------

# Fix duplicate Drive directories first (Google Drive sometimes makes "...(1)" copies)
if (length(dir(file.path(path.google, NDVIsave, ".."), NDVIsave)) > 1) {
  message("multiple NDVIsave directories found")
  dirsDupe <- dir(file.path(path.google, NDVIsave, ".."), NDVIsave)
  dirsDupe <- dirsDupe[dirsDupe != NDVIsave]
  for(i in seq_along(dirsDupe)){
    fCP <- dir(file.path(path.google, NDVIsave, "..", dirsDupe[i]))
    # Move files from extra directories to the right one
    for(j in seq_along(fCP)){
      file.copy(from=file.path(path.google, NDVIsave, "..", dirsDupe[i], fCP[j]), to=file.path(path.google, NDVIsave, fCP[j]), overwrite=T, copy.mode=T)
      file.remove(file.path(path.google, NDVIsave, "..", dirsDupe[i], fCP[j]))
    } # end j loop
    file.remove(file.path(file.path(path.google, NDVIsave, ".."), dirsDupe[i]))
  }
} # End dupe fix

# Build the list of expected file prefixes (one per mission/landcover that 03 exported)
expected_prefixes <- character(0)
if(!all(filesCheck$landsat8 == "none")) {
  expected_prefixes <- c(expected_prefixes, paste0("Landsat8_", filesCheck$landsat8[filesCheck$landsat8 != "none"]))
}
if(!all(filesCheck$landsat9 == "none")) {
  expected_prefixes <- c(expected_prefixes, paste0("Landsat9_", filesCheck$landsat9[filesCheck$landsat9 != "none"]))
}

# A file is "ready" only when it matches the prefix AND today's date AND we can read at
# least a header + one data row. Google Drive can list a file (with full cloud metadata)
# before the local content is hydrated, so a size check alone is not enough.
file_ready <- function(prefix) {
  hits <- dir(file.path(path.google, NDVIsave), pattern = prefix)
  hits <- hits[grepl(strToday, hits)]
  if(length(hits) == 0) return(FALSE)
  paths <- file.path(path.google, NDVIsave, hits)
  any(vapply(paths, function(f) {
    lines <- tryCatch(suppressWarnings(readLines(f, n = 2)), error = function(e) character(0))
    length(lines) >= 2
  }, logical(1)))
}

ready <- vapply(expected_prefixes, file_ready, logical(1))
missing <- expected_prefixes[!ready]

if(length(missing) > 0) {
  msg <- paste0("WARNING: ", length(missing), " of ", length(expected_prefixes),
                " expected files for ", strToday, " are not yet synced/readable: ",
                paste(missing, collapse = ", "),
                ". Stopping without running 04/05/06 to avoid reprocessing stale data.")
  log_msg(msg)
  warning(msg)
  quit(save = "no", status = 0)
}

log_msg(paste("All", length(expected_prefixes), "expected files present and readable. Running processing pipeline."))

# ----------------------------------------------------------------------------------
# 4. Run the processing pipeline
# ----------------------------------------------------------------------------------
print("Running next steps!")

tryCatch({
  source("NDVI_Automation_Workflow/New_Data_Models_Norms/04_Processing_New_Data.R")
  log_msg("Script 04 (Processing New Data) completed successfully")
}, error = function(e) {
  log_msg(paste("ERROR in script 04 (Processing New Data):", e$message))
  stop(e)
})

tryCatch({
  source("NDVI_Automation_Workflow/New_Data_Models_Norms/05_Year_Specific_Adjustments.R")
  log_msg("Script 05 (Year Specific Adjustments) completed successfully")
}, error = function(e) {
  log_msg(paste("ERROR in script 05 (Year Specific Adjustments):", e$message))
  stop(e)
})

tryCatch({
  source("NDVI_Automation_Workflow/New_Data_Models_Norms/06_Optional_CheckFigs.R")
  log_msg("Script 06 (Check Figs) completed successfully")
}, error = function(e) {
  log_msg(paste("ERROR in script 06 (Check Figs):", e$message))
  stop(e)
})

if(pushData){
  tryCatch({
    source("Deploy_Drought_App.R")
    log_msg("Deploy_Drought_App.R completed successfully")
  }, error = function(e) {
    log_msg(paste("ERROR in Deploy_Drought_App.R:", e$message))
    stop(e)
  })
}

# Mark today as done so later retry runs this morning exit quietly.
saveRDS(list(date = todayStr, processed = Sys.time()), processed_marker)

log_msg("=== Process job (1b) completed successfully ===")
