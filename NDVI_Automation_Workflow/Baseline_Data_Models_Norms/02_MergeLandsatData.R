# path.google <- "/Users/jocelyngarcia/Library/CloudStorage/GoogleDrive-jgarcia@mortonarb.org"
path.google <- "~/Google Drive/"
pathShare <- file.path(path.google, "Shared drives", "Urban Ecological Drought", "data", "UrbanEcoDrought_NDVI_LocalExtract-RAW")
NDVIsave <- ("UrbanEcoDrought_NDVI_LocalExtract-RAW")
fout <- "../data_all"
if(!dir.exists(pathShare)) dir.create(pathShare, recursive=T)
if(!dir.exists(fout)) dir.create(fout, recursive=T)

# Check if files are being detected
fNDVI <- dir(file.path(path.google, "My Drive", NDVIsave))
print(fNDVI)  # Should list all files in that directory

day.labels <- data.frame(Date=seq.Date(as.Date("2023-01-01"), as.Date("2023-12-01"), by="month"))
day.labels$yday <- lubridate::yday(day.labels$Date)
day.labels$Text <- paste(lubridate::month(day.labels$Date, label=T), lubridate::day(day.labels$Date))
day.labels
summary(day.labels)

# Clunky code, but should pull the latest file
lcnames <- c("forest", "crop", "grassland", "urban-high", "urban-medium", "urban-low", "urban-open")

ndviAll <- data.frame()
for(LCTYPE in lcnames){
  fileL8 <- dir(file.path(path.google, "My Drive", NDVIsave), paste0("Landsat8_", LCTYPE, "_"))[length(dir(file.path(path.google, "My Drive", NDVIsave), paste0("Landsat8_", LCTYPE, "_")))]
  fileL9 <- dir(file.path(path.google, "My Drive", NDVIsave), paste0("Landsat9_", LCTYPE, "_"))[length(dir(file.path(path.google, "My Drive", NDVIsave), paste0("Landsat9_", LCTYPE, "_")))]
  fileL7 <- dir(file.path(path.google, "My Drive", NDVIsave), paste0("Landsat7_", LCTYPE, "_"))[length(dir(file.path(path.google, "My Drive", NDVIsave), paste0("Landsat7_", LCTYPE, "_")))]
  fileL5 <- dir(file.path(path.google, "My Drive", NDVIsave), paste0("Landsat5_", LCTYPE, "_"))[length(dir(file.path(path.google, "My Drive", NDVIsave), paste0("Landsat5_", LCTYPE, "_")))]
  
  if(!file.exists(file.path(pathShare, fileL8))) file.copy(from=file.path(path.google, "My Drive", NDVIsave, fileL8), to=file.path(pathShare, fileL8), overwrite=T, copy.mode=T)
  if(!file.exists(file.path(pathShare, fileL9))) file.copy(from=file.path(path.google, "My Drive", NDVIsave, fileL9), to=file.path(pathShare, fileL9), overwrite=T, copy.mode=T)
  if(!file.exists(file.path(pathShare, fileL7))) file.copy(from=file.path(path.google, "My Drive", NDVIsave, fileL7), to=file.path(pathShare, fileL7), overwrite=T, copy.mode=T)
  if(!file.exists(file.path(pathShare, fileL5))) file.copy(from=file.path(path.google, "My Drive", NDVIsave, fileL5), to=file.path(pathShare, fileL5), overwrite=T, copy.mode=T)
  
  landsat8 <- read.csv(file.path(path.google, "My Drive", NDVIsave, fileL8))
  landsat9 <- read.csv(file.path(path.google, "My Drive", NDVIsave, fileL9))
  landsat7 <- read.csv(file.path(path.google, "My Drive", NDVIsave, fileL7))
  landsat5 <- read.csv(file.path(path.google, "My Drive", NDVIsave, fileL5))
  
  landsat8$mission <- "landsat 8"
  landsat9$mission <- "landsat 9"
  landsat7$mission <- "landsat 7"
  landsat5$mission <- "landsat 5"
  
  landsatAll <- rbind(landsat8, landsat9, landsat7, landsat5)
  # landsatAll <- rbind(landsat8, landsat9)
  landsatAll$type <- LCTYPE
  
  ndviAll <- rbind(ndviAll, landsatAll)
}
summary(ndviAll)
unique(ndviAll$mission)

ndviAll$date <- as.Date(ndviAll$date)
ndviAll$year <- lubridate::year(ndviAll$date)
ndviAll$yday <- lubridate::yday(ndviAll$date)
ndviAll$type <- factor(ndviAll$type, levels=rev(c("forest", "grassland", "crop", "urban-open", "urban-low", "urban-medium", "urban-high")))
head(ndviAll)
summary(ndviAll)
unique(ndviAll$type)

write.csv(ndviAll, file.path(pathShare, "NDVIall_baseline.csv"), row.names=F)
write.csv(ndviAll, file.path(fout, "NDVIall_baseline.csv"), row.names=F)
