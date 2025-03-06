# Deploy The shiny App -- copying code from the Phenology Forecast to see if it will work -- there will need to be lots of changes I'm sure

#-----------------------------------------------------------------------------------------------------------------------------------#
# Inputs: Code
# Outputs: Runs the app
# Notes: You can run the app locally or online. Which working directory you need to be in depends on which you are doing
# You must be in the "shiny_app" folder to run online. You must be in this scripts wd to run it locally
#-----------------------------------------------------------------------------------------------------------------------------------#
library(shiny)
library(shinyWidgets)

# First, copy over any updated data
path.UrbDrought <- "~/Google Drive/Shared drives/Urban Ecological Drought/"

#NDVI file path (Using NDVI data from NDVI Drought Monitoring Workflow so they are fit to the spline)
setwd("Urban Drought App")

file.copy(from=file.path(path.UrbDrought, "data/UrbanEcoDrought_NDVI_LocalExtract/allNDVI_data.csv"), 
          to="data/allNDVI_data.csv")

file.copy(from=file.path(path.UrbDrought, "data/NDVI_drought_monitoring/k=12_norms_all_LC_types.csv"), 
          to="data/k=12_norms_all_LC_types.csv")

dir.create("figures/04_panel_plots_usdm_deviation_meanNDVI")

figs.cp <- dir(file.path(path.UrbDrought, "data/NDVI_drought_monitoring/figures/04_panel_plots_usdm_deviation_meanNDVI"))
for(i in 1:length(figs.cp)){
  file.copy(from=file.path(path.UrbDrought, "data/NDVI_drought_monitoring/figures/04_panel_plots_usdm_deviation_meanNDVI", figs.cp[i]), 
          to=file.path("figures/04_panel_plots_usdm_deviation_meanNDVI", figs.cp[i]))
  
}

dir.create("figures/06_scatterplots_usdm_deviation_growing_season")

figs.cp <- dir(file.path(path.UrbDrought, "data/NDVI_drought_monitoring/figures/06_scatterplots_usdm_deviation_growing_season"))
for(i in 1:length(figs.cp)){
  file.copy(from=file.path(path.UrbDrought, "data/NDVI_drought_monitoring/figures/06_scatterplots_usdm_deviation_growing_season", figs.cp[i]), 
            to=file.path("figures/06_scatterplots_usdm_deviation_growing_season", figs.cp[i]))
  
}

# file.path(path.UrbDrought, "data/NDVI_drought_monitoring/figures/04_panel_plots_usdm_deviation_meanNDVI"),

setwd("..")



#This is to run the app LOCALLY
#If you open this script and are in it's directory, all you have to do is runApp.
#setwd("../")  #This is kept here for when I am bouncing between online and local runs
# runApp("Urban Drought App")


#This section is to run the app ONLINE
#Must change the directory to the app itself to run it online
setwd("Urban Drought App")

rsconnect::deployApp(forceUpdate = T, launch.browser = F)

setwd("../")

print("Drought Portal Updated!")

#This is how you manually stop the ONLINE app
#stopApp(returnValue = invisible())
