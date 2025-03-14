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
overwrite=F

# Copy the file with all of the yearly data
file.copy(from="NDVI_Automation_Workflow/data_all/NDVIall_years_modeled.csv", 
          to="Urban Drought App/data/NDVIall_years_modeled.csv", overwrite=T)
 
# Copy the normals file only if we need to
if(!file.exists("Urban Drought App/data/NDVIall_normals_modeled.csv") | overwrite){
  file.copy(from="NDVI_Automation_Workflow/data_all/NDVIall_normals_modeled.csv",
            to="Urban Drought App/data/NDVIall_normals_modeled.csv", overwrite=T)
}



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
