########################################
#Trying to fix r & python linking issue
########################################
#installing earth engine api 0.1.317
reticulate::py_install("earthengine-api==0.1.317", pip = TRUE)
#Setting credential path
Sys.setenv("EARTHENGINE_TOKEN" = "/Users/jocelyngarcia/.config/earthengine/jgarcia@mortonarb.org/credentials")
rgee::ee_Authenticate()


library(rgee)
ee_Initialize()
py_config()


library(rgee)
rgee::ee_Initialize(py_env = "r-reticulate")
library(rgee)
ee_clean_credentials()
ee_check()


#lastest version of rgee 
remotes::install_github("r-spatial/rgee", force = TRUE)
library(rgee)

#deleting old credentials not stored in the right place, setting path manually, and then authorizing
unlink("~/.config/earthengine/credentials", recursive = TRUE)

rgee::ee_Authenticate()

reticulate::import("ee")
rgee::ee_Authenticate()

Sys.setenv("EARTHENGINE_TOKEN" = "/Users/jocelyngarcia/.config/earthengine/jgarcia@mortonarb.org/credentials")

rgee::ee_Initialize(email = "jgarcia@mortonarb.org", drive = TRUE)

#Checking to make sure my account is linked 
reticulate::py_run_string("import ee; print(ee.data.getList({'id': 'users/jgarcia'}))")



#Uninstalling rgee & reticulate 
#library(utils)
#remove.packages("rgee")
#remove.packages("reticulate")

# Reinstall reticulate if necessary (only do this once for setup)
#install.packages("reticulate")

# Load the reticulate library
library(reticulate)

# Set Python version explicitly (if needed)
use_python("~/.pyenv/versions/3.10.14/bin/python3", required = TRUE)

# Check the Python configuration (optional)
py_config()

# Set the RETICULATE_PYTHON environment variable to the correct Python environment
Sys.setenv(RETICULATE_PYTHON = "/Users/jocelyngarcia/.pyenv/versions/rgee-env/bin/python")

# Set the virtual environment for rgee
use_virtualenv("/Users/jocelyngarcia/.virtualenvs/r-reticulate", required = TRUE)


reticulate::py_run_string("import ee; ee.Initialize()")



# After this, proceed with loading rgee
library(rgee)

ee_Initialize()

ee_clean_user_credentials()
ee_Authenticate()
#Checking python dependencies config
reticulate::py_config()

Sys.setenv("EARTHENGINE_GCLOUD" = "/opt/homebrew/bin/gcloud")

Sys.getenv("EARTHENGINE_CONFIG")
ee_Authenticate()



Sys.setenv("EARTHENGINE_CONFIG" = "/Users/jocelyngarcia/.config/earthengine")


#Checking
reticulate::py_config()


use_virtualenv("/Users/jocelyngarcia/.virtualenvs/rgee-new-env", required = TRUE)
