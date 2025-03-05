########################################
#Trying to fix r & python linking issue
########################################

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


# After this, proceed with loading rgee
library(rgee)
ee_install()

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

ee_initialize()

use_virtualenv("/Users/jocelyngarcia/.virtualenvs/rgee-new-env", required = TRUE)
