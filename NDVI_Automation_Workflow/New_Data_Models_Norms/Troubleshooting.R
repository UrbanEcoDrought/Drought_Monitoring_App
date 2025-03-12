########################################
#Trying to fix r & python linking issue
########################################
#installing earth engine api 0.1.317
reticulate::py_install("earthengine-api==0.1.317", pip = TRUE)
#Setting credential path
Sys.setenv("EARTHENGINE_TOKEN" = "/Users/jocelyngarcia/.config/earthengine/jgarcia@mortonarb.org/credentials")
rgee::ee_Authenticate()

library(reticulate)
library(rgee)
rgee::ee_clean_user_credentials()
reticulate::use_virtualenv("/Users/jocelyngarcia/.virtualenvs/r-reticulate", required = TRUE)


reticulate::import("ee")
rgee::ee_Authenticate()
#rgee::ee_Initialize(email = "jgarcia@mortonarb.org", drive = TRUE)
#rgee::ee_Initialize(user = "users/jgarcia", drive = TRUE)
rgee::ee_Initialize(user = 'users/jgarcia', drive=T, project = "urbanecodrought")

#Checking to make sure my account is linked
reticulate::py_run_string("import ee; print(ee.data.getList({'id': 'users/jgarcia'}))")



#Troubleshooting code to try if not working 
#deleting old credentials not stored in the right place, setting path manually, and then authorizing
unlink("~/.config/earthengine", recursive = TRUE)

# Set Python version explicitly (if needed)
use_python("~/.pyenv/versions/3.10.14/bin/python3", required = TRUE)

# Check the Python configuration (optional)
py_config()

# Set the RETICULATE_PYTHON environment variable to the correct Python environment
Sys.setenv(RETICULATE_PYTHON = "/Users/jocelyngarcia/.pyenv/versions/rgee-env/bin/python")

# Set the virtual environment for rgee
use_virtualenv("/Users/jocelyngarcia/.virtualenvs/r-reticulate", required = TRUE)

reticulate::py_run_string("import ee; ee.Initialize()")

Sys.setenv("EARTHENGINE_GCLOUD" = "/opt/homebrew/bin/gcloud")

Sys.getenv("EARTHENGINE_CONFIG")
ee_Authenticate()
