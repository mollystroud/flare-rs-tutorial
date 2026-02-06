################################################################################
# Author: Molly Stroud
# Started 2/5/26
################################################################################
# This script will run FLARE using the inputs created in 02Get_Inputs
pacman::p_load('tidyverse', 'lubridate')

# Point FLARE to GLM location
Sys.setenv('GLM_PATH'='/binary/macos-tahoe26/glm')
