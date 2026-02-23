################################################################################
# Author: Molly Stroud
# Started 1/20/26
################################################################################

# This script will install all necessary packages to run FLARE 

remotes::install_github('rqthomas/GLM3r')
remotes::install_github('usgs-r/glmtools', force = T, upgrade = 'never')
library(glmtools)
pacman::p_load('rstac', 'terra', 'stars', 'ggplot2', 'tidyterra', 'viridis', 'yaml', 
               'gdalcubes', 'tmap', 'dplyr', 'tidyverse', 'sf', 'reticulate',
               'arrow', 'raster', 'terra', 'elevatr', 'marmap', 'rLakeAnalyzer')