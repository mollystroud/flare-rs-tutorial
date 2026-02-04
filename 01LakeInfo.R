#################################################################################
# Author: Molly Stroud
# Started 1/29/26
################################################################################
pacman::p_load(sf)
# Input below:
# 1. Your desired bounding box coordinates (and UTM zone)
# 2. The coordinates of a representative point(s) over your lake or reservoir of interest
# 3. Your start and end dates in the following format: YYYY-DD-MM

################################################################################
# INPUTS
################################################################################

# a four letter site name
# EXAMPLE: SUGG for Lake Suggs
site <- "wald"
# specify bounding box
bbox <- c(xmin = -71.3452,
          ymin = 42.4366,
          xmax = -71.3334,
          ymax = 42.4421)

# pick representative point(s) of lake
# for example, if your lake is a perfect circle, a good point would be the
# middle of the circle
points_df <- data.frame(lon = c(-71.3394), lat = c(42.4393))
points <- st_as_sf(x = points_df,
                   coords = c("lon", "lat"),
                   crs = 4326)


# dates over which you want to run forecasts
# **START DATE MUST BE AFTER 2020-10-01**
start_date <- "2025-10-01"
end_date <- "2025-10-10"
#end_date <- "2025-11-01"


