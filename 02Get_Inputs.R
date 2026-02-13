################################################################################
# Author: Molly Stroud
# Started 1/20/26
################################################################################

# This script will:
# 1. Download remote sensing data
# 2. Download meteorological data
# 3. Grab bathymetry data (OPTIIONAL)
# 4. Grab Kw factor
# 5. Eestimate sediment zone info
# 6. Create GLM file

# get lake specifications
source("01LakeInfo.R")

################################################################################
# 1. Download remote sensing data
# Warning: if you are trying to download data over a long period of time (>>1yr) 
# or over a large lake, this will take a long time. Consider looping through
# chunks of time in this section and merging the dfs together at the end
################################################################################
source("get_LST.R")
thermaldata <- get_lst(bbox, 
        paste0(start_date, "T00:00:00Z"), 
        paste0(end_date, "T00:00:00Z"),
        points)
# see what it looks like!
ggplot() +
  geom_stars(data = thermaldata["thermal_C"]) +
  facet_wrap(~time) +
  theme_classic() +
  scale_fill_viridis(na.value = 'transparent') +
  labs(fill = "Temperature (C)") +
  coord_fixed()
# get values
thermal_vals <- get_vals(points, thermaldata)
output <- clean_data(thermal_vals)
# save out targets
# create directory for targets file
dir.create(paste0('./targets/', site, '/'))
write_csv(output, paste0('targets/', site, '/', site, '-targets-rs.csv'))


################################################################################
# 2. Download meteorological data
# Warning: this may take a while depending on length of your date range
# Warning: python must be installed to run this 
################################################################################
source("get_met.R")
# download stage 2 
get_stage_2(start_date, end_date, site, bbox)
# download stage 3
get_stage_3(start_date, site, bbox)


################################################################################
# 3. Get bathymetric data (OPTIONAL!)
# If you already have existing bathymetry, skip to get_ha function and input your
# bathymetry raster
################################################################################
# get bathymetry from GLOBathy
source("get_bathy.R")
bathy <- find_matches(bbox)
plot(bathy) # check this is the correct lake
ha <- get_ha(bathy, points)

################################################################################
# 4. Get Kw factor (light extinction)
# If your lake of interest is in the US, use the function get_kw_US
# If your lake of interest is outside the US, use the function get_kw_global
################################################################################
source("get_Kw.R")
# Search for lake in LAGOS US database
mylake_kw <- get_kw_US(bbox)

# Search for lake in global database
#mylake_kw <- get_kw_global(bbox)

# If your lake was unavailable in either of these databases, 
# you may set mylake_kw based on knowledge of your lake of interest

# If your lake is very turbid (Secchi < 1):
# mylake_kw <- 1.7/1

# If your lake is somewhere between turbid and clear (Secchi > 1, < 5):
mylake_kw <- 1.7/3

# If your lake is very clear (Secchi > 5):
# mylake_kw <-  1.7/5


################################################################################
# 5. Estimate sediment zone info
################################################################################
source("get_SedZoneInfo.R")
devtools::install_github("FLARE-forecast/ropenmeteo", force = T, upgrade = "never")
library(ropenmeteo)
# first get air temperature data over a few years
era5_download <- get_historical_weather(latitude = points_df$lat[1],
                                        longitude = points_df$lon[1],
                                        start_date = Sys.Date() - 2000, # get a long enough date range
                                        end_date = Sys.Date(),
                                        variables = c("temperature_2m"))
# if the lake is < 5 m deep, the peak doy and amplitude are also comparable to air temp
sed_data <- get_sed_zone_data(era5_download, 
                              depth = (max(values(bathy), na.rm = T) - min(values(bathy), na.rm = T)),
                              start_date)
print(sed_data)


################################################################################
# 6. Create GLM file and config files
################################################################################
source("edit_nml_functions.R")
remotes::install_github('usgs-r/glmtools', force = T, upgrade = 'never')
library(glmtools)

var_list <- list(site, mylake_kw, site, points_df[[2]][1], points_df[[1]][1],
                 dim(ha)[1], rev(ha$depths), rev(ha$Area.at.z), rev(max(ha$depths) - min(ha$depths)), 
                 sed_data$sed_temp, sed_data$sed_amp, sed_data$doy,
                 sed_data$zone_heights, sed_data$nzones[1])
var_name_list <- list("sim_name", "Kw", "lake_name", "latitude", "longitude",
                      "bsn_vals", "H", "A", "lake_depth",
                      "sed_temp_mean", "sed_temp_amplitude", "sed_temp_peak_doy",
                      "zone_heights", "n_zones")
# create list of variable values & names for input to nml
update_nml(var_list, var_name_list, 
           working_directory = 'configuration/analysis', nml = 'glm3.nml')

# update configure_flare
yml <- yaml::read_yaml("configuration/analysis/configure_flare.yml")
yml$location$site_id <- site
yml$location$latitude <- points_df[[2]][1]
yml$location$longitude <- points_df[[1]][1]
yml$default_init$lake_depth <- (max(ha$depths) - min(ha$depths))
yml$default_init$temp <- rep(sed_data$water_temp_init[1], times = yml$default_init$lake_depth+1)
yml$default_init$temp_depths <- seq(0, yml$default_init$lake_depth)
yml$model_settings$modeled_depths <- seq(0, yml$default_init$lake_depth)

yaml::write_yaml(yml, "configuration/analysis/configure_flare.yml")

################################################################################
# Now, open 03FLARE to run FLARE
################################################################################