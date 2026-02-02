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
data <- get_lst(bbox, 
        box_utm, 
        paste0(start_date, "T00:00:00Z"), 
        paste0(end_date, "T00:00:00Z"))
# mask out non-water pixels
masked_data <- water_mask(data)
# see what it looks like!
ggplot() +
  geom_stars(data = masked_data["thermal_C"]) +
  facet_wrap(~time) +
  theme_classic() +
  scale_fill_viridis(na.value = 'transparent') +
  labs(fill = "Temperature (C)") +
  coord_fixed()
# get values
thermal_vals <- get_vals(points, masked_data)
output <- clean_data(thermal_vals)
# save out targets
# create directory for targets file
dir.create('targets/')
dir.create(paste0('/targets/', site, '/'))
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
get_ha(bathy, points)

################################################################################
# 4. Get Kw factor (light extinction)
# If your lake of interest is in the US, use the function get_kw_US
# If your lake of interest is outside the US, use the function get_kw_global
################################################################################
source("get_Kw.R")
# Search for lake in LAGOS US database
mylake_kw <- get_kw_US(bbox)

# Search for lake in global database
mylake_kw <- get_kw_global(bbox)

# If your lake was unavailable in either of these databases, 
# you may set mylake_kw based on knowledge of your lake of interest
# If your lake is very turbid (Secchi < 1):
# mylake_kw <- 1.7/1
# If your lake is somewhere between turbid and clear (Secchi > 1, < 5):
# mylake_kw <- 1.7/3
# If your lake is very clear (Secchi > 5):
# mylake_kw <-  1.7/5


################################################################################
# 5. Estimate sediment zone info
################################################################################
source("get_SedZoneInfo.R")
# first get air temperature data over a few years
era5_download <- get_historical_weather(latitude = points_df$lat,
                                        longitude = points_df$lon,
                                        start_date = Sys.Date() - 2000, # get a long enough date range
                                        end_date = Sys.Date(),
                                        variables = c("temperature_2m"))
# the average sediment zone temperature is comparable to average air temperature
sed_temp <- mean(era5_download$prediction)
# if the lake is < 5 m deep, the peak doy and amplitude are also comparable to air temp
sed_data <- get_sed_zone_data(era5_download, (max(values(bathy), na.rm = T) - min(values(bathy), na.rm = T)))


################################################################################
# 6. Create GLM file
################################################################################

