################################################################################
# Code started by Molly Stroud on 12/16/25
# Download data from dynamical.org and put into correct formatting for FLARE
# https://dynamical.org/catalog/noaa-gefs-forecast-35-day/
################################################################################
pacman::p_load('tidyverse', 'zarr', 'Rarr', 'sf', 'reticulate')

################################################################################
# Use Python env
################################################################################
message("Setting up Python environment. Python must be downloaded for this to run.")
venv_path <- file.path(getwd(), ".venv")
py_install("dask")
if (!dir.exists(venv_path)) {
  virtualenv_create(venv_path)
  virtualenv_install(
    venv_path,
    packages = c(
      "dask",
      "xarray",
      "zarr",
      "certifi",
      "numpy",
      "fsspec",
      "requests",
      "aiohttp"
    )
  )
}
use_virtualenv(venv_path, required = TRUE)

# python libraries
certifi <- import("certifi")
os <- import("os")
xr <- import("xarray")
builtins <- import("builtins", convert = FALSE)

# make sure http can be accessed
os$environ["SSL_CERT_FILE"] <- certifi$where()

message("Opening data from data.dynamical.org")
# open the zarr from dynamical.org
ds <- xr$open_zarr(
  "https://data.dynamical.org/noaa/gefs/forecast-35-day/latest.zarr?email=optional@email.com",
  #"https://data.dynamical.org/noaa/gefs/analysis/latest.zarr",
  consolidated = TRUE,
  decode_timedelta = TRUE,
  chunks = "auto"
)
#py_to_r(ds$init_time$values)

source("to_hourly.R")
# variables of interest
vars <- c(
  'temperature_2m',
  'relative_humidity_2m',
  'pressure_surface',
  'wind_u_10m',
  'wind_v_10m',
  'downward_long_wave_radiation_flux_surface',
  'downward_short_wave_radiation_flux_surface',
  'precipitation_surface'
)

# function: get met data from dynamical.org
get_temp_gefs <- function(site_id, start_time, bbox, lead_time = TRUE) {
  lat_min = bbox[[2]]
  lat_max = bbox[[4]]
  lon_min = bbox[[1]]
  lon_max = bbox[[3]]
  mean_lat <- mean(c(lat_min, lat_max))
  mean_lon <- mean(c(lon_min, lon_max))
  temp <- ds[r_to_py(vars)]$sel(
    init_time = as.character(start_time),
    latitude = mean_lat,
    longitude = mean_lon,
    method = "nearest"
  )
  if(lead_time == TRUE){
    temp_r <- temp$assign_coords(
      lead_hours = temp$lead_time$astype("timedelta64[h]")$astype("int"),
      member_id  = temp$ensemble_member,
      init_time = temp$init_time
    )
  } else {
    temp_r <- temp$assign_coords(
      lead_hours = lead_time,
      member_id  = temp$ensemble_member,
      init_time = temp$init_time
    )
  }
  temp_r$to_dataframe()$reset_index()$to_csv('met_temp.csv', index = F)
  temp_df <- read_csv('met_temp.csv', show_col_types = FALSE)
  file.remove("met_temp.csv")
  temp_df <- temp_df |>
    dplyr::select(-c(expected_forecast_length,
              ingested_forecast_length,
              latitude,
              longitude,
              spatial_ref,
              lead_hours)) |>
    pivot_longer(cols = all_of(vars),
                 names_to = 'variable',
                 values_to = 'prediction') |>
    mutate(family = 'ensemble', site_id = site_id, 
           init_time = as.Date(init_time), tz = '') |>
    dplyr::rename(
      'reference_datetime' = 'init_time',
      'datetime' = 'valid_time',
      'parameter' = 'member_id'
    )
  # change variable names
  temp_df$variable[temp_df$variable == "temperature_2m"] <- "air_temperature"
  temp_df$variable[temp_df$variable == "relative_humidity_2m"] <- "relative_humidity"
  temp_df$variable[temp_df$variable == "pressure_surface"] <- "air_pressure"
  temp_df$variable[temp_df$variable == "wind_u_10m"] <- "eastward_wind"
  temp_df$variable[temp_df$variable == "wind_v_10m"] <- "northward_wind"
  temp_df$variable[temp_df$variable == "downward_long_wave_radiation_flux_surface"] <- "surface_downwelling_longwave_flux_in_air"
  temp_df$variable[temp_df$variable == "downward_short_wave_radiation_flux_surface"] <- "surface_downwelling_shortwave_flux_in_air"
  temp_df$variable[temp_df$variable == "precipitation_surface"] <- "precipitation_flux"
  var_order <- names(temp_df)
  # set as UTC
  temp_df$datetime <- as_datetime(temp_df$datetime)
  attr(temp_df$datetime, "tzone") <- "UTC"
  # call hourly function
  df <- get_hourly(temp_df, mean_lon, mean_lat)
  return(df)
}

# stage 2 function
get_stage_2 <- function(start_date, end_date, site, bbox){
  dates <- seq(as.Date(start_date),
               as.Date(end_date),
               by = "1 day")
  for(date in dates) {
    message("Downloading stage 2 met data for ", as.Date(date))
    metdata <- get_temp_gefs(
      site_id = site,
      start_time = as.character(as.Date(date)),
      bbox = bbox)
    metdata$reference_datetime <- as.Date(date)
    metdata |>
      dplyr::group_by(reference_datetime, site_id) |>
      dplyr::group_walk(~ {
        dir <- file.path(
          "drivers/met/gefs-v12/stage2",
          paste0("reference_datetime=", .y$reference_datetime),
          paste0("site_id=", .y$site_id))
        dir.create(dir, recursive = TRUE, showWarnings = FALSE)
        arrow::write_parquet(.x, file.path(dir, "part-0.parquet"))
      })
  }
  message("Stage 2 data downloaded!")
}

# stage 3 function
get_stage_3 <- function(start_date, site, bbox){
  # get date sequence
  dates <- seq(as.POSIXct(as.Date(start_date) - (5)), 
               as.POSIXct(start_date), 
               by = ("3 hours"))
  # empty df
  stage3 <- data.frame()
  # for each date, get met data and bind together
  for(time in dates){
    message("Downloading stage 3 met data for ", (as.POSIXct(time)))
    metdata <- get_temp_gefs(site_id = site, 
                             start_time = as.character(as.POSIXct(time)),
                             bbox = bbox,
                             lead_time = 0)
    stage3 <- rbind(stage3, metdata)
  }
  
  stage3 |>
    group_by(site_id) |>
    group_walk(~ {
      dir <- file.path(
        "drivers/met/gefs-v12/stage3",
        paste0("site_id=", .y$site_id))
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
      arrow::write_parquet(.x, file.path(dir, "part-0.parquet"))
    })
  message("Stage 3 data downloaded!")
}
