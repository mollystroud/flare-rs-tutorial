################################################################################
# Code started by Molly Stroud on 2/26/26
################################################################################

################################################################################
# the below code is designed to pull SWOT water surface elevation data over a 
# specified area using Hydrocron
################################################################################
get_swot <- function(bbox, start_date, end_date, site){
  base_url <- "https://soto.podaac.earthdatacloud.nasa.gov/hydrocron/v1/timeseries"
  # first get lake ID
  message("Downloading SWOT Prior Lake Database IDs")
  lakeIDs <- read_csv("SWOT_PLD_IDs.csv")
  mylake <- lakeIDs |>
    filter(bbox["left"] < lon & bbox["right"] > lon) |>
    filter(bbox["bottom"] < lat & bbox["top"] > lat)
  # get parameters for hydrocron
  params <- list(
    feature = "PriorLake",
    feature_id = mylake$lake_id,   # known from shapefile or prior lookup
    start_time = paste0(start_date, "T00:00:00Z"),
    end_time   = paste0(end_date, "T23:59:59Z"),
    collection_name = "SWOT_L2_HR_LakeSP_D",
    fields     = "lake_id,time_str,wse,area_total",
    output     = "csv"
  )
  resp <- GET(base_url, query = params)
  stop_for_status(resp)
  resp_text <- content(resp, "text", encoding = "UTF-8")
  resp_json <- fromJSON(resp_text)
  csv_string <- resp_json$results$csv
  data <- read_csv(csv_string,
                   na = c("-999999999999.0", "no_data"),
                   show_col_types = FALSE)
  # now organize into proper FLARE format
  #datetime,observation,site_id,depth,variable
  data <- data |>
    na.omit(data) |>
    dplyr::select(time_str, wse) |>
    rename(datetime = time_str) |>
    rename(observation = wse) |>
    mutate(datetime = paste0(as.Date(datetime), "T00:00:00Z")) |>
    mutate(site_id = site) |>
    mutate(depth = NA) |>
    mutate(variable = "depth")
  return(data)
}


# ggplot() +
#   geom_line(data = data, aes(x = time_str, y = wse*3.28084, color = "SWOT")) +
#   scale_color_manual(values = c("SWOT" = "purple", "in situ" = "darkgreen")) +
#   theme_classic() +
#   labs(x = element_blank(), y = element_blank(), color = element_blank())


# IN SITU COMP
# library(lubridate)
# #ccr
# insitu <- read_csv('/Users/mollystroud/Downloads/CCR_monthlyWVWA_1994_2025 - CCR_monthlyWVWA_1994_2022.csv')
# insitu <- insitu[1987:nrow(insitu),]
# insitu$Date <- as_date(mdy_hm(insitu$Date))
# #fcr
# insitu <- read_csv('/Users/mollystroud/Downloads/FCR_50_WaterLevel_CURRENT - Water Level.csv')
# insitu$DateTime <- as.Date(insitu$DateTime)
# 
# ggplot() +
#   geom_line(data = data, aes(x = time_str, y = wse*3.28084, color = "SWOT")) +
#   geom_line(data = insitu, aes(x = Date, y = Reservoir_level, color = 'in situ')) +
#   scale_color_manual(values = c("SWOT" = "purple", "in situ" = "darkgreen")) +
#   theme_classic() +
#   labs(x = element_blank(), y = element_blank(), color = element_blank())




# Creation of database to pull lake ID from
# From doi/10.1029/2023WR036896
# library(DBI)
# library(RSQLite)
# library(tidyverse)
# 
# files <- list.files("/Users/mollystroud/Downloads/SWOT_Prior_Lake_Database", full.names = T)
# 
# lakeIDs <- data.frame()
# for(file in files) {
#   con <- dbConnect(RSQLite::SQLite(), dbname = file)
#   dbListTables(con)
#   df <- dbGetQuery(con, "SELECT * FROM lake") |>
#     dplyr::select(lake_id, names, lon, lat, ref_area) |>
#     mutate(lake_id = as.character(lake_id))
#   print(df)
#   lakeIDs <- rbind(lakeIDs, df)
#   dbDisconnect(con)
# }
# 
# write_csv(lakeIDs, "SWOT_PLD_IDs.csv")


