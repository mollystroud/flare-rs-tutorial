################################################################################
# Code started by Molly Stroud on 2/26/26
################################################################################

################################################################################
## the below code is designed to pull SWOT data over a specified area
################################################################################
library(ggplot2)
library(httr)
library(jsonlite)
library(httr)
library(readr)

bbox <- c(left = -79.981728, 
          bottom = 37.367522, 
          right = -79.942552, 
          top = 37.407255)

#fcr
bbox <- c(left = -79.840037, 
          bottom = 37.301435, 
          right = -79.833651, 
          top = 37.311487)

base_url <- "https://soto.podaac.earthdatacloud.nasa.gov/hydrocron/v1/timeseries"

# first get lake ID
lakeIDs <- read_csv("SWOT_PLD_IDs.csv")
mylake <- lakeIDs |>
  filter(bbox["left"] < lon & bbox["right"] > lon) |>
  filter(bbox["bottom"] < lat & bbox["top"] > lat)

params <- list(
  feature = "PriorLake",
  feature_id = mylake$lake_id,   # known from shapefile or prior lookup
  start_time = "2023-06-01T00:00:00Z",
  end_time   = "2026-01-30T23:59:59Z",
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




# IN SITU COMP
library(lubridate)
# ccr
insitu <- read_csv('/Users/mollystroud/Downloads/CCR_monthlyWVWA_1994_2025 - CCR_monthlyWVWA_1994_2022.csv')
insitu <- insitu[1987:nrow(insitu),]
insitu$Date <- as_date(mdy_hm(insitu$Date))

# fcr
#insitu <- read_csv('/Users/mollystroud/Downloads/FCR_50_WaterLevel_CURRENT - Water Level.csv')
#insitu$DateTime <- as.Date(insitu$DateTime)

ggplot() +
  geom_line(data = data, aes(x = time_str, y = wse*3.28084, color = "SWOT")) +
  geom_line(data = insitu, aes(x = Date, y = Reservoir_level, color = 'in situ')) +
  scale_color_manual(values = c("SWOT" = "purple", "in situ" = "darkgreen")) +
  theme_classic() +
  labs(x = element_blank(), y = element_blank(), color = element_blank())




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


