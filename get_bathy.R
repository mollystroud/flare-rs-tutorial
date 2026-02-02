################################################################################
# GLOBathy script
################################################################################
pacman::p_load(tidyverse, sf, raster, terra, dplyr, elevatr, marmap, rLakeAnalyzer)
################################################################################
# this code creates an index file for GLOBathy, so users can easily find the
# bathymetry file corresponding to their lake of interest
################################################################################
#files <- list.files("Bathymetry_Rasters", full.names = T, recursive = T, pattern = ".tif")

#file_index <- data.frame()
#for(file in files){
  #tif <- raster(file)
  #xmin <- round(xmin(tif), digits = 4)
  #xmax <- round(xmax(tif), digits = 4)
  #ymin <- round(ymin(tif), digits = 4)
  #ymax <- round(ymax(tif), digits = 4)
  #info <- cbind(xmin, xmax, ymin, ymax, file)
  #file_index <- rbind(file_index, info)
  #print(file)
#}
#write_csv(file_index, "Bathymetry_Rasters/index_file.csv")
message("Downloading GLOBathy index file")
index <- read_csv("https://amnh1.osn.mghpcc.org/bio230121-bucket01/GLOBathy/GLOBathy_index.csv")
find_matches <- function(bbox){
  mean_x <- (bbox["xmin"] + bbox["xmax"]) / 2
  mean_y <- (bbox["ymin"] + bbox["ymax"]) / 2
  match <- index |>
    filter(xmin < mean_x & xmax > mean_x) |>
    filter(ymin < mean_y & ymax > mean_y)
  if(nrow(match > 0)){
    print(match)
    bathy <- raster(match$file)
    return(bathy)
  } else {message("No matches found")}
}

get_ha <- function(bathy_raster, points){
  # convert to df and clean up
  bathy_df <- as.data.frame(bathy_raster, xy = T)
  bathy_df <- na.omit(bathy_df)
  colnames(bathy_df) <- c("Longitude", "Latitude", "Elevation")
  # convert to H/A relationship for GLM
  min_elevation <- min(bathy_df$Elevation)
  
  bathy_df <- bathy_df |> 
    dplyr::mutate(height = Elevation - min(Elevation)) |> 
    dplyr::select(Longitude, Latitude, height) 
  data_grid <- griddify(bathy_df, nlon = ncol(bathy_raster), nlat = nrow(bathy_raster)) 
  area_grid <- raster::area(data_grid, na.rm = TRUE, weights = FALSE)
  # filter out cells with no data
  area_grid <- area_grid[!is.na(area_grid)] 
  surface_area <- length(area_grid)*mean(area_grid) 
  area_layers <- approx.bathy(Zmax = abs(max(bathy_df$height)), 
                              surface_area*1000000,
                              Zmean= mean(bathy_df$height), 
                              method = "cone",
                              zinterval = 1,
                              depths = seq(0, abs(max(bathy_df$height)), by = 1))
  
  #convert depth back to elevation
  area_layers$depths <- area_layers$depths + min_elevation
  ## plot the bathymetry profile
  area_layers$depths <- area_layers$depths*-1 #make it so that surface (0m) is at top
  # add actual elevation
  elev <- elevatr::get_elev_point(points)
  #print(elev$elevation)
  area_layers$depths <- area_layers$depths + elev$elevation
  plot(area_layers$Area.at.z, area_layers$depths, type = 'l', 
       xlab = 'Area at Depth (m2)', ylab = 'Depth (m)', main = 'GLOBathy')
  
}


