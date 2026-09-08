################################################################################
# Code started by Molly Stroud on 11/18/25
################################################################################

################################################################################
## the below code is designed to pull landsat thermal imagery over a specified
# area and estimate temperature over the reservoir
################################################################################
# get bboxes
#source("NEON_bboxes.R") # or, create your own bbox here

# define stac url
ls = stac("https://planetarycomputer.microsoft.com/api/stac/v1")

################################################################################
# Lightweight STAC search: returns number of cloud-free Landsat 8/9 items in a
# date range without downloading any raster data
################################################################################
search_lst_items <- function(bbox, start_date, end_date) {
  items <- ls |>
    stac_search(collections = "landsat-c2-l2",
                bbox = bbox,
                datetime = paste(start_date, end_date, sep="/"),
                limit = 1000) |>
    ext_query("eo:cloud_cover" < 30) |> #filter for cloud cover
    post_request()
  n_ls89 <- sum(vapply(items$features,
                       function(f) f$properties$platform %in% c("landsat-8", "landsat-9"),
                       logical(1)))
  return(n_ls89)
}

################################################################################
# Function to create thermal stars object with specified dates and bbox.
# If spinup_start and spinup_end are provided, warns when no images fall within
# that spinup period.
################################################################################
get_lst <- function(bbox, start_date, end_date, points,
                    spinup_start = NULL, spinup_end = NULL) {
  # check for images in the spinup period, if provided
  if(!is.null(spinup_start) && !is.null(spinup_end)){
    n_spinup <- search_lst_items(bbox, spinup_start, spinup_end)
    if(n_spinup == 0){
      message("There are no remote sensing images in your spinup period. We recommend adjusting or lengthening the spinup period so that at least one image is available.")
    }
  }
  # grab items within dates of interest
  items <- ls |>
    stac_search(collections = "landsat-c2-l2",
                bbox = bbox,
                datetime = paste(start_date, end_date, sep="/"),
                limit = 1000) |>
    ext_query("eo:cloud_cover" < 30) |> #filter for cloud cover
    post_request() |>
    items_sign(sign_fn = sign_planetary_computer()) |>
    items_fetch()
  if(length(items$features) == 0){
    message("There are no cloud-free thermal images of this lake in the specified date range. Consider changing or expanding your date range.")
    return( )
  } else {
    message("Downloading Landsat Thermal data")
    # keep only Landsat 8/9 surface-temperature (L2SP) scenes, which carry the
    # thermal lwir11 band; the collection also returns surface-reflectance (L2SR)
    # scenes that do not
    items$features <- Filter(
      function(f) f$properties$platform %in% c("landsat-8", "landsat-9") &&
        identical(f$properties$`landsat:correction`, "L2SP"),
      items$features
    )
    if(length(items$features) == 0){
      message("There are no cloud-free thermal images of this lake in the specified date range. Consider changing or expanding your date range.")
      return( )
    }
    # define the cube space
    cube <- cube_view(srs = "EPSG:4326",
                      extent = list(t0 = start_date,
                                    t1 = end_date,
                                    left = bbox[1],
                                    right = bbox[3],
                                    top = bbox[4],
                                    bottom = bbox[2]),
                      dx = 0.00031, # 30 m resolution
                      dy = 0.00031,
                      dt = "P1D",
                      aggregation = "median",
                      resampling = "average")
    # create stac image collection
    col <- stac_image_collection(items$features,
                                 asset_names = c("lwir11", "qa_pixel"),
                                 url_fun = identity)
    # make raster cube
    data <- raster_cube(image_collection = col,
                        view = cube) |>
      apply_pixel(expr = "((qa_pixel & (1<<7)) != 0) * lwir11", names = "thermal") |> # if not water, set to 0
      apply_pixel(expr = "(thermal * 0.00341802) - 124.15", names = "thermal_C") # convert to C
    # make stars obj
    ls_stars <- st_as_stars(data)
    ls_stars$thermal_C[ls_stars$thermal_C == -124.15] <- NA
    # remove empty dates
    arr <- ls_stars[[1]] # extract raw array (x, y, time)
    non_na_counts <- apply(arr, 3, function(slice) sum(!is.na(slice))) # count non-NA pixels for each time
    valid_idx <- which(non_na_counts > 0) # indices of slices that have at least one real value
    # build cleaned object by stacking only valid slices
    slices <- lapply(valid_idx, function(i) ls_stars[,,, i, drop = FALSE])
    clean_ls_stars <- do.call(c, c(slices, along = "time"))
    if(all(is.na(clean_ls_stars$thermal))){
      message("There are no cloud-free thermal images of this lake in the specified date range. Consider changing or expanding your date range.")
      return( )
    } else {
      vals <- st_extract(clean_ls_stars["thermal_C"], points)
      vals_df <- data.frame(vals)
      if(all(is.na(vals_df$thermal_C))){
        message("There are no cloud-free thermal images of this lake in the specified date range. Consider changing or expanding your date range.")
        return( )
      } else {
        return(clean_ls_stars)
      }
    }
  }
}
################################################################################
# function to extract values and write out csv
################################################################################
get_vals <- function(points, thermal_data){
  vals <- st_extract(thermal_data["thermal_C"], points)
  vals_df <- data.frame(vals)
  if(all(is.na(vals_df$thermal_C))){
    message("There are no cloud-free thermal images of this lake in the specified date range. Consider changing or expanding your date range.")
    return( )
  }
  # if only one point, add back in time column and rearrange to format
  if(length(vals_df) < 3){
    time <- st_dimensions(thermal_data)$time$values$start
    if((is.null(time) || is.na(time))){
      time <- st_dimensions(thermal_data)$time$offset
    }
    vals_df$time <- time
    vals_df <- vals_df |>
      relocate(thermal_C, .after = time)
  }
  
  # if multiple points, group same date points and get mean temp
  if(dim(points)[1] > 1){
    vals_df <- vals_df |>
      group_by(time) |>
      summarize(mean_thermal_C = mean(thermal_C, na.rm = T))
    return(vals_df)
  } else {
    return(vals_df)
  }
}

################################################################################
# function to clean up data for input to FLARE
################################################################################
clean_data <- function(values, site_id){
  values <- na.omit(values)
  if(length(values) > 2){
    values <- values[2:3]
  }
  values$time <- paste0(values$time, "T00:00:00Z")
  values$site_id <- site_id
  values$depth <- 0
  values$variable <- 'temperature'
  colnames(values)[1] <- "datetime"
  colnames(values)[2] <- "observation"
  values$observation[values$observation < 0] <- 0 # remove likely incorrect #s
  return(values)
}
