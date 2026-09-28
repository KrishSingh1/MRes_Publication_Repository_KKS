### DEA FC Reprocessing ###
# Trim spatial data + filter by ue
# Krish Singh
# 20240122

# Library -----------------------------------------------------------------

library(data.table)
library(ncdf4)
library(dplyr)
library(sf)
library(ggplot2)
library(ausplotsR)
library(sfheaders)
library(lubridate)


# Functions ---------------------------------------------------------------

get_preprocessed_dea_fc <- function(query, directory, site.corners.data,
                                    ue_filter = 25.5, plot_spatial_extent = FALSE){
  dea.fc <- tryCatch({
    temp_read <- fread(paste0(directory, "/", query, ".csv")) # use data.table for faster processing
    test.dea.trimed <- trim_to_nearest_coord(site.corners.data = site.corners.data,
                                             dea.fc.i = temp_read,
                                             query = query, buffer = 20,
                                             plot_result = plot_spatial_extent)

    variables_to_agg <- c('time', 'pv', 'npv', 'bs', 'ue', 'x', 'y')
    temp <- test.dea.trimed %>% 
      dplyr::filter(ue <= ue_filter) %>% # ue filter 
      dplyr::mutate(time = ymd_hms(time),
                    time_ymd = as_date(time)) %>% 
      dplyr::group_by(time_ymd) %>%         # Average all time-point to be a day-resolution 
      dplyr::summarise(across(everything(), .fns = function(x) mean(x, na.rm = T)))
    
    return(temp)
  }, error = function(e) {
    print(paste0(conditionMessage(e), " in ", query))
    return(NA)
  })
  return(dea.fc)
}

trim_to_nearest_coord <- function(site.corners.data, dea.fc.i, query, buffer = 30, plot_result = FALSE) {
  
  # Subset the site corners data by the query 
  
    essential_points <- c('SW', 'SE', 'NE', 'NW')
    site_4_points <- site.corners.data %>%
      subset((site_location_name == query) & (point %in% essential_points))
    
    #print(site_4_points)

    SW <- st_coordinates(subset(site_4_points, subset = (point == 'SW')))[,c('X','Y')]
    SE <- st_coordinates(subset(site_4_points, subset = (point == 'SE')))[,c('X','Y')]
    NE <- st_coordinates(subset(site_4_points, subset = (point == 'NE')))[,c('X','Y')]
    NW <- st_coordinates(subset(site_4_points, subset = (point == 'NW')))[,c('X','Y')]
    
    #print(dea.fc.i)
    trimmed <- dea.fc.i %>% 
      st_as_sf(crs = 3577, coords = c('x', 'y'))
    
    #print(trimmed)
    boundary_polygon <- st_sfc(st_polygon(list(rbind(SW, SE, NE, NW, SW))), crs = 3577) %>%
      st_buffer(dist = buffer)
    
    trimmed <- trimmed[st_contains(boundary_polygon, trimmed, sparse = FALSE, prepared = T),]
    #print(trimmed)
    
    if(plot_result) { # Plot the result if desired
     g <- ggplot() +  geom_sf(data = boundary_polygon) + geom_sf(data = trimmed) +
       geom_sf(data = site_4_points, colour = 'red')
     plot(g)
    }
    
    trimmed <- trimmed %>%
      sf_to_df(fill = T)
    trimmed <- trimmed[, c('time', 'pv', 'npv', 'bs', 'ue', 'x', 'y', 'spatial_ref')]
    
  return(trimmed)
}


# Main --------------------------------------------------------------------

# Alg:
# 1. Obtain all coordinate points for each sites via the published corner points 
# 2. Convert the coordinates from the corner points into EPSG 3577
# 3. Read in the DEA FC from the site 
# 4. Using the corner points from the published corner points, subset the DEA FC
#    --> such that all internal points are kept 
# 5. Filter the DEA FC to include all points under ue <= 25.5
# 6. Perform Spatial Averaging by the mean and by the date of the satelite image 

directory <- 'DATASETS/DEA_FC_PROCESSED/RawDataCurrent/NewBatchCurrent'
files <- list.files(directory, pattern = "\\.csv$", full.names = FALSE)
file.names <- tools::file_path_sans_ext(files)

site.corners.data <- read.csv('DATASETS/AusPlots_Published_Corner_Points_20240701/Published Plot Corners_extract26062024_cleaned.csv')
site.corners.data.cleaned <- site.corners.data[, c('site_location_name', 'point', 'x', 'y')]
site.corners.data.cleaned <- site.corners.data.cleaned %>%
  st_as_sf(coords = c('x', 'y')) %>%
  st_set_crs(3577) # Set crs to the original crs

plot(st_geometry(site.corners.data.cleaned)) # Plot to check if this roughly makes an Australian shape

error.messages <- c('')
counter_max <- length(file.names)
counter_current <- 1

pb = txtProgressBar(min = 1, max = counter_max, initial = counter_current, style = 3,
                    title = 'Processing', label = file.names[1]) 
for (query in file.names) {
  # Get the progress bar
  site.fc <- get_preprocessed_dea_fc(query, site.corners.data = site.corners.data.cleaned, 
                                     directory = directory, plot_spatial_extent = F)
  write.csv(site.fc, paste0('DATASETS/DEA_FC_PROCESSED/SPATIAL_AND_UE_FILTER/', query, '.csv')) 
  
  counter_current <- counter_current + 1
  setTxtProgressBar(pb,counter_current,label = query)
  
}
writeLines(error.messages, 'DATASETS/DEA_FC_PROCESSED/SPATIAL_AND_UE_FILTER/log.txt')
close(pb)


