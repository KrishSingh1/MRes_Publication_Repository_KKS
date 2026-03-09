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


# Functions ---------------------------------------------------------------

get_preprocessed_dea_fc <- function(query, 
                                    directory = 'C:/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/DataExtraction/BACKUP_DATA/csv_files',
                                    site.corners.data){
  dea.fc <- tryCatch({
    temp_read <- fread(paste0(directory, "/", query, ".csv")) # use data.table for faster processing
    test.dea.trimed <- trim_to_nearest_coord(site.corners.data = site.corners.data,
                                             dea.fc.i = temp_read,
                                             query = query, buffer = 20)
    #write.csv(test.dea.trimed, paste0('../DATASETS/DEA_FC_PROCESSED/SPATIAL/', query, '.csv')) # Save Separately for debugging purposes
    
    temp <- subset(test.dea.trimed, subset = (ue <= 25.5)) # filter based on unmixing error (25.5)
    temp <- aggregate(temp[,-1], 
                          by = list(temp$time), FUN = mean, na.rm = T) # aggregate
    colnames(temp)[1] = 'time'
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

    SW <- st_coordinates(subset(site_4_points, subset = (point == 'SW')))[,c('X','Y')]
    SE <- st_coordinates(subset(site_4_points, subset = (point == 'SE')))[,c('X','Y')]
    NE <- st_coordinates(subset(site_4_points, subset = (point == 'NE')))[,c('X','Y')]
    NW <- st_coordinates(subset(site_4_points, subset = (point == 'NW')))[,c('X','Y')]
    
    trimmed <- dea.fc.i %>% 
      st_as_sf(crs = 3577, coords = c('x', 'y'))
    boundary_polygon <- st_sfc(st_polygon(list(rbind(SW, SE, NE, NW, SW))), crs = 3577) %>%
      st_buffer(dist = buffer)
    
    trimmed <- trimmed[st_within(trimmed, boundary_polygon, sparse = FALSE),]
    
    if(plot_result == TRUE) { # Plot the result if desired
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
for (query in file.names) {
  # Get the progress bar
  print(paste0(
    'START: ', counter_current, '/',counter_max, ' {', query  ,'}'
  ))
  site.fc <- get_preprocessed_dea_fc(query, site.corners.data = site.corners.data.cleaned, directory = directory)
  print(site.fc)
  if(class(site.fc) != 'data.frame') {
    error.messages <- c(error.messages, paste0('Error in processing ', query, '.csv'))
  } else {
    #write.csv(site.fc, paste0('../DATASETS/DEA_FC_PROCESSED/SPATIAL_AND_UE_FILTER/', query, '.csv')) 
  }
  counter_current <- counter_current + 1
}
#writeLines(error.messages, '../DATASETS/DEA_FC_PROCESSED/SPATIAL_AND_UE_FILTER/log.txt')



