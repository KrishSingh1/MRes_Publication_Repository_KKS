####### DEA Evaluation Nearest Point  #######
# Author: Krish Singh
# Date: 2024-07-11
# Objective: To evaluate DEA FC against AusPlots FC at the nearest time point
#   of survey 
# Note: this is the current method we decided to use to evaluate DEA FC
#   because we smoothed the DEA FC via savitzky-golay filter
#   
#  
# Inputs:
# - DEA FC
# - AusPlots FC
# Process:
# 1. For each survey:
# 2.  Read in DEA FC + AusPlots FC + site_info
# 3.  Get time of survey from site
# 4.  From this time point, find the closest time point from DEA FC
# 5.  Record the DEA time point, the AusPlots time point, the DEA pv_filter, npv_filter, and bs_filter, and the AusPlots pv_filter, npv_filter, bs_filter
# Output:
# - A dataset with columns:
# - site_unique, ausplots timepoint, dea time point, DEA FC, AusPlots FC 
# Then, we plot some evaluation plots 

# Libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(tune)
library(ggpmisc)
library(reshape2)
library(tidyr)
library(MASS)
library(viridis)

# Functions ---------------------------------------------------------------


# Main --------------------------------------------------------------------

# Read and Process Datasets 

# We Perform the time point matching for each site with avaliable DEA FC
# To do that, we check our directory for downloaded DEA FC
directory <- "DATASETS/DEA_FC_PROCESSED/MODELLED_PREPROCESSED/"
files <- list.files(directory, pattern = "\\.csv$", full.names = FALSE)
fileNames <- tools::file_path_sans_ext(files)
fileNames <- unlist(lapply(strsplit(fileNames, '_'), FUN = function(x){
  unlist(x)[3]
}))

AusPlots_fc <- read.csv('DATASETS/AusPlots_FC_Iter_2_0_6.csv')[,-1]
site_info <- read.csv('DATASETS/extracted_Final_site_info_2-0-6.csv')[,-1]

colnames(site_info) <- unlist(lapply(strsplit(colnames(site_info), '\\.'), FUN = function(x){
  unlist(x)[3]
}))

AusPlots_fc <- AusPlots_fc %>%
  left_join(site_info[,c('site_unique', 'site_location_name', 'visit_start_date')], by = 'site_unique') %>%
  subset(site_location_name %in%  fileNames) %>% 
  mutate(visit_start_date = as.Date(visit_start_date),
         time = NA,
         pv_filter = NA,
         npv_filter = NA,
         bs_filter = NA) 

for (site in fileNames) {
  
  # Read site dea_fc 
  dea_fc <- read.csv(paste0(directory, 'Input_DataSet_', site, '.csv')) %>%
    mutate(time =  as.Date(time))
  
  # Get indices of the site 
  site_ground_indices <- which(AusPlots_fc$site_location_name == site)
  
  # Now for each visit within a site, find the nearest timepoint 
  for (i in site_ground_indices){
    on_ground_date <- AusPlots_fc$visit_start_date[i]
    
    closest_time_point_index <- dea_fc$time %>% 
      difftime(on_ground_date, units = 'days') %>%
      as.numeric() %>% abs() %>% which.min()
    
    # Update the dataset accordingly
    AusPlots_fc[i, c('time', 'pv_filter', 'npv_filter', 'bs_filter')] <- dea_fc[closest_time_point_index, c('time', 'pv_filter', 'npv_filter', 'bs_filter')]
  }
}

# Final Fix for the dea date
AusPlots_fc <- AusPlots_fc %>% mutate(time = as.Date(time))
write.csv(AusPlots_fc, 'DATASETS/DEA_FC_Ground_Truth_Evaluation.csv')

# Group by vegetation and growth form
AusPlots_fc <- read.csv('DATASETS/AusPlots_Extracted_Data/Final/DEA_FC_Ground_Truth_Evaluation.csv') %>%
  dplyr::select(!X)

remote_long <- AusPlots_fc %>%
  dplyr::select(!c(green, brown,bare, error)) %>%
  pivot_longer(cols = c("pv_filter", "npv_filter", "bs_filter"), 
               names_to = "fraction_type",
               values_to = "remote_value") %>%
  mutate(fraction_type = case_when(
    fraction_type == 'pv_filter' ~ 'green',                  
    fraction_type == 'npv_filter' ~ 'brown',
    fraction_type == 'bs_filter' ~ 'bare'))


obs_long <- AusPlots_fc %>%
  dplyr::select(!c("pv_filter", "npv_filter", "bs_filter")) %>%
  pivot_longer(cols = c(green, brown, bare), 
               names_to = "fraction_type",
               values_to = "on_ground_value")

is_agg <- TRUE
if(!is_agg) {
  print('Allocating vegetation type per visit')
  veg.type.agg <- read.csv('DATASETS/AusPlots_Extracted_Data/Final/AusPlots_VegType_PC_Height_Rule.csv') %>%
  dplyr::select(c("site_unique", "vegetation_type"))
  file_name <- 'DEA_FC_Ground_Truth_Evaluation_complete_Height_Rule.csv'
} else {
  print('Allocating vegetation type based on avg. visit')
  veg.type.agg <- read.csv('DATASETS/AusPlots_Extracted_Data/Final/AusPlots_Agg_VegType_PC_Height_Rule.csv') %>%
    dplyr::select(c("site_location_name", "vegetation_type"))
  file_name <- 'DEA_FC_Ground_Truth_Evaluation_complete_Height_Rule_Agg.csv'
}


combined <- remote_long %>% 
  left_join(obs_long,
            by = c('site_unique', 'fraction_type', 'time', 'site_location_name',
                   'visit_start_date', 'other'))  %>% 
  left_join(veg.type.agg) %>%
  filter(!is.na(fraction_type)) %>%
  filter(!is.na(on_ground_value))
  

write.csv(combined, paste0('../DATASETS/AusPlots_Extracted_Data/', file_name))