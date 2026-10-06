##### Theil-sen Regression ####
# Date: 24-07-24
# Author: Krish Singh
# Objective: determine a linear trend for each site FC using Theil-Sen Regression


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(data.table)
library(tidyr)
library(deming)

# Functions ---------------------------------------------------------------



# Main --------------------------------------------------------------------

directory <- 'DATASETS/DEA_FC_PROCESSED/PROCESSED/'
files <- list.files(directory, pattern = "\\.csv$", full.names = FALSE)
file.names <- tools::file_path_sans_ext(files)

site.info <- read.csv('DATASETS/extracted_Final_site_info_2-0-6.csv')
site.info.unique <- unique(site.info[,c('site.info.site_location_name', 
                                        'site.info.latitude', 'site.info.longitude')])
colnames(site.info.unique) <- c('site_location_name', 'latitude', 'longitude')

# Create temp df 
temp <- data.frame('site_location_name' = NA)
fractions <- c('pv_filter', 'npv_filter', 'bs_filter')
fractions_col_names <- c()
for (f in fractions) {
  fractions_col_names <- c(fractions_col_names, 
                           paste0(f, '_', 'slope'), 
                           paste0(f, '_', 'intercept'),
                           paste0(f, '_', 'slope_lower'),
                           paste0(f, '_', 'slope_higher'))
} # create the columns based on fraction, mainly the slope and intercept names 
temp[fractions_col_names] <- NA

counter <- 1
pb <- txtProgressBar(min = counter, max = length(file.names), style = 3)
for (f in file.names) { # Iterate through each site 
  setTxtProgressBar(pb, counter)
  # Keep track of the progress 
  #print(paste('Reading ', f, ' {',counter,'/',length(file.names),'}'))
  
  # This vector contains the slope/intercept params for each fraction
  fraction_slopes <- c()
  for (fraction in fractions) { # calc slope/intercept via thei-sen reg for each fraction
    dea_fc <- fread(paste0(directory, f, '.csv')) %>%
      mutate(time = ymd_hms(time)) 
    

    formula <- as.formula(paste0(fraction, ' ~ ', 'time'))
    t_s <- theilsen(formula, data = dea_fc, x = T, y = T, model = TRUE)
    fraction_slopes <- c(fraction_slopes, 
                         t_s$coefficients[2], # slope 
                         t_s$coefficients[1], # intercept 
                         t_s[["ci"]][2],
                         t_s[["ci"]][4]) 
    
  
  }
  names(fraction_slopes) <- fractions_col_names # name the slope/intercept
  temp <- rbind(temp, c('site_location_name' = f, fraction_slopes)) # bind the slope/intercept info into the temp
  counter <- counter + 1
}

thiel_sen_data <- temp %>% 
  filter(!is.na(site_location_name)) %>% 
  remove_rownames() %>% 
  mutate(pv_filter_slope_yr = as.numeric(pv_filter_slope) * 365,
         npv_filter_slope_yr = as.numeric(npv_filter_slope) * 365,
         bs_filter_slope_yr = as.numeric(bs_filter_slope) * 365)


# Now add lat-long coordinates to these sites:
thiel_sen_data <- thiel_sen_data %>% 
  left_join(site.info.unique, by = 'site_location_name')

# Checking for Slopes significance  -------------------------------------------------------

# We simply just check whether or not the lower and higher conf interval crosses 0

# get the sign of conf lower and higher 

for (i in c('pv_filter', 'npv_filter', 'bs_filter')){
  lower_sign <- sign(thiel_sen_data[[paste0(i, '_slope_lower')]]   %>% as.numeric())
  higher_sign <- sign(thiel_sen_data[[paste0(i, '_slope_higher')]] %>% as.numeric())
  thiel_sen_data[paste0(i, '_signf')] <- lower_sign == higher_sign
}


fwrite(thiel_sen_data,'DATASETS/AusPlots_Theil_Sen_Regression_Stats_Signf.csv', row.names=FALSE)
