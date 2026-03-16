###### DEA_Data_Repeated_Sites_Visualisation ######
# By Krish Singh
# 20240929
# To see the change in fractional cover from site visit to another with DEA data. 

# Libraries ---------------------------------------------------------------

library(plotly)
library(ggplot2)
library(dplyr)
library(caret)
library(cowplot)
library(data.table)
library(tune)
library(ggpubr)
library(ggpmisc)
library(Matrix)
library(Metrics)
library(grid)
library(tidyr)



# Functions ---------------------------------------------------------------



# Main --------------------------------------------------------------------

sites.query <- read.csv("../DATASETS/sites_info_query.csv")

# Load AusPlots data 
evaluation_fc <- fread('../DATASETS/AusPlots_Extracted_Data/Final/DEA_FC_Ground_Truth_Evaluation.csv') %>%
  mutate(time = as.Date(time)) %>%
  arrange(time)
  
# Looking at the varying number of NAs between in-situ for green, brown, bs
# Its best to calculate the change differently
# Additionally, we are not considering the length of time into our change calcs 

evaluation_list <- list(green_fc = na.omit(evaluation_fc[,c('site_unique', 'site_location_name', 'green', 'pv_filter', 'time')]),
     brown_fc = na.omit(evaluation_fc[,c('site_unique', 'site_location_name', 'brown', 'npv_filter', 'time')]),
     bs_fc = na.omit(evaluation_fc[,c('site_unique', 'site_location_name', 'bare', 'bs_filter', 'time')]))

evaluation_list <- lapply(evaluation_list, FUN = function(x) {
  counts.df <- as.data.frame(table(x$site_location_name)) %>%
    subset(Freq >= 2)  # Remove sites that were not revisited
  x <- subset(x, site_location_name %in% unique(counts.df$Var1)) # now subset the dataframe by sites with more than 1 visit
  return(x)
})

change_list <- lapply(evaluation_list, FUN = function(one_fraction) {
  
  one_fraction <- as.data.frame(one_fraction)
  site_list <- unique(one_fraction$site_location_name)
  temp <- c()
  
  for(s in site_list) {
    site_fc_information <- subset(one_fraction, site_location_name == s) %>%
      arrange(time)
    n_samples <- nrow(site_fc_information)
    
    for (sample in 1:(n_samples-1)) {
      # Sample 'a' and 'b' the first and second  time point, with the time points 
      
      essential_cols_names <- c('green', 'brown', 'bare', 'pv_filter', 'bs_filter', 'npv_filter', 'time')
      essential_cols <- colnames(site_fc_information)[which(colnames(site_fc_information) %in% essential_cols_names)] # Extract by fraction type and 'time'

      a <- site_fc_information[sample, essential_cols]
      b <- site_fc_information[sample + 1, essential_cols]
      
      time_a <- site_fc_information[['time']][sample]
      time_b <- site_fc_information[['time']][sample + 1]
      
      change <- b - a 
      change <- change %>%
        mutate(time_a = time_a,
               time_b = time_b,
               site_location_name = s) %>%
        rename(days_difference = time)
      
      temp <- rbind(temp, change)
    }
  }
  temp <- as.data.frame(temp)
  rownames(temp) <- 1:nrow(temp)
    
  return(temp)
})

## Combine the fractions into one  

remote_long <- change_list$green_fc %>%
  left_join(change_list$brown_fc) %>%
  left_join(change_list$bs_fc)  %>% 
  select(!c("green", "brown", "bare")) %>%
  pivot_longer(cols = c("pv_filter", "npv_filter", "bs_filter"), 
               names_to = "fraction_type",
               values_to = "remote_value") %>%
  mutate(fraction_type = case_when(
    fraction_type == 'pv_filter' ~ 'green',                  
    fraction_type == 'npv_filter' ~ 'brown',
    fraction_type == 'bs_filter' ~ 'bare'))

obs_long <- change_list$green_fc %>%
  left_join(change_list$brown_fc) %>%
  left_join(change_list$bs_fc) %>%
  select(!c("pv_filter", "npv_filter", "bs_filter")) %>%
  pivot_longer(cols = c("green", "brown", "bare"), 
               names_to = "fraction_type",
               values_to = "on_ground_value")
  

veg.type.agg <- read.csv('../DATASETS/AusPlots_Extracted_Data/Final/AusPlots_Agg_VegType_PC_Height_Rule.csv') %>%
  select(c("site_location_name", "vegetation_type"))
  
combined <- remote_long %>% 
  left_join(obs_long,
            by = c('site_location_name','time_a', 
                   'time_b', 'fraction_type', 'days_difference'))  %>%
  left_join(veg.type.agg) 


write.csv(combined, '../DATASETS/AusPlots_Extracted_Data/Final/Fractional_Cover_Change_Evaluation.csv')
