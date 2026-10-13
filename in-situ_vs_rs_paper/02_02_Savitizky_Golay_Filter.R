####### Apply Savitizky Golay  Filter #######
## Krish K. Singh
## 06/10/26
## Objective:
## Perform the smoothing algorithm on the DEA FC timeseries 


# Library ---------------------------------------------------------------------------------------------------------


load_packages <- function(p) {
  installed_packages <- p %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    install.packages(p[!installed_packages])
  }
  invisible(lapply(p, library, character.only = TRUE))
}


packages <- c("dplyr", "data.table", "ggplot2", "gsignal", "stringr", "lubridate")
load_packages(packages)

# Functions -------------------------------------------------------------------------------------------------------



# Main ------------------------------------------------------------------------------------------------------------

## Savitzky-Golay Filter Parameters:
window_length_smooth = 15
polyorder = 4 
spatial_trimmed_dea_fc_dir <- 'DATASETS/DEA_FC_PROCESSED/SPATIAL_AND_UE_FILTER'
data_file_names <- list.files(spatial_trimmed_dea_fc_dir) %>% 
  str_subset(., 'log', negate = T) 

processed_out_dir <- 'DATASETS/DEA_FC_PROCESSED/PROCESSED'
# Process_Data ----------------------------------------------------------------------------------------------------

counter_max <- length(data_file_names)
counter_current <- 1

pb = txtProgressBar(min = 1, max = counter_max, initial = counter_current, style = 3,
                    title = 'Processing', label = data_file_names[counter_current]) 

for (i in seq_along(data_file_names)) {
  df_name <- data_file_names[i]
  data_file_path <- file.path(spatial_trimmed_dea_fc_dir, df_name)
  
  data <- fread(file = data_file_path) %>% 
    mutate(time_ymd = ymd(time_ymd)) %>% 
    arrange(time_ymd) %>% 
    mutate(pv_filter = sgolayfilt(pv, p = polyorder, n = window_length_smooth), # Perform filter 
           npv_filter = sgolayfilt(npv, p = polyorder, n = window_length_smooth),
           bs_filter = sgolayfilt(bs, p = polyorder, n = window_length_smooth)) %>% 
    rowwise() %>% 
    mutate(pv_filter  = max(0, pv_filter), 
           npv_filter = max(0, npv_filter),
           bs_filter  = max(0, bs_filter)) # constrain values between 0 and the value of the filter
  
  fwrite(file = file.path(processed_out_dir, df_name),
         x = data)
  
  counter_current <- counter_current + 1
  
  setTxtProgressBar(pb,counter_current,label = df_name)
}



