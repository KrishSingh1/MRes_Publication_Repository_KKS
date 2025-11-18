#### Thesis Ploting all vegetation classes ####
## KRISH sINGH
## 22-11-2024
## To do: Remove bioclimatic regions


# Libraries ----------------------------------------------------------------

library(ggplot2)
library(patchwork)
library(cowplot)
library(dplyr)
library(ozmaps)
library(ggstatsplot)
library(RColorBrewer)
library(ggsignif)
library(grDevices)
library(raster)
library(classInt)


# Functions ----------------------------------------------------------------

# Main --------------------------------------------------------------------


# Set up raster -----------------------------------------------------------


bioclimatic_region_tiff <- "../DATASETS/Australian Bioclimatic Regions/Australian_Bioclimatic_Regions.tif"
bio_raster <- raster(bioclimatic_region_tiff)
plot(bio_raster)

bio_df <- bio_raster %>% 
  as.data.frame(xy = TRUE) %>%
  na.omit() %>%
  mutate(Australian_Bioclimatic_Regions = as.factor(Australian_Bioclimatic_Regions)) %>%
  rename(`Bioclimatic Region` = Australian_Bioclimatic_Regions) 

bio_df$'Bioclimatic Region' <- recode_factor(bio_df$'Bioclimatic Region',
              '1' = 'Tropical',
              '2' = 'Savanna',
              '3' = 'Warm Temperate',
              '4' = 'Cool Temperate',
              '5' = 'Mediterranean',
              '6' = 'Desert')

# Testing
bioclimatic_regions_map <- ggplot(data = bio_df) +
  geom_raster(mapping = aes(x = x, y = y, 
                  fill = `Bioclimatic Region`)) +
  theme_bw() 
  
bioclimatic_regions_map


# Set up plot points ------------------------------------------------------


sites_super_classified <- read.csv('../DATASETS/AusPlots_Extracted_Data/Final/sites_super_classified.csv')
sites_super_classified$`Dominant Growth Form` <- unlist(lapply(sites_super_classified$super_group,FUN = function(x){
  strsplit(x, ' ')[[1]][2]
  }))

sites_super_classified$`Dominant Growth Form` <- unlist(lapply(sites_super_classified$`Dominant Growth Form`,FUN = function(x){
  if(x == 'Hummock.grass') {
    return('Hummock grass')
  } else if (x == 'Tussock.grass') {
    return('Tussock grass')
  } else if (x == 'Tree.Palm'){
    return('Tree Palm')
  }
  
  return(x)
}))



site_locations <- read.csv('../DATASETS/AusPlots_Extracted_Data/Final/extracted_Final_site_info_2-0-6.csv')
site_locations <- site_locations[,c('site.info.site_location_name', 'site.info.latitude', 'site.info.longitude')] %>%
  unique() %>%
  rename(y = site.info.latitude,
         x = site.info.longitude,
         site_location_name = site.info.site_location_name)

  
sites_super_classified_merged <- sites_super_classified %>% left_join(site_locations)

ordered_type <- c('Forb', 'Hummock grass', 'Tussock grass', 'Chenopod', 'Shrub', 'Tree Palm')
sites_super_classified_merged$`Dominant Growth Form` <- factor(sites_super_classified_merged$`Dominant Growth Form`,
       levels = ordered_type)

color_maping = c(rgb(0.7815272727272726, 0.86, 0.33999999999999997),
                 rgb(0.4978909090909092, 0.86, 0.33999999999999997),
                 'forestgreen',
                 rgb(0.33999999999999997, 0.6869818181818181, 0.86),
                 rgb(0.33999999999999997, 0.4033454545454544, 0.86),
                 rgb(0.86, 0.33999999999999997, 0.592436363636363))

# Lets begin plotting -----------------------------------------------------


bioclimatic_regions_map <- ggplot() +
  geom_raster(data = bio_df, mapping = aes(x = x, y = y, fill = `Bioclimatic Region`)) +
  geom_point(data = sites_super_classified_merged,
              mapping = aes(x = x, y = y, shape = `Dominant Growth Form`, color = `Dominant Growth Form`),
             alpha = 0.9) +
  scale_color_manual(values = color_maping) +
  theme_bw() +
  ylab('Latitude') +
  xlab('Longitude') 

bioclimatic_regions_map

# Now A different map with Dominant Vegetation Type -----------------------

dom_veg_type <- read.csv('../DATASETS/AusPlots_Extracted_Data/Final/DEA_FC_Ground_Truth_Evaluation_complete_Height_Rule_Agg.csv') %>%
  unique() %>% left_join(site_locations) %>%
  rename(`Dominant Vegetation Type` = vegetation_type)
  


bioclimatic_regions_map <- ggplot(data = bio_df) +
  geom_raster(mapping = aes(x = x, y = y, 
                            fill = `Bioclimatic Region`)) +
  geom_point(data = dom_veg_type,
             mapping = aes(x = x, y = y, shape = `Dominant Vegetation Type`, color = `Dominant Vegetation Type`),
             alpha = 0.9) +
  scale_colour_manual(values = c("blue", "red", 'yellow'))+
  theme_bw() +
  ylab('Latitude') +
  xlab('Longitude')

bioclimatic_regions_map


# Using SLATS Star Transect locations -------------------------------------

# Using jenks
breaks <- c(0, 5, 17, 34, 58, 94)
labels <- c("0 - 5", "5 - 17", "17 - 34", "34 - 58", "58 - 94")

slats <- read.csv('../DATASETS/SLATS_STAR/star_transects.csv') %>%
  mutate(tables = tolower(tables))
slats_cal_val <- slats[grepl('fractional_cal/val', slats$tables),]

slats_agg <- slats_cal_val[, c('site', 'persist', 'ref_x', 'ref_y')] %>%
  group_by(site) %>%
  summarise(mean_persist = round(mean(persist)),
            ref_x = mean(ref_x),
            ref_y = mean(ref_y)) %>% # Take the mean of coords because they differ slightly by a bit for the revisit
  mutate(persist_cat = cut(mean_persist,
                           breaks = breaks,
                           labels = labels, 
                           include.lowest = TRUE))

#jenks_bins <- classIntervals(slats_agg$mean_persist, n = 5, style = "jenks")
#print(jenks_bins$brks)

bioclimatic_regions_map <- ggplot(data = bio_df) +
  geom_raster(mapping = aes(x = x, y = y,
                    fill = `Bioclimatic Region`)) +
  geom_point(data = slats_agg,
             mapping = aes(x = ref_x, 
                           y = ref_y, 
                           color = persist_cat), pch = 16) +
  theme_bw() +
  ylab('Latitude') +
  xlab('Longitude') +
  labs(color = 'Persistent Cover (%)') +
  viridis::scale_color_viridis(discrete = TRUE, option = 'cividis') 
  #viridis::scale_fill_viridis(discrete = TRUE, option = "cividis")
bioclimatic_regions_map





