#### Generate Map of AusPlots Sites ####
## KRISH sINGH
## 22-11-2024


# Libraries ----------------------------------------------------------------

library(ggplot2)
library(patchwork)
library(cowplot)
library(dplyr)
library(ozmaps)
library(RColorBrewer)
library(grDevices)
library(raster)
library(ggnewscale)


# Functions ----------------------------------------------------------------

# Main --------------------------------------------------------------------


# Set up Raster 
bioclimatic_region_tiff <- "DATASETS/Australian_Bioclimatic_Regions.tif"
bio_raster <- raster(bioclimatic_region_tiff)

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


# Set up site locations 
site_locations <- read.csv('DATASETS/extracted_Final_site_info_2-0-6.csv')
site_locations <- site_locations[,c('site.info.site_location_name', 'site.info.latitude', 'site.info.longitude')] %>%
  unique() %>%
  rename(y = site.info.latitude,
         x = site.info.longitude,
         site_location_name = site.info.site_location_name)

# Get veg dominance 
dom_veg_type <- read.csv('DATASETS/DEA_FC_Ground_Truth_Evaluation_complete_Height_Rule_Agg.csv') %>%
  left_join(site_locations) %>%
  dplyr::select(site_location_name, x, y, vegetation_type) %>%
  rename(`Vegetation Type` = vegetation_type) %>%
  unique()



# Generate Figure 1 -------------------------------------------------------
sf_oz <- ozmaps::ozmap("states")
ausplots_map <- ggplot() +
  geom_raster(data = bio_df,
              mapping = aes(x = x,
                            y = y, 
                            fill = `Bioclimatic Region`)) +
  geom_sf(data = sf_oz, fill = 'transparent') +
  scale_fill_manual(values = c('Desert' =  '#FDBF6F',
                               'Cool Temperate' = '#B2DF8A',
                               'Warm Temperate' = '#33A02C',
                               'Savanna' = '#FF7F00',
                               'Mediterranean' = '#FB9A99',
                               'Tropical' = '#A6CEE3')) +
  new_scale_fill() +
  geom_point(data = dom_veg_type,
             mapping = aes(x = x,
                           y = y,
                           shape = `Vegetation Type`,
                           fill = `Vegetation Type`
                           ),
             stroke = 0.1,
             alpha = 0.9,
             size =2.2) +
  scale_fill_manual(values = c('Tree' = 'yellow',
                    'Shrub' = '#E60026',
                    'Grass' = '#0070FF')) + 
  scale_shape_manual(values = c('Grass' = 21,
                                'Shrub' = 24,
                                'Tree' = 22)) +
  labs(fill = 'Vegetation Type\n(Dominant)',
       shape = 'Vegetation Type\n(Dominant)') +
  theme_bw() +
  ylab('') +
  xlab('') +
  ylim(c(-45, -10)) +
  xlim(c(112, 155))

plot(ausplots_map)

ggsave(plot = ausplots_map, 
       filename = 'FIGURES/Figure_1_AusPlots_Map.png',
       height = 10.48,
       width = 14.86,
       units = 'cm',
       scale = 1.3,
       dpi = 600)

