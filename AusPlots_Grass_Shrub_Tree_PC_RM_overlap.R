### AusPlots Dominant Growth-Form Calc
# 14-04-2025
# Krish Karan Singh



# Libraries ---------------------------------------------------------------

library(dplyr)
library(ausplotsR)
library(plotly)
library(plot3D)
library(ozmaps)
library(hrbrthemes)
library(GGally)
library(viridis)
library(Ternary)


# Functions ---------------------------------------------------------------

calc_growth_form_pc <- function(PI, growthform = 'major_growth_form', species_name = '') {
  
  total_points = length(unique(PI$hits_unique))
  PI <- PI %>%
    subset(in_canopy_sky == F)
  
  if(species_name == 'SN'){
    PI <-  PI %>%
      subset(!is.na(standardised_name)) 
  } else if (species_name == 'HD') {
    PI <-  PI %>%
      subset(!is.na(herbarium_determination)) 
  }
    
  if(nrow(PI) > 0 ){
     unique_hit_table <- table(unique(PI[,c('hits_unique', growthform)]))
     hits_per_growth_form <- (colSums(unique_hit_table)/total_points) * 100
  } else {
    hits_per_growth_form <- NA 
  }
  
  return(hits_per_growth_form)
}

classify <- function(dataset.row) {
  return(names(which.max(dataset.row[c("Grass","Shrub","Tree")])))
}

# Set-up ------------------------------------------------------------------

# Load Point Intercept Data
veg_info <- readRDS("../DATASETS/AusPlots_Extracted_Data/Final/site_veg_Final2-0-6.rds")
PI <- veg_info$veg.PI
PI_growth_forms_names <- sort(unique(PI$growth_form)) 
PI_growth_forms_names <- PI_growth_forms_names[which(PI_growth_forms_names != 'NC')]

# Load classification scheme
growth.form.classification <- read.csv("../DATASETS/AusPlots_Extracted_Data/Growth_Type_Classification.csv", header = F) %>%
  mutate(V2 = if_else(is.na(V2), 'Other', V2)) %>% # <-- convert the NA into 'other'
  rename(major_growth_form = V2) %>%
  mutate(growth_form = PI_growth_forms_names) # <- now the column names and growth form names will follow the same naming scheme

PI_classified <- PI %>% left_join(growth.form.classification, by = 'growth_form')



# Main --------------------------------------------------------------------

# Test case: 

PI_test1 <- veg_info$veg.PI %>%
  subset(site_location_name == 'NTASTU0004') %>% # <-- we want site NTASTU0004
  subset(transect == 'E5-W5') %>% # <-- we want transect E5-W5
  subset(point_number < 11) %>%  # <-- we want the 10th of E5-W5 i.e. first 11 point intercepts
  left_join(growth.form.classification, by = 'growth_form') %>%
  filter(!in_canopy_sky)
  
# Expected: 
# Grass     Shrub      Tree 
# 0.8181818 0.1818182 0.4545455 

colSums(table(unique(PI_test1[,c('hits_unique', 'major_growth_form')])))/11

# Indeed it does show!

# Now for a whole visit

PI_test2 <- veg_info$veg.PI %>%
  subset(site_location_name == 'NTASTU0004') %>% # <-- we want site NTASTU0004
  left_join(growth.form.classification, by = 'growth_form') %>%
  filter(!in_canopy_sky)

calc_growth_form_pc(PI_test2)

# Now for a whole dataset 

PI_classified <- PI %>% left_join(growth.form.classification, 
                                  by = 'growth_form')

site_unique <- unique(PI_classified$site_unique)

pc_data <- data.frame()

pb <- txtProgressBar(min = 1, max = length(site_unique), style = 3)
counter <- 1

for(s in c(site_unique)) {
  setTxtProgressBar(pb, counter)
  PI_s <- PI_classified %>%
    subset(site_unique == s)
  data <- calc_growth_form_pc(PI_s, 'major_growth_form', 'SN')
  if(class(data) == 'logical') { # check if the data returned with 'NA'
    print(paste('Skip:', s ))
  } else {
    data_row <- as.data.frame(t(data))
    data_row['site_unique'] <- s
  }
  pc_data <- bind_rows(pc_data, data_row)
  counter <- counter + 1 
}

row.names(pc_data) <- 1:nrow(pc_data) # clean up row names 
pc_data['site_location_name'] <- unlist( # Get site_location_name
  lapply(pc_data$site_unique, FUN = function(x){
    unlist(strsplit(x, '-'))[1]
  })
) 

pc_data <- pc_data %>% 
  replace(is.na(.), 0) # Make any instance of NA into 0

version <- gsub('\\.', '-', packageVersion("ausplotsR"))
file.pathd <- paste0('../DATASETS/AusPlots_Extracted_Data/Final/','AusPlots_Sites_Growth_Form_No_Overlap', version, '.csv')
write.csv(pc_data,file.pathd)

# For major growth forms: Now to aggregate the data by the mean 

PC_agg <- aggregate(
  pc_data[,c('Grass', 'Shrub', 'Tree', 'Other')],
  by = list(pc_data$site_location_name),
  FUN = mean
) %>% rename(site_location_name = Group.1) %>%
  mutate(vegetation_type = unlist(apply(., MARGIN = 1, FUN = classify)))

version <- gsub('\\.', '-', packageVersion("ausplotsR"))
file.pathd <- paste0('../DATASETS/AusPlots_Extracted_Data/Final/','AusPlots_Sites_Classified_No_Overlap', version, '.csv')
write.csv(PC_agg,file.pathd)

# Comparing the differences between the older classification data 

class_order <- c('grass', 'shrub', 'tree')

ausplots_data <- read.csv('../DATASETS/AusPlots_Extracted_Data/Final/AusPlots_Sites_Classified_2-0-6.csv') %>%
  mutate(original_veg_type = vegetation_type) %>%
  select(site_location_name, original_veg_type, grass, shrub, tree)

setdiff(PC_agg$site_location_name, ausplots_data$site_location_name)

# Interestingly, I got more 40 sites than I originally had if I didn't chose to exclude
# sites based on if they have standardised names

# [1] "NSANET0002" "NSANET0003" "NSANET0004" "QDADEU0002" "QDADEU0003" "QDADEU0004" "QDADEU0005"
# [8] "QDADEU0006" "QDAGUP0032" "SAACHC0003" "SAACHC0004" "SAACHC0005" "SAACHC0006" "SAACHC0008"
# [15] "SAACHC0009" "SAACHC0010" "SAACHC0011" "SAACHC0012" "SAACHC0014" "SAAEYB0015" "SAAEYB0016"
# [22] "SAAEYB0035" "SAAEYB0036" "SAAKAN0013" "SAANUL0005" "SAARIV0010" "WAAGSD0002" "WAAGSD0003"
# [29] "WAAGSD0004" "WAAGVD0007" "WAAGVD0008" "WAAGVD0009" "WAAGVD0010" "WAAGVD0011" "WAAGVD0012"
# [36] "WAAGVD0014" "WAAGVD0015" "WAAGVD0016" "WAAGVD0017" "WAALSD0004"

# Reduce into the common sites

common <- intersect(PC_agg$site_location_name, ausplots_data$site_location_name)

PC_agg_common <- PC_agg %>%
  subset(site_location_name %in% common)

PC_agg_common_compare <- PC_agg %>%
  left_join(ausplots_data) %>% 
  mutate(Grass = round(Grass,2),
         Shrub = round(Shrub, 2),
         Tree = round(Tree,2))

conflics <- which(tolower(PC_agg_common_compare$vegetation_type) !=
  tolower(PC_agg_common_compare$original_veg_type))

PC_agg_common_compare[conflics, ] 

knitr::kable(PC_agg_common_compare[conflics, c('site_location_name', 'Grass','Shrub', 'Tree', 'grass', 'shrub', 'tree', 'original_veg_type', 'vegetation_type')] , format = "markdown")

# Visualisation -----------------------------------------------------------


fig <- plot_ly(PC_agg, x = ~Grass, y = ~Shrub, z = ~Tree, alpha = 0.9)
fig <- fig %>% add_markers()
fig

p <- plot_ly(
  PC_agg, x = ~Grass, y = ~Shrub, z = ~Tree,
  type='mesh3d', intensity = ~Tree,
  colorscale = c('#FFE1A1', 'darkgreen')
)
p

p <- plot_ly(
  PC_agg, x = ~Grass, y = ~Shrub, color = ~Tree
)
p


scatter3D(x = PC_agg$Grass, y = PC_agg$Shrub, z = PC_agg$Tree,
          pch = 18, bty = "u")




ggparcoord(PC_agg,
           columns = 2:4,
           scale="globalminmax",
           showPoints = TRUE, 
           title = "No scaling",
           alphaLines = 0.3
) + 
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum()+
  theme(
    legend.position="none",
    plot.title = element_text(size=13)
  ) +
  xlab("")


pairs(PC_agg[, c("Grass", "Shrub", "Tree")], 
      main = "Scatterplot Matrix")



# TernaryPlot, but I am cautious because grass, shrub, and tree do not sum to a constant!
# I can normalise the data, but is this appropriate?

class_order <- c('Grass', 'Shrub', 'Tree')

PC_agg_norm <- PC_agg
PC_agg_norm$row_sums <- rowSums(PC_agg_norm[,2:4])
PC_agg_norm[,class_order] <- PC_agg_norm[,class_order]/ PC_agg_norm$row_sums

fc <- read.csv('../DATASETS/AusPlots_Extracted_Data/Final/DEA_FC_Ground_Truth_Evaluation_complete.csv') %>%
  subset(fraction_type == 'green') %>%
  select(c(site_location_name, on_ground_value)) %>%
  group_by(site_location_name) %>%
  summarise(mean_pv = mean(on_ground_value)) %>%
  as.data.frame()
  
PC_agg_norm <- PC_agg_norm %>% left_join(fc) %>%
  na.omit()

#
PC_agg_pv_test <- PC_agg %>% left_join(fc) %>%
  na.omit() %>% left_join(veg_info$site.info) %>%
  select(c('site_location_name', 'Grass', 'Shrub', 'Tree', 'Other', 'mean_pv',
           'latitude', 'longitude')) %>%
  unique()

write.csv(PC_agg_pv_test,'C:/Users/krish/Desktop/Growth_Form_PC_unscaled.csv')

#

spectrumBins <- 100 # Number of bins to use
mySpectrum <- hcl.colors(spectrumBins, palette = "viridis")
binnedGrowthForm <- cut(PC_agg_norm$mean_pv, spectrumBins)
pointCol <- mySpectrum[binnedGrowthForm]

TernaryPlot(alab = class_order[1], 
            blab = class_order[2], 
            clab = class_order[3])
TernaryPoints(coordinates = PC_agg_norm[,class_order], col = pointCol, pch = 16)


# Legend for colour scale
PlotTools::SpectrumLegend(
  "topleft",
  cex = 0.8, # Font size
  palette = mySpectrum,
  legend = paste(
    round(seq(from = max(PC_agg_norm$mean_pv, spectrumBins), to = min(0, spectrumBins),
        length.out = 5)),
    "%"
  ),
  bty = "n", # No framing box
  xpd = NA, # Don't clip at margins
  # title.font = 2, # Bold.  Argument only available in R>3.6
  title = "Photosynthetic Vegetation Cover (%)"
)



# Validation with Growth forms 

PI_classified <- PI %>% left_join(growth.form.classification, 
                                  by = 'growth_form')

site_unique <- unique(PI_classified$site_unique)

pc_data <- data.frame()

pb <- txtProgressBar(min = 1, max = length(site_unique), style = 3)
counter <- 1

for(s in c(site_unique)) {
  setTxtProgressBar(pb, counter)
  PI_s <- PI_classified %>%
    subset(site_unique == s)
  data <- calc_growth_form_pc(PI_s, 'growth_form', 'SN')
  if(class(data) == 'logical') { # check if the data returned with 'NA'
    print(paste('Skip:', s ))
  } else {
    data_row <- as.data.frame(t(data))
    data_row['site_unique'] <- s
  }
  pc_data <- bind_rows(pc_data, data_row)
  counter <- counter + 1 
}

row.names(pc_data) <- 1:nrow(pc_data) # clean up row names 
pc_data['site_location_name'] <- unlist( # Get site_location_name
  lapply(pc_data$site_unique, FUN = function(x){
    unlist(strsplit(x, '-'))[1]
  })
) 

pc_data <- pc_data %>% 
  replace(is.na(.), 0) # Make any instance of NA into 0

pc_data[,!(names(pc_data) %in% c('site_location_name', 'site_unique'))] <- 
  round(pc_data[,!(names(pc_data) %in% c('site_location_name', 'site_unique'))],1) # so its to the same rounding as the package

PC_agg <- aggregate(
  pc_data[,!(names(pc_data) %in% c('site_location_name', 'site_unique'))],
  by = list(pc_data$site_location_name),
  FUN = mean
) %>% rename(site_location_name = Group.1) %>%
  arrange(site_location_name) %>%
  select(order(colnames(.)))
  
package_growth_form <- growth_form_table(PI, m_kind = "percent_cover",
                                         cover_type = "PFC", species_name = "SN", 
                                         cumulative = F)
package_growth_form$site_location_name <-  unlist( # Get site_location_name
  lapply(row.names(package_growth_form), FUN = function(x){
    unlist(strsplit(x, '-'))[1]
  })
) 
package_growth_form_agg <- package_growth_form %>%
  group_by(site_location_name) %>%
  summarise(across(everything(), mean)) %>%
  arrange(site_location_name) %>%
  select(order(colnames(.)))

sum(PC_agg$site_location_name != package_growth_form_agg$site_location_name)
names(package_growth_form_agg)
names(PC_agg)
# Site_location names are in the right order 
# Column names are in the right order 

summary(PC_agg[,-17] - package_growth_form_agg[,-17]) # round 


# Weird Cases -------------------------------------------------------------

# SAARIV0007 -> Was simply a bug in my code where columns had misaligned data (its fixed now!)
# QDAGUP0029 -> In AusPlot's function, they excluded the second visit, while I did not 
#              -> 'HD' v 'SN' 

wc <- PI_classified %>% subset(
  site_location_name == 'QDAGUP0029'
)
unique(wc$site_unique)

wc2 <- PI_classified %>% subset(
  site_unique == 'QDAGUP0029-53488'
)

calc_major_growth_form_pc(wc2)

wc3 <- PI_classified %>% subset(
  site_unique == 'QDAGUP0029-59025'
)

calc_major_growth_form_pc(wc3)


calc_growth_form_pc(wc2)*100
calc_major_growth_form_pc(wc2)*100

calc_growth_form_pc(wc3)*100

length(unique(wc3$hits_unique))

 growth_form_table(wc3, m_kind = "percent_cover",
                  cover_type = "PFC", species_name = "HD", cumulative = F)

# QDABBS0001
 
 
 # Something worth flagging 
 wc <- PI_classified %>% subset(
   site_location_name == 'QDABBS0001'
 )
 unique(wc$standardised_name) # two NA values: NA and 'Na'
 unique(wc$standardised_name[which(!is.na(wc$standardised_name))]) # 'Na' is still included 
 
 
 wc_test <- wc %>% 
   mutate(standardised_name = if_else(
     tolower(standardised_name) == 'na', 
     NA, standardised_name))
 
 growth_form_table(wc, m_kind = "percent_cover",
                   cover_type = "PFC", species_name = "SN", cumulative = F)
 
 growth_form_table(wc_test, m_kind = "percent_cover",
                   cover_type = "PFC", species_name = "SN", cumulative = F)
 
 ### For all comparisons between all sites I have 
 
all_growth_forms <-  growth_form_table(PI, m_kind = "percent_cover",
                                        cover_type = "PFC", species_name = "SN", cumulative = F)
 
PI_test <- PI %>%  mutate(standardised_name = if_else(
   tolower(standardised_name) == 'na', 
   NA, standardised_name))
 
all_growth_forms_test <- growth_form_table(PI_test, m_kind = "percent_cover",
                                            cover_type = "PFC", species_name = "SN", cumulative = F)

setdiff(colnames(all_growth_forms), colnames(all_growth_forms_test))
# [1] "Fungus"    "Tree.fern", all_growth_forms_test appear to have excluded these growth-forms 

compare <- all_growth_forms[,colnames(all_growth_forms_test)]
compare <- all_growth_forms[,colnames(all_growth_forms_test)] - all_growth_forms_test

 