#### Ausplots Growth Form by Height Rule ####
## Krish Singh
## 23-04-25


# Libraries ---------------------------------------------------------------

library(ausplotsR)
library(dplyr)
library(fmsb)
library(tidyr)
library(hrbrthemes)
library(RColorBrewer)
library(paletteer)


# Functions ---------------------------------------------------------------


calc_major_growth_form_pc <- function(PI, species_name = '')
{
  total_points <- length(unique(PI$hits_unique))
  PI <- PI %>%
    subset(in_canopy_sky == F) 
  
  # Subset by species naming convention 
  if(species_name == 'SN'){
    PI <-  PI %>%
      subset(!is.na(standardised_name)) 
  } else if (species_name == 'HD') {
    PI <-  PI %>%
      subset(!is.na(herbarium_determination)) 
  } else {
    print('Not valid')
    break
  }
  
  if(nrow(PI) > 0 ){
    hits_per_growth_form <- PI %>%
      mutate(
        major_growth_form = (factor(major_growth_form,
                                    levels=c('Other_Veg','Grass', 'Shrub', 'Tree'),
                                    ordered=TRUE)),
        height = if_else(is.na(height), 0, height)
      ) %>%
      select(major_growth_form, hits_unique, height) %>%
      na.omit() %>%
      group_by(hits_unique) %>%
      slice_max(order_by = data.frame(height, major_growth_form), with_ties = F)
        
    hits_per_growth_form <- (table(hits_per_growth_form$major_growth_form)/
      total_points) * 100

  } else {
    hits_per_growth_form <- NA
  }
  
  return(hits_per_growth_form)
}

get_location_name <- function(site.unique) {
  return(unlist(strsplit(site.unique, split =  '-'))[1])
}

classify <- function(dataset.row) {
  sub <- dataset.row[c("Grass","Shrub","Tree")]
  veg_type <- max(names(which(sub == max(sub)))) # grab veg type associated with the  max values, then priorise the taller growth form
  return(veg_type)
}
# Main --------------------------------------------------------------------

# Load PI Data 
site_veg <- readRDS('../DATASETS/AusPlots_Extracted_Data/Final/site_veg_Final2-0-6.rds')
PI <- site_veg$veg.PI
site_name_example <- 'NTAARP0004'

# Get the growth form names
PI_growth_forms_names <- sort(unique(PI$growth_form)) 
PI_growth_forms_names <- PI_growth_forms_names[which(PI_growth_forms_names != 'NC')]

# Load classification scheme
growth.form.classification <- read.csv("../DATASETS/AusPlots_Extracted_Data/Growth_Type_Classification.csv", header = F) %>%
  mutate(V2 = if_else(is.na(V2), 'Other_Veg', V2)) %>% # <-- convert the NA into 'other'
  rename(major_growth_form = V2) %>%
  mutate(growth_form = PI_growth_forms_names) # <- now the column names and growth form names will follow the same naming scheme
  
# Classify all growth forms
PI_classified <- PI %>% left_join(growth.form.classification, by = 'growth_form')
site_PI_example <- PI_classified  %>%
  subset(site_unique == 'NTAARP0004-58932')

calc_major_growth_form_pc(site_PI_example, species_name = 'HD')

# Case checks 
PI_test1 <- PI_classified  %>%
  subset(site_location_name == 'NTASTU0004') %>% # <-- we want site NTASTU0004
  subset(transect == 'E5-W5') %>% # <-- we want transect E5-W5
  subset(point_number < 11)  # <-- we want the 10th of E5-W5 i.e. first 11 point intercepts

# My expected output is 45.45% Grass and Tree, and 0% Shrub and Other_veg

calc_major_growth_form_pc(PI_test1, species_name = 'HD')

#Other_Veg     Grass     Shrub      Tree 
#  0.00000  45.45455   0.00000  45.45455 
# It is indeed consistent 

PI_test2 <- PI_classified  %>%
  subset(site_unique == 'NSABHC0026-57102') %>% # <-- we want site NTASTU0004
  subset(transect == 'S2-N2') %>% # <-- we want transect E5-W5
  subset(point_number < 11)  # <-- we want the 10th of E5-W5 i.e. first 11 point intercepts


# Expected output: Grass:  36.35 , Shrub: 9.0909, Tree: 0, Other: 0 
calc_major_growth_form_pc(PI_test2, species_name = 'HD')

#Other_Veg     Grass     Shrub      Tree 
# 0.000000 36.363636  9.090909  0.000000 

# Consistent 

PI_test3 <- PI_classified  %>%
  subset(site_unique == 'SAARIV0001-58046') %>% # <-- we want site NTASTU0004
  subset(transect == 'E1-W1') %>% # <-- we want transect E5-W5
  subset(point_number < 21)  # <-- we want the 10th of E5-W5 i.e. first 11 point intercepts

# Expected Output: Tree: 45%, Shub: 5%, Grass: 10% 
calc_major_growth_form_pc(PI_test3, species_name = 'HD')

#Other_Veg     Grass     Shrub      Tree 
#        0        10         5        45 

# Consistent

# Now I want to obtain the major growth forms for all of the site visits 

site_unique <- unique(PI_classified$site_unique) # all site visit ID
growth_form_pc_data <- data.frame() # data to append to 
pb <- txtProgressBar(min = 1, max = length(site_unique), style = 3)
counter <- 1
for (s in site_unique) {
  setTxtProgressBar(pb, counter)
  PI_visit <- PI_classified %>% subset(site_unique == s) # subset by site ID 
  growth_form_pc <- calc_major_growth_form_pc(PI_visit, 'HD')
  if(is.table(growth_form_pc)){ # what to do if the function returns NA due to (no valid PI)
    growth_form_pc['site_unique'] <- s # Append the site visit ID to the dataset
    growth_form_pc_data <- bind_rows(growth_form_pc_data, growth_form_pc) # Append the data to the complete dataset 
  } else {
    print(paste(s, 'has no valid PI'))
  }
  counter <- counter +1
}

growth_form_pc_data <- growth_form_pc_data %>%
  mutate(Grass = as.numeric(Grass),
         Shrub = as.numeric(Shrub),
         Tree = as.numeric(Tree),
         vegetation_type = unlist(
           apply(growth_form_pc_data,
                 MARGIN = 1,
                 FUN = classify)
           ),
         site_location_name = unlist(
           lapply(site_unique,
                  FUN = get_location_name)
           )
         )


# Save the Data 
write.csv(growth_form_pc_data, '../DATASETS/AusPlots_Extracted_Data/Final/AusPlots_VegType_PC_Height_Rule.csv',)

# Now Aggregate by site visit

PC_agg <- aggregate(
  round(growth_form_pc_data[,c('Grass', 'Shrub', 'Tree')],1),
  by = list(growth_form_pc_data$site_location_name),
  FUN = mean
) %>% rename(site_location_name = Group.1) %>%
  mutate(vegetation_type = unlist(apply(., MARGIN = 1, FUN = classify)))

# Save the agg Data 
write.csv(PC_agg, '../DATASETS/AusPlots_Extracted_Data/Final/AusPlots_Agg_VegType_PC_Height_Rule.csv',)