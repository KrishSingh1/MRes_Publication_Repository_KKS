###### AusPlots Vegetation Data Extraction ######
## Author: Krish Singh
## Date: 240105
## Purpose: To extract all vegetation data from different site locations from AusPlots


# Libraries ---------------------------------------------------------------

library(ausplotsR)

# Functions ---------------------------------------------------------------

# Main --------------------------------------------------------------------
version <- gsub('\\.', '-', packageVersion("ausplotsR"))
my.data <- get_ausplots(site_info=TRUE, structural_summaries=TRUE,
                        veg.vouchers=TRUE, veg.PI=TRUE, basal.wedge=TRUE, soil_subsites=TRUE,
                        soil_bulk_density=TRUE, soil_character=TRUE)

file <- paste0("../DATASETS/AusPlots_Extracted_Data/Final/","site_veg_Final", version, ".rds")
saveRDS(my.data, file = file)