###### AusPlots Vegetation Data Extraction ######
## Author: Krish Singh
## Date: 240105
## Purpose: To extract all vegetation data from different site locations from AusPlots


# Libraries ---------------------------------------------------------------

library(ausplotsR)

# Functions ---------------------------------------------------------------

# Main --------------------------------------------------------------------
version <- gsub('\\.', '-', packageVersion("ausplotsR"))
veg_info <- get_ausplots(site_info=TRUE, structural_summaries=TRUE,
                        veg.vouchers=TRUE, veg.PI=TRUE, basal.wedge=TRUE, soil_subsites=TRUE,
                        soil_bulk_density=TRUE, soil_character=TRUE)

# Process Data ----------------------------------------------------------------------------------------------------

# Transform columns to allow export - record 2 of 'variableLabel' is a list, need to concat the elements
veg_info$metadata.dictionary$variableLabel <- unlist(lapply(veg_info$metadata.dictionary$variableLabel, FUN = function(x){
  if(class(x) == "list"){
    x <- paste(x, collapse = ",")
  }
  return(x)
}))

# Save Outputs ----------------------------------------------------------------------------------------------------
file <- paste0("DATASETS/Test/","site_veg_Final", version, ".rds")
saveRDS(veg_info, file = file) # Save the entire results as an R object 

# Export each dataframe from the downloaded r object as a csv file 
for (i in names(veg_info)) {
  if (i != 'citation') {
    name <- gsub('\\.','_',i) # replace '.' with '_' for file naming
    sub_file <- paste0("DATASETS/Test/","extracted_Final_",name,"_",version ,  ".csv")
    write.csv(veg_info[i], sub_file)
    print(paste0('Exported ', i, ' as a csv file.'))
  }
}