####### Automate Analysis #######
## Krish K. Singh
## 13/10/26
## Objective:
## To automate the data preprocessing and analysis in sequence for reproducibility of results
## Note: This will not include AusPlots PI and DEA FC extraction. 


# 01 AusPlots Data Extraction and processing  -------------------------------------------------------------------------------------

# source("01_00_Ausplots_Veg_Extraction_2-0-6.R")      # Obtain AusPlots Vegetation information   
# source("01_01_AusPlots_FC_Extraction.R")             # Calculate AusPlots FC 
source("01_01_AusPlots_Extract_CornerPoints.R")        # Extract Corner coordinates from AusPlots
source("01_01_AusPlots_Growth_Form_by_Height_Rule.R")  # Classify AusPlots Dominant Vegetation forms


# 02 DEA FC Preprocessing -----------------------------------------------------------------------------
# See '/DEA_FC_Extraction' for code to extract DEA FC from NCI 
source("02_00_DEA_FC_Preprocessing.R")                 # Perform Spatial and UE trimming 
source("02_01_Savitizky_Golay_Filter.R")               # Perform Savitizky-Golay Filter on DEA FC 


# 03 Combine DEA and AusPlots -------------------------------------------------------------------------------------


[1] "00_Automate_Analysis.R"                     
[3] "01_AusPlots_FC_Extraction.R"                
[4] "01_Ausplots_Veg_Extraction_2-0-6.R"         
[5] "02_AusPlots_Growth_Form_by_Height_Rule.R"   
[6] "02_DEA_FC_Preprocessing.R"                  
[7] "03_Savitizky_Golay_Filter.R"                
[8] "04_DEA_Evaluation_Nearest_Point.R"          
[9] "05_DEA_Repeat_Site_Visit_Evaluation.R"      
[10] "06_Theil_sen_Regression.R"                  
[11] "07_Results_Section_1_Figures.R"             
[12] "07_Results_Section_1_Statistical_Analyses.R"
[13] "08_Results_Section_2_Figures.R"             
[14] "09_Results_Section_3_Figures.R" 