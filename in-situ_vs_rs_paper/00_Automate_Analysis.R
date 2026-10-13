####### Automate Analysis #######
## Krish K. Singh
## 13/10/26
## Objective:
## To automate the data preprocessing and analysis in sequence for reproducibility of results
## Note: This will not include AusPlots PI and DEA FC extraction. 


# 01 AusPlots Data Extraction and processing  -------------------------------------------------------------------------------------
# source("01_01_Ausplots_Veg_Extraction_2-0-6.R")         # Obtain AusPlots Vegetation information   
# source("01_02_AusPlots_FC_Extraction.R")                # Calculate AusPlots FC 
# source("01_02_AusPlots_Extract_CornerPoints.R")         # Extract Corner coordinates from AusPlots
source("01_02_AusPlots_Growth_Form_by_Height_Rule.R")     # Classify AusPlots Dominant Vegetation forms

# 02 DEA FC Preprocessing -----------------------------------------------------------------------------
# See '/DEA_FC_Extraction' for code to extract DEA FC from NCI 
source("02_01_DEA_FC_Preprocessing.R")                    # Perform Spatial and UE trimming 
source("02_02_Savitizky_Golay_Filter.R")                  # Perform Savitizky-Golay Filter on DEA FC
source("02_03_Theil_sen_Regression.R")                    # Perform Theil-Sen Regression on filtered FC

# 03 Combine DEA and AusPlots -------------------------------------------------------------------------------------
source("03_01_DEA_Evaluation_Nearest_Point.R")            # Align In-situ FC survey times with DEA FC
source("03_02_DEA_Repeat_Site_Visit_Evaluation.R")        # Calculate Delta FC from both AusPlots and DEA

# 04 Results and Visualisations (with minor data preprocessing) ------------------------------------------------------
source("04_01_Results_Section_1_Figures.R")               # Plot Theil-Sen Maps and Boxplots 
source("04_01_Results_Section_1_Statistical_Analyses.R")  # Perform ANOVA and Tukey-T test on Theil-Sen Slopes 
source("04_01_Results_Section_2_Figures.R" )              # Plot DEA FC against AusPlots 
source("04_01_Results_Section_3_Figures.R" )              # Plot long-term trends in DEA FC vs AusPlots

# SI Plots or Numbers Relevant to the study ----------------------------------------------------------------------------------

source("SI_AusPlots_Change_In_Vegetation_Type.R")         # Calculate how many Plots changed Veg dominance 
source("SI_Grass_Shrub_Tree_Ternary.R" )                  # Plot a Grass-Tree Ternary Plot 
source("SI_AusPlots_Map.R" )                              # Plot spatial distribution of AusPlots