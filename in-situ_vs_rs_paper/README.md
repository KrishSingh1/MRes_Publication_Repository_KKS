## Description of Code

### Data Acquisition Code

**AusPlots_FC_Extraction.R**:
Contains code used to iteratively extract fractional cover from the AusPlots site database using a list of site names. Datasets used:
- *extracted_Final_site_info_2-0-6.csv*
The datasets produced from this code includes:
- *AusPlots_FC_Iter_2_0_6.csv*

**AusPlots_Veg_Extraction_2-0-6.R**:
Contains code to query all AusPlots site level data (e.g. coordinates, point intercept) from AusPlots database. Datasets produced:
- *extracted_Final_site_info_2-0-6.csv*

#### DEA_FC_Extraction folder

**single_site_retrieval_230628.py**:
Contains code used to query and and apply initial preprocessing steps (water mask) for DEA FC data at AusPlots sites. Dataset produced:
- *All DEA FC data is located at 'DATASETS\DEA_FC_PROCESSED\RawDataCurrent\NewBatchCurrent'*

**AusPlots_Merged_Completed.csv:**
Contains site coordinate information used as input for single_site_retrieval_230628.py.

**submit:**
Contains instructions to peform the query over the NCI.

**dea-notebooks:**
Contains code produced by DEA team that allows for query and data preprocessing of DEA FC data. Reference:
Krause, C., Dunn, B., Bishop-Taylor, R., Adams, C., Burton, C., Alger, M., Chua, S., Phillips, C., Newey, V., Kouzoubov, K., Leith, A., Ayers, D., Hicks, A., DEA Notebooks contributors 2021. Digital Earth Australia notebooks and tools repository. Geoscience Australia, Canberra. https://doi.org/10.26186/145234


### Data Preprocessing Code:

**AusPlots_Extract_CornerPoints.R:**
Contains code used to clean coordinate data of the AusPlots sites. If there are missing coordinate points, then they are estimated based on the site's dimensions and their SW point. Datasets used:
- *Published Plot Corners_extract26062024.csv*
  
Dataset Produced:
- `Published Plot Corners_extract26062024_cleaned.csv`

**AusPlots_Growth_Form_by_Height_Rule.R:**
Contains code used to classify AusPlots sites by the dominance in growth form percent cover. It uses datasets:
- *site_veg_Final2-0-6.rds*
- *Growth_Type_Classification.csv*
  
The datsets produced from this code includes:
- *AusPlots_VegType_PC_Height_Rule.csv*
- *AusPlots_Agg_VegType_PC_Height_Rule.csv*

**DEA_FC_Preprocessing.R:**
Contains code used to spatially trim the dimensions of the extracted DEA FC data at each site. Furthermore, to apply a unmixing error filter. Datasets used:
- *All data in: 'DATASETS/DEA_FC_PROCESSED/RawDataCurrent/NewBatchCurrent'*
- *Published Plot Corners_extract26062024_cleaned.csv*
Datasets produced:
- `All spatially trimmed site-level DEA FC data in 'DATASETS/DEA_FC_PROCESSED/SPATIAL_AND_UE_FILTER`

**DEA_Evaluation_Nearest_Point.R:**
Contains code used to compare AusPlots and DEA FC (closest timestamp) for each site visits. It uses datasets:
- *All DEA FC data from: 'DATASETS/DEA_FC_PROCESSED/MODELLED_PREPROCESSED/'*
-*'AusPlots_FC_Iter_2_0_6.csv'*
-*'extracted_Final_site_info_2-0-6.csv'*

Datasets produced:
- `DEA_FC_Ground_Truth_Evaluation.csv`
- `AusPlots_VegType_PC_Height_Rule.csv`
- `AusPlots_Agg_VegType_PC_Height_Rule.csv`

**DEA_Data_Repeated_Sites_Change.R:**
Contained code used to calculate the change in FC between site visits from AusPlots and DEA's FC data (closest timestamp), producing a dataset that allows for comparison between the changes. It used datasets:
- *AusPlots_Agg_VegType_PC_Height_Rule.csv*
  
Datasets produced:
- `Fractional_Cover_Change_Evaluation.csv`

**Thiel-sen_Regression.R:**
Contains code used to perform thiel-sen regression analysis on each site's DEA-FC timeseries. Datasets used:
- *All datasets in 'DATASETS/DEA_FC_PROCESSED/MODELLED_PREPROCESSED/'*
- *extracted_Final_site_info_2-0-6.csv*
  
The dataset produced from this code includes:
- `AusPlots_Theil_Sen_Regression_Stats_Signf.csv`

### Results/Supplementary Code:

**Results_Section_1_Figures.R:**
Contains code used to generate Figures (2-3, S2) for results section: 'Long-term vegetation trends in remotely-sensed FC.' Datasets used
- *AusPlots_Theil_Sen_Regression_Stats_Signf.csv*
- *AusPlots_Agg_VegType_PC_Height_Rule.csv*

**Results_Section_1_Statistical_Analyses.R**
Contains code used to generate ANOVA and Tukey T Test tables (Table S2-4) for results section: 'Long-term vegetation trends in remotely-sensed FC.' This code was also used to label the pairwise significant differences in Figure 3. Datasets used: 
- *AusPlots_Theil_Sen_Regression_Stats_Signf.csv*
- *AusPlots_Agg_VegType_PC_Height_Rule.csv*

**Results_Section_2_Figures.R:**
Contains code used to generate Figures (4-7, S4) for results section: 'Correspondence between AusPlots and DEA FC' Datasets used:
-  *DEA_FC_Ground_Truth_Evaluation_complete_Height_Rule_Agg.csv*
-  *Fractional_Cover_Change_Evaluation.csv*

**Results_Section_3_Figures.R:**
Contains code used to generate Figure 8 for results section: 'Comparison of long-term vegetation trend with changes between site visits.' Datasets used:
- *AusPlots_Theil_Sen_Regression_Stats_Signf.csv*
- *DEA_FC_Ground_Truth_Evaluation.csv*
- *Input_DataSet_SAABHC0004.csv*
- *Input_DataSet_QDAEIU0006.csv*

**SI_Shrub_Tree_Grass_Ternary.R**:
Contains code used to generate Figure S3. Datasets used:
-  *AusPlots_Agg_VegType_PC_Height_Rule.csv*
-  *DEA_FC_Ground_Truth_Evaluation_complete_Height_Rule_Agg.csv*

### Other

**AusPlots_Map.R:**
Contains code used to generate Figure 1. Datasets used:
- *Australian_Bioclimatic_Regions.tif*
- *extracted_Final_site_info_2-0-6.csv*
- *DEA_FC_Ground_Truth_Evaluation_complete_Height_Rule_Agg.csv*

**AusPlots_Change_In_Vegetation_Type.R:**
Code used to explore the change in dominant vegetation type across site visits. Used to support Methods Datasets section 'In-situ vegetation monitoring data.' Datasets used:
- *DEA_FC_Ground_Truth_Evaluation_complete_Height_Rule_Agg.csv*
- *AusPlots_VegType_PC_Height_Rule.csv*
- *site_veg_Final2-0-6.rds*
