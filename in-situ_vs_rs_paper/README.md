# Code under this Repository

The scripts under this repository were written to extract, process, analyse, and generate figures for the paper 'Correspondence...'. They are numbered by the order in which they should be run, as some scripts depend on the outputs of the previous scripts to work as intended. This is done on two levels, where the main section (first two digits) highlights the main preprocessing stage, while the subsections (second two digits) highlights minor functions. Generally, there are no particular order to which the minor subsections should be run given that they share the same sub-section number (second two digits).

The inputs/outputs of this script are either under the folder 'in-situ_vs_rs_paper/DATASETS' or 'in-situ_vs_rs_paper/FIGURES' for datasets and figures respectively.

For DEA FC Extraction, see [DEA_FC_Extraction folder](#DEA_FC_Extraction folder).

## 00 Running the automated pipeline

### **00_Pipeline.R**

This script runs the entire data processing, analysis, and figure generation pipeline by automatically running most of the scripts below in order. Note that this does not include AusPlots and DEA FC extraction to keep consistent with the database version (AusPlots), and the script for DEA FC extraction requires the National Computational Infrastructure to run.

## 01 AusPlots Extraction

### 01_01_Ausplots_Veg_Extraction_2-0-6.R
- This script queries the AusPlots database to extract data such as plot information, point intercept data, and ancillary data.

Outputs:
- *site_veg_Final2-0-6.rds:* a r data file containing all site data to be extracted from the database including plot information, point intercept data, and ancillary data.
- *extracted_Final_site_info_2-0-6.csv:* a csv file containing just site information data such as coordinates, and site descriptors.  

### 01_02_AusPlots_FC_Extraction.R
- This script extracts PI data from AusPlots and calculates the fractional cover iteratively per site. It also logs the outcomes of each FC calculation with codes (0 = no error, 1 = missing PI data, 2 = fc returns empty char, 3 = partially missing fc row-wise (by visit), 4 = partially missing fc column-wise (either one fc component is missing)).

Inputs:
- *extracted_Final_site_info_2-0-6.csv* ([01_01_Ausplots_Veg_Extraction_2-0-6.R](#01_01_Ausplots_Veg_Extraction_2-0-6.R))

Outputs:
- *AusPlots_FC_Iter_2_0_6.csv'*: A csv file containing in-situ based fractional cover components per site visit, and the outcome of the data extraction (error column).


### 01_02_AusPlots_Extract_CornerPoints.R
- This script outputs a the boundary points (corner points; SW, SE, NW, NE) of each AusPlots sites based on the site information dataset and based on the boundary points given by the AusPlots team. It also performs data cleaning.

Inputs:
- *AusPlots_Published_Corner_Points_20240701/Published Plot Corners_extract26062024.csv*: The corner points data given by the AusPlots Teams 
- *DEA_FC_PROCESSED/RawDataCurrent/NewBatchCurrent/[SITE_LOCATION_NAME].csv*:  (See [DEA_FC_Extraction folder](#DEA_FC_Extraction folder)) The extracted DEA FC datasets obtained by querying the database from NCI. It includes the fraction of photosynthetic, non-photosynthetic, and bare soil, x and y pixel coordinates, timestamp, and a crs reference. Used here to compare what sites are missing from *Published Plot Corners_extract26062024.csv*, that we will need boundary points for.

Outputs:
- *Published Plot Corners_extract26062024_cleaned.csv*: The corner points of each AusPlots sites (SW, SE, NW, NE).

### **01_02_AusPlots_Growth_Form_by_Height_Rule.R**
- This script classifies the vegetation type (grass, shrub, tree) of each site by the plant growth forms dominance from the PI vegetation data using the height rule.

Inputs:
- *GROWTH_FORM_TO_VEG_TYPE_DEFINITIONS/Growth_Type_Classification.csv*: Contains the encoding of growth forms to vegetation type.
- *site_veg_Final2-0-6.rds*: a r data file containing all site data to be extracted from the database including plot information, point intercept data, and ancillary data.

Outputs:
- AusPlots_VegType_PC_Height_Rule.csv: A csv file containing the percent cover of all vegetation types on a site-visit-basis. 
- AusPlots_Agg_VegType_PC_Height_Rule.csv: A csv file containing the percent cover of all vegetation types on a site-basis (i.e. the percent cover as been aggregated across each site visit). 

## 02 DEA FC Processing

### 02_01_DEA_FC_Preprocessing.R
For all extracted site-specific DEA FC data. this script performs spatial trimming and spatial aggregation, unmixing error (ue) filters,  and constrains the the time range between 1988-01-01 to 2023-01-01. There is an optional plotting feature (via the function `trim_to_nearest_coord` ) that allows you to see what pixels are being kept for each site. 

Inputs:
- *DEA_FC_PROCESSED/RawDataCurrent/NewBatchCurrent/[SITE_LOCATION_NAME].csv* (See [DEA_FC_Extraction folder](#DEA_FC_Extraction folder)) 
- Published Plot Corners_extract26062024_cleaned.csv (see 
[01_02_ausplots_Extract_CornerPoints.R](#01_02_ausplots_Extract_CornerPoints.R))

Outputs: 
- *DEA_FC_PROCESSED/SPATIAL_AND_UE_FILTER/[SITE_LOCATION_NAME].csv*: a site specific spatially aggregated timeseries of DEA FC from 1988 to 2022. 

### 02_02_Savitizky_Golay_Filter.R
The script performs further smoothing on the DEA FC data per site iteratively to reduce noise effects. 

Inputs:
- *DEA_FC_PROCESSED/SPATIAL_AND_UE_FILTER/[SITE_LOCATION_NAME].csv* (see [02_01_DEA_FC_Preprocessing.R](#02_01_DEA_FC_Preprocessing.R))

Outputs:
- *DEA_FC_PROCESSED/PROCESSED/[SITE_LOCATION_NAME].csv*: Similar to   *DEA_FC_PROCESSED/SPATIAL_AND_UE_FILTER/[SITE_LOCATION_NAME].csv*. but the fractional cover components are smoothed, with the columns having a '\_filter' suffix. 

### 02_03_Theil_sen_Regression.R
- This script performs a Theil-sen regression analysis over each site's extracted DEA FC timeseries, extracting slope (with CI intervals), and intercept. It also determines the 95% significance by checking if the interval crosses 0 (in-significant) or not (significant). 

Inputs:
- *DEA_FC_PROCESSED/PROCESSED/[SITE_LOCATION_NAME].csv* (See [02_02_Savitizky_Golay_Filter.R](#02_02_Savitizky_Golay_Filter.R) )

Outputs:
- *AusPlots_Theil_Sen_Regression_Stats_Signf.csv*: A csv file containing site-specific theil-sen regression statistics.


## 03 Combining DEA and AusPlots FC data for comparison

### 03_01_DEA_Evaluation_Nearest_Point.R
- This script matches the site visits with the closest DEA FC time point at that site. It also classifies each comparison by site's dominant vegetation type. By default, the vegetation type is based on the average percent cover of vegetation types among visits, but can be visit unique by setting `is_agg` to FALSE. 

Inputs:
- *AusPlots_FC_Iter_2_0_6.csv* (See [01_02_AusPlots_FC_Extraction.R](#01_02_AusPlots_FC_Extraction.R))
- *extracted_Final_site_info_2-0-6.csv* (See [01_01_Ausplots_Veg_Extraction_2-0-6.R](#01_01_Ausplots_Veg_Extraction_2-0-6.R))
- *AusPlots_Agg_VegType_PC_Height_Rule.csv* (See [01_02_AusPlots_Growth_Form_by_Height_Rule.R](#01_02_AusPlots_Growth_Form_by_Height_Rule.R), if `is_agg == TRUE`) OR *AusPlots_VegType_PC_Height_Rule.csv* (See [01_02_AusPlots_Growth_Form_by_Height_Rule.R](#01_02_AusPlots_Growth_Form_by_Height_Rule.R), `is_agg == FALSE`)

Outputs:
- *DEA_FC_Ground_Truth_Evaluation.csv*: A csv file containing in-situ and DEA FC-based fractional cover components (in-situ = green, brown, bare, other, and DEA-FC = pv_filter, npv_filter, bs_filter), site visit id, visit timestamp, and the DEA FC time stamp. 
- *DEA_FC_Ground_Truth_Evaluation_complete_Height_Rule_Agg.csv*: Similar to *DEA_FC_Ground_Truth_Evaluation.csv*, but in long-form (pivoted by the fraction_type the values associated with DEA FC (remote_value) and in-situ (on_ground)) and contains the vegetation type classification, and normalisation of fc cover name (green, brown, bare).
- Potential output: *DEA_FC_Ground_Truth_Evaluation_complete_Height_Rule.csv*: Similar to *DEA_FC_Ground_Truth_Evaluation_complete_Height_Rule_Agg.csv* but the vegetation type classification is visit-specific. 

###  03_02_DEA_Repeat_Site_Visit_Evaluation.R

- This script calculates $\Delta$FC for by taking the difference in FC for each consecutive site visit (valid for repeat site visits). FC includes the in-situ FC and the DEA FC timepoints closest to the visit time.  It also classifies the site records by their dominant vegetation type.

Inputs:
- *DEA_FC_Ground_Truth_Evaluation.csv'* (See [03_01_DEA_Evaluation_Nearest_Point.R](#03_01_DEA_Evaluation_Nearest_Point.R))
- *AusPlots_Agg_VegType_PC_Height_Rule.csv'* (See [01_02_AusPlots_Growth_Form_by_Height_Rule.R](#01_02_AusPlots_Growth_Form_by_Height_Rule.R)

Outputs: 
- *Fractional_Cover_Change_Evaluation.csv*:  A csv file containing the site location name, fraction type, DEA_FC-based value (remote_value), and in-situ-based value (on_ground_value), vegetation_type, time point (time_a) earlier visit, and time point b (time_b) later visit, and the difference between time_a and time_b (in days).

## 04 Analysis and Figure Generation


### 04_01_Results_Section_1_Figures.R
This script plots out:
- *Figure_2_Thiel_Sen_Regression_Maps.png*: The Theil-Sen regression trend significance of each remotely sensed fractional cover type in 1987-2022 for all AusPlots sites. a) photosynthetic vegetation cover, b) non-photosynthetic vegetation cover, and c) bare soil cover. Red and green points represent significant negative or positive trends respectively, while blue points indicate non-significant trends.
- *Figure_3_Boxplots.png*: Violin plots of the Theil-Sen regression trend (%/yr) of remotely sensed fractional cover over the period 1987-2022 for AusPlots sites dominated by grass, shrub, or trees. These plots are grouped by a) photosynthetic vegetation cover, b) non-photosynthetic vegetation cover, and c) bare soil. The red dot is the mean of the respective vegetation type at each panel. Letters above the boxplots indicate the significance between groups as determined by One-way ANOVA and Tukey Test. Each mean trend value underwent a Two-tailed single-sample T Test to determine significant differences from 0. Significance codes indicate p-values: ‘***’: <0.001, ‘**’: < 0.01, ‘*’: <0.05, ‘.’ < 0.1.  See Table S2-4 for the results for these tests.
- *Figure_S2_FCHistogram.png:* Histogram plots of Theil-Sen regression trends (% / year) of remotely sensed fractional cover over the period 1987-2022, grouped by a) photosynthetic vegetation cover, b) non-photosynthetic vegetation cover, c) bare soil cover. The black dashed line represents the mean, and the red dashed lines represents the lower and upper standard deviation difference from the mean.

Inputs:
- AusPlots_Theil_Sen_Regression_Stats_Signf.csv (See [02_03_Theil_sen_Regression.R](#02_03_Theil_sen_Regression.R))
- AusPlots_Agg_VegType_PC_Height_Rule.csv (See [01_02_AusPlots_Growth_Form_by_Height_Rule.R](#01_02_AusPlots_Growth_Form_by_Height_Rule.R))


### 04_01_Results_Section_1_Statistical_Analyses.R
The script performs the following statisitical tests:
- t-test: testing if fractional cover theil-sen slopes are significantly different to 0
- ANOVA: testing if fractional cover theil are significantly different across vegetation types (grass, shrub, trees)
- Tukey Test: distinguishing what pairs of vegetation types are significantly different.

The data tables associated with each test are printed as a .docx table:
- *ANOVA_TABLES/ANOVA_Table.docx*

Inputs:
- AusPlots_Theil_Sen_Regression_Stats_Signf.csv (See [02_03_Theil_sen_Regression.R](#02_03_Theil_sen_Regression.R))
- AusPlots_Agg_VegType_PC_Height_Rule.csv (See [01_02_AusPlots_Growth_Form_by_Height_Rule.R](#01_02_AusPlots_Growth_Form_by_Height_Rule.R))

### 04_01_Results_Section_2_Figures.R
The script plots out:

- *Figure_4_DEA_FC_VS_AusPlots_FC.png:  Figure 4: Correspondence between Digital Earth Australia's Fractional Cover product (y-axis) and AusPlots' measured fractional cover (x-axis). Data for bare soil (BS), non-photosynthetic vegetation (NPV), and photosynthetic vegetation (PV). are shown on the left, middle, and right column respectively. The blue straight line is a fitted linear model with a 95% confidence envelope. The text on each panel is the model’s equation, and R2, and RMSE and mean bias as calculated using the in-situ and remotely sensed data. The red dashed line is a one-to-one line.:
- *Figure_5_Veg_DEA_FC_VS_AusPlots_FC.png:* *Same as Figure 4 but stratified into the three main vegetation types – grass, shrub, and tree.
- *Figure_6_Change_DEA_FC_VS_AusPlots_FC.png: Correspondence between the change in Digital Earth Australia (y-axis) and AusPlots’ (x-axis) Fractional Cover (FC) measurements between timepoints of site visits. The change is calculated based on the difference in FC between consecutive site visits. The change for sites with 3 or more visits are calculated pairwise. DEA data correspond to the closest available timepoint to the site visit. Data for bare soil (BS), non-photosynthetic vegetation (NPV), and photosynthetic vegetation (PV) covers are shown on the left, middle, and right column respectively. The blue straight line is a fitted linear model with a 95% confidence envelope. The text on each panel is the model’s equation and model R2, and RMSE and mean bias as calculated using the in-situ and remotely sensed data. The red dashed line is a one-to-one line.*
- *Figure_7_Change_Veg_DEA_FC_VS_AusPlots_FC.png: *Same as Figure 6 but stratified into the three main vegetation types – grass, shrub, and tree.
- *Figure_S4_Absolute_Error_Vs_Time_Difference.png*: Relationship between the time difference (in days) between site visit and closest DEA FC record, and the absolute error calculated from the difference between remotely-sensed (DEA) and in-situ (AusPlots) fractional cover (FC). The plot is gridded horizontally by the vegetation type (grass, shrub, tree) and vertically fractional cover component (photosynthetic vegetation, non-photosynthetic vegetation, bare soil). On each panel, a linear model was fitted (blue solid line) with a 95% confidence interval. Text shows the linear model equation, p-value, and R2.

Inputs:
- DEA_FC_Ground_Truth_Evaluation_complete_Height_Rule_Agg.csv (See [03_01_DEA_Evaluation_Nearest_Point.R](#03_01_DEA_Evaluation_Nearest_Point.R))

### 04_01_Results_Section_3_Figures.R
The script plots out:
- *Figure_8_Short_vs_Long_Term.png*: Comparison of the linear model trend from AusPlots repeat in-situ data (a-c) and long-term trend of the full DEA FC time series. R2 values are given on each plot. e-f) is the same as a-c), but in-situ data is interchanged with remotely-sensed data at the closest possible timepoint.  Red dashed line, one-to-one line. The FC values are grouped by a and d) photosynthetic vegetation (PV), b and e) non-photosynthetic vegetation cover (NPV), and c and f) bare soil (BS). g-h) shows the PV timeseries of two example sites g) Boolcoomatta Reserve, South Australia (-32.019, 140.527), (code: SAABHC0004), and h) Canyon Resources Reserve, Queensland (-18.468, 143.892) (code: QDAEIU0006). The red lines and text show Theil-Sen trend of the full PV timeseries. Blue lines and text: trend of DEA PV between two site visits. Black lines and text: trend of in-situ PV between two sites visits.

Inputs:
- AusPlots_Theil_Sen_Regression_Stats_Signf.csv (See [02_03_Theil_sen_Regression.R](#02_03_Theil_sen_Regression.R))
- DEA_FC_Ground_Truth_Evaluation.csv (See [03_01_DEA_Evaluation_Nearest_Point.R](#03_01_DEA_Evaluation_Nearest_Point.R))
- DEA_FC_PROCESSED/PROCESSED/SAABHC0004.csv (see [02_01_DEA_FC_Preprocessing.R](#02_01_DEA_FC_Preprocessing.R))
- DEA_FC_PROCESSED/PROCESSED/QDAEIU0006.csv (see [02_01_DEA_FC_Preprocessing.R](#02_01_DEA_FC_Preprocessing.R))


## SI and other main text supporting analyses


### SI_Grass_Shrub_Tree_Ternary.R

The script plots out:
- *Figure_S3_Ternary_Plot.png*: Ternary Plot of the percent cover of grass, shrub, and tree at AusPlots sites. The datapoints are normalised such that these percent covers sum to 100% at each site. They are coloured by the field-derived photosynthetic vegetation (PV) cover. The red triangle intersects with the 50% cover of each vegetation type.

Inputs:
- *AusPlots_Agg_VegType_PC_Height_Rule.csv* (See [01_02_AusPlots_Growth_Form_by_Height_Rule.R](#01_02_AusPlots_Growth_Form_by_Height_Rule.R))
- *DEA_FC_Ground_Truth_Evaluation_complete_Height_Rule_Agg.csv* (See [03_01_DEA_Evaluation_Nearest_Point.R](#03_01_DEA_Evaluation_Nearest_Point.R))


### SI_AusPlots_Change_In_Vegetation_Type.R
Script used to explore transitions in dominant vegetation types across site visits. The figures generated were not directly included in the paper, but was used to aid in the exploration of vegetation type changes. 

Inputs:
- DEA_FC_Ground_Truth_Evaluation_complete_Height_Rule_Agg.csv (See [03_01_DEA_Evaluation_Nearest_Point.R](#03_01_DEA_Evaluation_Nearest_Point.R))
- AusPlots_VegType_PC_Height_Rule.csv (See [01_02_AusPlots_Growth_Form_by_Height_Rule.R](#01_02_AusPlots_Growth_Form_by_Height_Rule.R))
- site_veg_Final2-0-6.rds ([01_01_Ausplots_Veg_Extraction_2-0-6.R](#01_01_Ausplots_Veg_Extraction_2-0-6.R))

Outputs (under 'Supplementary_Information')
- *site_visit_counts.png:* A Bar plot showing the frequency of the number of visits of each site. It is further divided by  the dominant vegetation type. 
- *site_veg_change_dist.png:* A barplot showing the frequency of vegetation dominance change per vegetation type. 
- *site_transitions_plot.png:* A barplot showing the frequency of vegetation transitions. 
- *change_in_veg_dominance.png:* A lolipop chart showing the transition of vegetation type per site, with the y-axis showing vegetation type, x axis the percent cover, the points are coloured by the visit number (1-3, yellow to red gradient, the x dashed lines indicates the change in dominance )
- *pc_per_visit_trends_box_plt.png:* A boxplot showing the trend in the change in vegetation type cover per site visit. 

### SI_AusPlots_Map.R
This script plots out:
- *Figure_1_AusPlots_Map.png*: AusPlots sites on the Australian map overlaid by bioclimatic regions by Hutchinson et al. (2005) aggregated as in (Haverd et al., 2013). Sites are categorised by the dominant vegetation type (grass, shrub, and tree) present at the sites. Vegetation type dominance is defined by the vegetation type with the largest vegetation cover averaged over all site visits.

Inputs:
- *BIOCLIMATIC_REGIONS/Australian_Bioclimatic_Regions.tif*: A tif file containing the boundaries of Australian bioclimatic regions as defined by Hutchinson et al. (2005) aggregated as in (Haverd et al., 2013). 
- *extracted_Final_site_info_2-0-6.csv* ([01_01_Ausplots_Veg_Extraction_2-0-6.R](#01_01_Ausplots_Veg_Extraction_2-0-6.R))


## DEA_FC_Extraction folder

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

