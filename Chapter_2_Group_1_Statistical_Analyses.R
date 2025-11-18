##### Statistical Analysises #####
## 25-09-2024
## Krish Singh




# Libraries ----------------------------------------------------------------

library(dplyr)
library(tidyr)
library(broom)
library(flextable)
library(ggpubr)

# Functions ---------------------------------------------------------------


# Main --------------------------------------------------------------------


# Load the main datasets 
theil_sen_reg <- read.csv('../DATASETS/AusPlots_Theil_Sen_Regression_Stats/AusPlots_Theil_Sen_Regression_Stats_Signf.csv')

# Vegetation type 
veg_type <- read.csv('../DATASETS/AusPlots_Extracted_Data/Final/AusPlots_Agg_VegType_PC_Height_Rule.csv')



# Bioclimatic regions
Broader_Classifications = c('Tropical/Savanna', 
                            'Tropical/Savanna', 'Temp/Med', 'Temp/Med','Temp/Med','Desert')
bioclimatic <- read.csv('../DATASETS/Australian Bioclimatic Regions/AusPlots_BioclimaticRegion_Classified.csv')
bioclimatic$bioclimatic_region <- unlist(lapply(bioclimatic$Group_Code, FUN = function(x){
  return(Broader_Classifications[x])
}))
colnames(bioclimatic)[1] <- 'site_location_name'


# Combine the datasets
theil_sen_reg <- theil_sen_reg %>%
  left_join(veg_type, by = 'site_location_name') %>%
  left_join(bioclimatic, by = 'site_location_name')


# Test 1 ------------------------------------------------------------------
# Test for a difference in mean between each fraction 

aov_test <- theil_sen_reg %>% 
  pivot_longer(cols = c("pv_filter_slope", "npv_filter_slope", "bs_filter_slope"), 
               names_to = "fraction_type",
               values_to = "slope_value")

res.aov <- aov(slope_value ~ fraction_type, data = aov_test)
summary(res.aov)

word <- tidy(res.aov)
df_str <- paste(capture.output(write.table(word, sep = "\t", row.names = FALSE, col.names = TRUE)), collapse = "\n")
writeClipboard(df_str)



## Test which ones are different
TukeyHSD(res.aov)

perm <- RVAideMemoire::perm.anova(slope_value ~ fraction_type, data = aov_test, nperm = 1000)
perm
summary(perm)

word <- tidy(TukeyHSD(res.aov))
df_str <- paste(capture.output(write.table(word, sep = "\t", row.names = FALSE, col.names = TRUE)), collapse = "\n")
writeClipboard(df_str)

to_clipboard <- function(table){
  word <- tidy(table)
  df_str <- paste(capture.output(write.table(word, sep = "\t", row.names = FALSE, col.names = TRUE)), collapse = "\n")
  writeClipboard(df_str)
}

# Test 2 ------------------------------------------------------------------
# Test for a difference in mean for each vegetation type in their respective fractional type

# For PV:

res.aov <- aov(pv_filter_slope_yr ~ vegetation_type, data = theil_sen_reg)
summary(res.aov)

word <- tidy(res.aov)
word
df_str <- paste(capture.output(write.table(word, sep = "\t", row.names = FALSE, col.names = TRUE)), collapse = "\n")

writeClipboard(df_str)


to_clipboard(TukeyHSD(res.aov))
TukeyHSD(res.aov)

## Now whether the means of vegetation types are significant

grass.pv <- subset(theil_sen_reg, vegetation_type == 'Grass')[,c('pv_filter_slope_yr', 'vegetation_type')]

t.test(grass.pv$pv_filter_slope_yr)
$p.value

shrub.pv <- subset(theil_sen_reg, vegetation_type == 'Shrub')[,c('pv_filter_slope_yr', 'vegetation_type')]

t.test(shrub.pv$pv_filter_slope_yr)

tree.pv <- subset(theil_sen_reg, vegetation_type == 'Tree')[,c('pv_filter_slope_yr', 'vegetation_type')]

t.test(tree.pv$pv_filter_slope_yr)



# For NPV:

res.aov <- aov(npv_filter_slope_yr ~ vegetation_type, data = theil_sen_reg)
summary(res.aov)
TukeyHSD(res.aov)
to_clipboard(TukeyHSD(res.aov))


grass.npv <- subset(theil_sen_reg, vegetation_type == 'Grass')[,c('npv_filter_slope_yr', 'vegetation_type')]

t.test(grass.npv$npv_filter_slope_yr)
$p.value

shrub.npv <- subset(theil_sen_reg, vegetation_type == 'Shrub')[,c('npv_filter_slope_yr', 'vegetation_type')]

t.test(shrub.npv$npv_filter_slope_yr)
$p.value

tree.npv <- subset(theil_sen_reg, vegetation_type == 'Tree')[,c('npv_filter_slope_yr', 'vegetation_type')]

t.test(tree.npv$npv_filter_slope_yr)
$p.value




# BS:

res.aov <- aov(bs_filter_slope_yr ~ vegetation_type, data = theil_sen_reg)
summary(res.aov)
TukeyHSD(res.aov)
to_clipboard(TukeyHSD(res.aov))


grass.bs <- subset(theil_sen_reg, vegetation_type == 'Grass')[,c('bs_filter_slope_yr', 'vegetation_type')]

t.test(grass.bs$bs_filter_slope_yr)
$p.value

shrub.bs <- subset(theil_sen_reg, vegetation_type == 'Shrub')[,c('bs_filter_slope_yr', 'vegetation_type')]

t.test(shrub.bs$bs_filter_slope_yr)
$p.value

tree.bs <- subset(theil_sen_reg, vegetation_type == 'Tree')[,c('bs_filter_slope_yr', 'vegetation_type')]

t.test(tree.bs$bs_filter_slope_yr)
$p.value

# Grab the means:

means_pv <- theil_sen_reg %>%
  group_by(vegetation_type) %>%
  summarize(pv_mean = mean(pv_filter_slope_yr),
            pv_sd = sd(pv_filter_slope_yr),
            .groups = 'drop')

means_npv <- theil_sen_reg %>%
  group_by(vegetation_type) %>%
  summarize(npv_mean = mean(npv_filter_slope_yr),
            npv_sd = sd(npv_filter_slope_yr),
            .groups = 'drop')

means_bs <- theil_sen_reg %>%
  group_by(vegetation_type) %>%
  summarize(bs_mean = mean(bs_filter_slope_yr),
            bs_sd = sd(bs_filter_slope_yr), 
            .groups = 'drop')

mean_combined <- means_pv %>%
  left_join(means_npv, by = 'vegetation_type') %>%
  left_join(means_bs, by = 'vegetation_type')



# Test 3 ------------------------------------------------------------------
# Test for a difference in means between aggregated bioclimatic groups


# For PV:

res.aov <- aov(pv_filter_slope ~ bioclimatic_region, data = theil_sen_reg)
summary(res.aov)
TukeyHSD(res.aov)
to_clipboard(TukeyHSD(res.aov))

# For NPV:

res.aov <- aov(npv_filter_slope ~ bioclimatic_region, data = theil_sen_reg)
summary(res.aov)
TukeyHSD(res.aov)
to_clipboard(TukeyHSD(res.aov))

# BS:

res.aov <- aov(bs_filter_slope ~ bioclimatic_region, data = theil_sen_reg)
summary(res.aov)
TukeyHSD(res.aov)
to_clipboard(TukeyHSD(res.aov))

# Grab the means:

means_pv <- theil_sen_reg %>%
  group_by(bioclimatic_region) %>%
  summarize(pv_mean = mean(pv_filter_slope_yr), .groups = 'drop')

means_npv <- theil_sen_reg %>%
  group_by(bioclimatic_region) %>%
  summarize(npv_mean = mean(npv_filter_slope_yr), .groups = 'drop')

means_bs <- theil_sen_reg %>%
  group_by(bioclimatic_region) %>%
  summarize(bs_mean = mean(bs_filter_slope_yr), .groups = 'drop')

mean_combined <- means_pv %>%
  left_join(means_npv, by = 'bioclimatic_region') %>%
  left_join(means_bs, by = 'bioclimatic_region')









