##### Statistical Analysises #####
## 25-09-2024
## Krish Singh

# Libraries ----------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggpubr)

# Functions ---------------------------------------------------------------

# Main --------------------------------------------------------------------
# Load the main datasets 
theil_sen_reg <- read.csv('../DATASETS/AusPlots_Theil_Sen_Regression_Stats/AusPlots_Theil_Sen_Regression_Stats_Signf.csv')

# Vegetation type 
veg_type <- read.csv('../DATASETS/AusPlots_Extracted_Data/Final/AusPlots_Agg_VegType_PC_Height_Rule.csv')

# Combine the datasets
theil_sen_reg <- theil_sen_reg %>%
  left_join(veg_type, by = 'site_location_name')


# Preliminary Test --------------------------------------------------------
# Normal Distribution

shapiro.test(theil_sen_reg$pv_filter_slope_yr)
shapiro.test(theil_sen_reg$npv_filter_slope_yr)
shapiro.test(theil_sen_reg$bs_filter_slope_yr)

ks.test(theil_sen_reg$pv_filter_slope_yr, 'pnorm')

qqnorm(theil_sen_reg$pv_filter_slope_yr)
qqnorm(theil_sen_reg$npv_filter_slope_yr)
qqnorm(theil_sen_reg$bs_filter_slope_yr)

ggdensity(theil_sen_reg$pv_filter_slope_yr, 
          main = "PV",
          xlab = "%PV/yr")

ggdensity(theil_sen_reg$npv_filter_slope_yr, 
          main = "NPV",
          xlab = "%NPV/yr")

ggdensity(theil_sen_reg$bs_filter_slope_yr, 
          main = "BS",
          xlab = "%BS/yr")

ggqqplot(theil_sen_reg$pv_filter_slope_yr)
ggqqplot(theil_sen_reg$npv_filter_slope_yr)
ggqqplot(theil_sen_reg$bs_filter_slope_yr)
hist(theil_sen_reg$bs_filter_slope_yr)
hist(theil_sen_reg$pv_filter_slope_yr)
hist(theil_sen_reg$npv_filter_slope_yr)

# Test 1 ------------------------------------------------------------------
# Test for a difference in mean for each vegetation type in their respective fractional type

# For PV:

res.aov_pv <- aov(pv_filter_slope_yr ~ vegetation_type, data = theil_sen_reg)
summary(res.aov_pv)
TukeyHSD(res.aov_pv)

## Now whether the means of vegetation types are significant

grass.pv <- subset(theil_sen_reg, vegetation_type == 'Grass')[,c('pv_filter_slope_yr', 'vegetation_type')]
t.test(grass.pv$pv_filter_slope_yr)
t.test(grass.pv$pv_filter_slope_yr)$p.value

shrub.pv <- subset(theil_sen_reg, vegetation_type == 'Shrub')[,c('pv_filter_slope_yr', 'vegetation_type')]
t.test(shrub.pv$pv_filter_slope_yr)
t.test(shrub.pv$pv_filter_slope_yr)$p.value

tree.pv <- subset(theil_sen_reg, vegetation_type == 'Tree')[,c('pv_filter_slope_yr', 'vegetation_type')]
t.test(tree.pv$pv_filter_slope_yr)
t.test(tree.pv$pv_filter_slope_yr)$p.value

# For NPV:
res.aov_npv <- aov(npv_filter_slope_yr ~ vegetation_type, data = theil_sen_reg)
summary(res.aov_npv)
TukeyHSD(res.aov_npv)

grass.npv <- subset(theil_sen_reg, vegetation_type == 'Grass')[,c('npv_filter_slope_yr', 'vegetation_type')]
t.test(grass.npv$npv_filter_slope_yr)
t.test(grass.npv$npv_filter_slope_yr)$p.value

shrub.npv <- subset(theil_sen_reg, vegetation_type == 'Shrub')[,c('npv_filter_slope_yr', 'vegetation_type')]
t.test(shrub.npv$npv_filter_slope_yr)
t.test(shrub.npv$npv_filter_slope_yr)$p.value

tree.npv <- subset(theil_sen_reg, vegetation_type == 'Tree')[,c('npv_filter_slope_yr', 'vegetation_type')]
t.test(tree.npv$npv_filter_slope_yr)
t.test(tree.npv$npv_filter_slope_yr)$p.value

# BS:
res.aov_bs <- aov(bs_filter_slope_yr ~ vegetation_type, data = theil_sen_reg)
summary(res.aov_bs)
TukeyHSD(res.aov_bs)

grass.bs <- subset(theil_sen_reg, vegetation_type == 'Grass')[,c('bs_filter_slope_yr', 'vegetation_type')]
t.test(grass.bs$bs_filter_slope_yr)
t.test(grass.bs$bs_filter_slope_yr)$p.value

shrub.bs <- subset(theil_sen_reg, vegetation_type == 'Shrub')[,c('bs_filter_slope_yr', 'vegetation_type')]
t.test(shrub.bs$bs_filter_slope_yr)
t.test(shrub.bs$bs_filter_slope_yr)$p.value

tree.bs <- subset(theil_sen_reg, vegetation_type == 'Tree')[,c('bs_filter_slope_yr', 'vegetation_type')]
t.test(tree.bs$bs_filter_slope_yr)
t.test(tree.bs$bs_filter_slope_yr)$p.value

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

