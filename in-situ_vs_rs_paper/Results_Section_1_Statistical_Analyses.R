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
theil_sen_reg <- read.csv('DATASETS/AusPlots_Theil_Sen_Regression_Stats_Signf.csv')

# Vegetation type 
veg_type <- read.csv('DATASETS/AusPlots_Agg_VegType_PC_Height_Rule.csv')

# Combine the datasets
theil_sen_reg <- theil_sen_reg %>%
  left_join(veg_type, by = 'site_location_name')


# Stat Tests ------------------------------------------------------------------
# Test for a difference in mean for each vegetation type in their respective fractional type

grass.pv <- subset(theil_sen_reg, vegetation_type == 'Grass')[,c('pv_filter_slope_yr', 'vegetation_type')]
t.test(grass.pv$pv_filter_slope_yr)
t.test(grass.pv$pv_filter_slope_yr)$p.value

# One Sample t-test
# 
# data:  grass.pv$pv_filter_slope_yr
# t = 11.915, df = 330, p-value < 2.2e-16
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   0.06546962 0.09136290
# sample estimates:
#   mean of x 
# 0.07841626 

shrub.pv <- subset(theil_sen_reg, vegetation_type == 'Shrub')[,c('pv_filter_slope_yr', 'vegetation_type')]
t.test(shrub.pv$pv_filter_slope_yr)
t.test(shrub.pv$pv_filter_slope_yr)$p.value

# One Sample t-test
# 
# data:  shrub.pv$pv_filter_slope_yr
# t = 7.7522, df = 216, p-value = 3.506e-13
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   0.04602106 0.07740122
# sample estimates:
#   mean of x 
# 0.06171114 

tree.pv <- subset(theil_sen_reg, vegetation_type == 'Tree')[,c('pv_filter_slope_yr', 'vegetation_type')]
t.test(tree.pv$pv_filter_slope_yr)
t.test(tree.pv$pv_filter_slope_yr)$p.value

# One Sample t-test
# 
# data:  tree.pv$pv_filter_slope_yr
# t = 6.0807, df = 186, p-value = 6.65e-09
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   0.04492654 0.08807842
# sample estimates:
#   mean of x 
# 0.06650248 

res.aov_pv <- aov(pv_filter_slope_yr ~ vegetation_type, data = theil_sen_reg)
summary(res.aov_pv)

# Df Sum Sq Mean Sq F value Pr(>F)
# vegetation_type   2   0.04 0.02025   1.249  0.287
# Residuals       732  11.86 0.01621   



# NPV ---------------------------------------------------------------------

grass.npv <- subset(theil_sen_reg, vegetation_type == 'Grass')[,c('npv_filter_slope_yr', 'vegetation_type')]
t.test(grass.npv$npv_filter_slope_yr)
t.test(grass.npv$npv_filter_slope_yr)$p.value

# One Sample t-test
# 
# data:  grass.npv$npv_filter_slope_yr
# t = -9.6668, df = 330, p-value < 2.2e-16
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   -0.14015702 -0.09275898
# sample estimates:
#   mean of x 
# -0.116458 

shrub.npv <- subset(theil_sen_reg, vegetation_type == 'Shrub')[,c('npv_filter_slope_yr', 'vegetation_type')]
t.test(shrub.npv$npv_filter_slope_yr)
t.test(shrub.npv$npv_filter_slope_yr)$p.value

# One Sample t-test
# 
# data:  shrub.npv$npv_filter_slope_yr
# t = -6.1908, df = 216, p-value = 2.974e-09
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   -0.09837881 -0.05086360
# sample estimates:
#   mean of x 
# -0.07462121 

tree.npv <- subset(theil_sen_reg, vegetation_type == 'Tree')[,c('npv_filter_slope_yr', 'vegetation_type')]
t.test(tree.npv$npv_filter_slope_yr)
t.test(tree.npv$npv_filter_slope_yr)$p.value

# One Sample t-test
# 
# data:  tree.npv$npv_filter_slope_yr
# t = -4.2289, df = 186, p-value = 3.68e-05
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   -0.07211057 -0.02623279
# sample estimates:
#   mean of x 
# -0.04917168 

res.aov_npv <- aov(npv_filter_slope_yr ~ vegetation_type, data = theil_sen_reg)
summary(res.aov_npv)

# Df Sum Sq Mean Sq F value
# vegetation_type   2  0.588 0.29404   7.865
# Residuals       732 27.366 0.03738        
# Pr(>F)    
# vegetation_type 0.000417 ***
#   Residuals                   
# ---
#   Signif. codes:  
#   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

TukeyHSD(res.aov_npv)

# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = npv_filter_slope_yr ~ vegetation_type, data = theil_sen_reg)
# 
# $vegetation_type
# diff         lwr        upr
# Shrub-Grass 0.04183680  0.00217418 0.08149942
# Tree-Grass  0.06728632  0.02574646 0.10882619
# Tree-Shrub  0.02544953 -0.01985848 0.07075754
# p adj
# Shrub-Grass 0.0358666
# Tree-Grass  0.0004524
# Tree-Shrub  0.3849296


# Bare Soil ---------------------------------------------------------------

grass.bs <- subset(theil_sen_reg, vegetation_type == 'Grass')[,c('bs_filter_slope_yr', 'vegetation_type')]
t.test(grass.bs$bs_filter_slope_yr)
t.test(grass.bs$bs_filter_slope_yr)$p.value

# One Sample t-test
# 
# data:  grass.bs$bs_filter_slope_yr
# t = 3.588, df = 330, p-value = 0.0003837
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   0.02056935 0.07049845
# sample estimates:
#   mean of x 
# 0.0455339 

shrub.bs <- subset(theil_sen_reg, vegetation_type == 'Shrub')[,c('bs_filter_slope_yr', 'vegetation_type')]
t.test(shrub.bs$bs_filter_slope_yr)
t.test(shrub.bs$bs_filter_slope_yr)$p.value

# One Sample t-test
# 
# data:  shrub.bs$bs_filter_slope_yr
# t = 0.75047, df = 216, p-value = 0.4538
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   -0.01684316  0.03755601
# sample estimates:
#   mean of x 
# 0.01035642 

tree.bs <- subset(theil_sen_reg, vegetation_type == 'Tree')[,c('bs_filter_slope_yr', 'vegetation_type')]
t.test(tree.bs$bs_filter_slope_yr)
t.test(tree.bs$bs_filter_slope_yr)$p.value

# One Sample t-test
# 
# data:  tree.bs$bs_filter_slope_yr
# t = -1.1891, df = 186, p-value = 0.2359
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   -0.04043604  0.01002191
# sample estimates:
#   mean of x 
# -0.01520706 

res.aov_bs <- aov(bs_filter_slope_yr ~ vegetation_type, data = theil_sen_reg)
summary(res.aov_bs)

# Df Sum Sq Mean Sq F value  Pr(>F)
# vegetation_type   2   0.47  0.2339   5.315 0.00511
# Residuals       732  32.21  0.0440                
# 
# vegetation_type **
#   Residuals         
# ---
#   Signif. codes:  
#   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

TukeyHSD(res.aov_bs)

# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = bs_filter_slope_yr ~ vegetation_type, data = theil_sen_reg)
# 
# $vegetation_type
# diff         lwr          upr
# Shrub-Grass -0.03517748 -0.07820503  0.007850083
# Tree-Grass  -0.06074096 -0.10580503 -0.015676895
# Tree-Shrub  -0.02556349 -0.07471538  0.023588413
# p adj
# Shrub-Grass 0.1337380
# Tree-Grass  0.0045900
# Tree-Shrub  0.4408474
