##### Statistical Analysises #####
## 25-09-2024
## Krish Singh

# Libraries ----------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggpubr)
library(purrr)
library(tibble)
library(flextable)
library(officer)

# Functions ---------------------------------------------------------------

# Function to extract ANOVA results
extract_anova <- function(model, variable_name) {
  
  aov_tab <- summary(model)[[1]]
  
  tibble(
    Variable = c(variable_name, variable_name),
    Source = rownames(aov_tab),
    Df = aov_tab$Df,
    Sum_Squares = round(aov_tab$`Sum Sq`,4),
    Mean_Squares = round(aov_tab$`Mean Sq`,4),
    F_value = round(c(aov_tab$`F value`[1], NA),4),
    P_value = round(c(aov_tab$`Pr(>F)`[1], NA),4)
  )
}

extract_tukey <- function(model, variable_name) {
  tuk <- as.data.frame(TukeyHSD(model)$vegetation_type)

  tibble(
    Variable = c(variable_name, "", ""),
    Vegetation_Type = rownames(tuk),
    Mean_Difference = round(tuk$diff, 4),
    Lower_Interval = round(tuk$lwr, 4),
    Upper_Interval = round(tuk$upr, 4),
    Adjusted_P = round(tuk$`p adj`, 4)
    )
}

extract_ttest <- function(x, variable, veg_type){
  
  tt <- t.test(x)

  tibble(
    Variable = variable,
    Vegetation_Type = veg_type,
    Mean = round(tt$estimate, 4),
    t_value = round(tt$statistic, 2),
    df = round(tt$parameter, 0),
    Lower_95CI = round(tt$conf.int[1], 4),
    Upper_95CI = round(tt$conf.int[2], 4),
    P_value = round(tt$p.value,4)
  )
}

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
# t = 12.411, df = 330, p-value < 2.2e-16
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   0.06912020 0.09515951
# sample estimates:
#   mean of x 
# 0.08213986

shrub.pv <- subset(theil_sen_reg, vegetation_type == 'Shrub')[,c('pv_filter_slope_yr', 'vegetation_type')]
t.test(shrub.pv$pv_filter_slope_yr)
t.test(shrub.pv$pv_filter_slope_yr)$p.value

# One Sample t-test
# 
# data:  shrub.pv$pv_filter_slope_yr
# t = 7.8976, df = 216, p-value = 1.425e-13
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   0.04734594 0.07883745
# sample estimates:
#   mean of x 
# 0.0630917 

tree.pv <- subset(theil_sen_reg, vegetation_type == 'Tree')[,c('pv_filter_slope_yr', 'vegetation_type')]
t.test(tree.pv$pv_filter_slope_yr)
t.test(tree.pv$pv_filter_slope_yr)$p.value

# One Sample t-test
# 
# data:  tree.pv$pv_filter_slope_yr
# t = 6.3073, df = 186, p-value = 2.018e-09
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   0.04769678 0.09111402
# sample estimates:
#   mean of x 
# 0.0694054 

res.aov_pv <- aov(pv_filter_slope_yr ~ vegetation_type, data = theil_sen_reg)
summary(res.aov_pv)

# Df Sum Sq Mean Sq F value Pr(>F)
# vegetation_type   2  0.051 0.02566   1.567  0.209
# Residuals       732 11.988 0.01638 



# NPV ---------------------------------------------------------------------

grass.npv <- subset(theil_sen_reg, vegetation_type == 'Grass')[,c('npv_filter_slope_yr', 'vegetation_type')]
t.test(grass.npv$npv_filter_slope_yr)
t.test(grass.npv$npv_filter_slope_yr)$p.value

# One Sample t-test
# 
# data:  grass.npv$npv_filter_slope_yr
# t = -9.7683, df = 330, p-value < 2.2e-16
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   -0.13975067 -0.09289869
# sample estimates:
#   mean of x 
# -0.1163247 

shrub.npv <- subset(theil_sen_reg, vegetation_type == 'Shrub')[,c('npv_filter_slope_yr', 'vegetation_type')]
t.test(shrub.npv$npv_filter_slope_yr)
t.test(shrub.npv$npv_filter_slope_yr)$p.value

# One Sample t-test
# 
# data:  shrub.npv$npv_filter_slope_yr
# t = -6.1092, df = 216, p-value = 4.607e-09
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   -0.09731670 -0.04983992
# sample estimates:
#   mean of x 
# -0.07357831 

tree.npv <- subset(theil_sen_reg, vegetation_type == 'Tree')[,c('npv_filter_slope_yr', 'vegetation_type')]
t.test(tree.npv$npv_filter_slope_yr)
t.test(tree.npv$npv_filter_slope_yr)$p.value


# One Sample t-test
# 
# data:  tree.npv$npv_filter_slope_yr
# t = -4.3501, df = 186, p-value = 2.24e-05
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   -0.07386097 -0.02777074
# sample estimates:
#   mean of x 
# -0.05081585

res.aov_npv <- aov(npv_filter_slope_yr ~ vegetation_type, data = theil_sen_reg)
summary(res.aov_npv)

# Df Sum Sq Mean Sq F value   Pr(>F)    
# vegetation_type   2  0.569 0.28428   7.697 0.000492 ***
#   Residuals       732 27.035 0.03693                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

TukeyHSD(res.aov_npv)

# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = npv_filter_slope_yr ~ vegetation_type, data = theil_sen_reg)
# 
# $vegetation_type
# diff          lwr        upr     p adj
# Shrub-Grass 0.04274637  0.003324007 0.08216873 0.0297879
# Tree-Grass  0.06550882  0.024220591 0.10679706 0.0006127
# Tree-Shrub  0.02276246 -0.022271098 0.06779601 0.4612639


# Bare Soil ---------------------------------------------------------------

grass.bs <- subset(theil_sen_reg, vegetation_type == 'Grass')[,c('bs_filter_slope_yr', 'vegetation_type')]
t.test(grass.bs$bs_filter_slope_yr)
t.test(grass.bs$bs_filter_slope_yr)$p.value

#One Sample t-test
# data:  grass.bs$bs_filter_slope_yr
# t = 3.1829, df = 330, p-value = 0.001597
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   0.01527429 0.06470378
# sample estimates:
#   mean of x 
# 0.03998903

shrub.bs <- subset(theil_sen_reg, vegetation_type == 'Shrub')[,c('bs_filter_slope_yr', 'vegetation_type')]
t.test(shrub.bs$bs_filter_slope_yr)
t.test(shrub.bs$bs_filter_slope_yr)$p.value

# One Sample t-test
# 
# data:  shrub.bs$bs_filter_slope_yr
# t = 0.58675, df = 216, p-value = 0.558
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   -0.01919916  0.03547510
# sample estimates:
#   mean of x 
# 0.008137972 


tree.bs <- subset(theil_sen_reg, vegetation_type == 'Tree')[,c('bs_filter_slope_yr', 'vegetation_type')]
t.test(tree.bs$bs_filter_slope_yr)
t.test(tree.bs$bs_filter_slope_yr)$p.value

#One Sample t-test
#
# data:  tree.bs$bs_filter_slope_yr
# t = -1.3211, df = 186, p-value = 0.1881
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   -0.042157460  0.008340797
# sample estimates:
#   mean of x 
# -0.01690833 


res.aov_bs <- aov(bs_filter_slope_yr ~ vegetation_type, data = theil_sen_reg)
summary(res.aov_bs)

# Df Sum Sq Mean Sq F value Pr(>F)   
# vegetation_type   2   0.41 0.20320   4.655 0.0098 **
#   Residuals       732  31.96 0.04365                  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

TukeyHSD(res.aov_bs)

# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = bs_filter_slope_yr ~ vegetation_type, data = theil_sen_reg)
# 
# $vegetation_type
# diff         lwr         upr     p adj
# Shrub-Grass -0.03185106 -0.07471083  0.01100871 0.1891607
# Tree-Grass  -0.05689737 -0.10178570 -0.01200903 0.0084474
# Tree-Shrub  -0.02504630 -0.07400653  0.02391392 0.4526999


# Create Tables ---------------------------------------------------------------------------------------------------

# Combine all ANOVA results
anova_table <- bind_rows(
  extract_anova(res.aov_pv,  "PV"),
  extract_anova(res.aov_npv, "NPV"),
  extract_anova(res.aov_bs,  "BS")
)

# Combine all Tukey Results
tukey_table <- bind_rows(
  extract_tukey(res.aov_npv, "NPV"),
  extract_tukey(res.aov_bs, "BS")
)

# Combine all Two-tailed One Sample T-test 
ttest_table <- bind_rows(
  
  extract_ttest(grass.pv$pv_filter_slope_yr, "PV", "Grass"),
  extract_ttest(shrub.pv$pv_filter_slope_yr, "PV", "Shrub"),
  extract_ttest(tree.pv$pv_filter_slope_yr,  "PV", "Tree"),
  
  extract_ttest(grass.npv$npv_filter_slope_yr, "NPV", "Grass"),
  extract_ttest(shrub.npv$npv_filter_slope_yr, "NPV", "Shrub"),
  extract_ttest(tree.npv$npv_filter_slope_yr,  "NPV", "Tree"),
  
  extract_ttest(grass.bs$bs_filter_slope_yr, "BS", "Grass"),
  extract_ttest(shrub.bs$bs_filter_slope_yr, "BS", "Shrub"),
  extract_ttest(tree.bs$bs_filter_slope_yr,  "BS", "Tree")
)


# Create Tables 

ft_anova <- flextable(anova_table) %>% 
  merge_v(j = "Variable") %>% 
  autofit() %>% 
  theme_booktabs()

ft_tukey <- flextable(tukey_table) %>% 
  merge_v(j = "Variable") %>% 
  autofit() %>% 
  theme_booktabs()

ft_ttest <- flextable(ttest_table) %>% 
  merge_v(j = "Variable") %>% 
  autofit() %>% 
  theme_booktabs()


# Export to Word Doc --------------------------------------------------------------------------------------------


doc <- read_docx() %>% 
  body_add_par("One-way ANOVA", style = "heading 2") %>% 
  body_add_flextable(ft_anova) %>% 
  body_add_par("") %>% 
  body_add_par("Tukey HSD Post Hoc Test", style = "heading 2") %>% 
  body_add_flextable(ft_tukey) %>% 
  body_add_par("") %>% 
  body_add_par("One-sample t-tests", style = "heading 2") %>% 
  body_add_flextable(ft_ttest)

print(doc, target = "ANOVA_TABLES/ANOVA_Table.docx")

