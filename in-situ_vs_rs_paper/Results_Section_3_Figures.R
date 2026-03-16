########## Chapter 2 Group 3 Figures ############
#### Krish K Singh
#### Date: 17-10-24 (Last Updated: 29.07.25)

# Library -----------------------------------------------------------------

library(plotly)
library(ggplot2)
library(dplyr)
library(caret)
library(cowplot)
library(data.table)
library(tune)
library(ggpubr)
library(ggpmisc)
library(Matrix)
library(Metrics)
library(grid)
library(tidyr)
library(lubridate)

# Functions ---------------------------------------------------------------

# Main --------------------------------------------------------------------

# Set up ------------------------------------------------------------------
# Set up the thiel-sen_regs 
theil_sen_reg <- read.csv('DATASETS/AusPlots_Theil_Sen_Regression_Stats_Signf.csv')
theil_sen_reg_copy <- theil_sen_reg

# Load AusPlots data 
evaluation_fc <- read.csv('DATASETS/DEA_FC_Ground_Truth_Evaluation.csv') %>%
  mutate(time = as.Date(time)) %>%
  arrange(time) 

# Looking at the varying number of NAs between in-situ for green, brown, bs
# Its best to calculate the change differently
evaluation_list <- list(green_fc = na.omit(evaluation_fc[,c('site_unique', 'site_location_name', 'green', 'pv_filter', 'time')]),
                        brown_fc = na.omit(evaluation_fc[,c('site_unique', 'site_location_name', 'brown', 'npv_filter', 'time')]),
                        bs_fc = na.omit(evaluation_fc[,c('site_unique', 'site_location_name', 'bare', 'bs_filter', 'time')]))


# Select only Sites with > 1 visits PER FC component
# NOTE: Even if a site has > 1 visits, any na in PV, NPV, BS will exclude the site from the dataset
#       But this proccess is independent of the FC component, i.e. NA in PV will not affect the selection process
#       of the NPV. 
evaluation_list <- lapply(evaluation_list, FUN = function(x) {
  counts.df <- as.data.frame(table(x$site_location_name)) %>%
    subset(Freq >= 2) 
  x <- subset(x, site_location_name %in% unique(counts.df$Var1)) # now subset the dataframe by sites with more than 1 visit
  return(x)
})

# Perform Linear Modelling ------------------------------------------------
# Note: Linear model is Fc component ~ time (in days), so
#       the slope will be Fc/day. 
#       365 * FC/day ~= FC/yr
# PV ----------------------------------------------------------------------
stats_fc_pv <- evaluation_list[['green_fc']] %>%
  group_by(site_location_name) %>%
  arrange('time') %>%
  do({
    model <- lm(pv_filter ~ time, data = .) # rs
    model2 <- lm(green ~ time, data = .) # og
    data.frame(
      intercept_rs_pv = coef(model)[1],
      slope_rs_pv = coef(model)[2],
      slope_rs_pv_yr = coef(model)[2]*365,
      intercept_insitu_pv = coef(model2)[1],
      slope_insitu_pv = coef(model2)[2],
      slope_insitu_pv_yr = coef(model2)[2]*365,
      n_samples = nrow(model$model)
    )
  })

# NPV ---------------------------------------------------------------------
stats_fc_npv <- evaluation_list[['brown_fc']] %>%
  group_by(site_location_name) %>%
  arrange('time') %>%
  do({
    model <- lm(npv_filter ~ time, data = .) # rs 
    model2 <- lm(brown ~ time, data = .) # og
    data.frame(
      intercept_rs_npv = coef(model)[1],
      slope_rs_npv = coef(model)[2],
      slope_rs_npv_yr = coef(model)[2]*365,
      intercept_insitu_npv = coef(model2)[1],
      slope_insitu_npv = coef(model2)[2],
      slope_insitu_npv_yr = coef(model2)[2]*365,
      n_samples = nrow(model$model)
    )
  })

# Bare Soil ---------------------------------------------------------------
stats_fc_bs <- evaluation_list[['bs_fc']] %>%
  group_by(site_location_name) %>%
  arrange('time') %>%
  do({
    model <- lm(bs_filter ~ time, data = .) # rs
    model2 <- lm(bare ~ time, data = .) # og
    data.frame(
      intercept_rs_bs = coef(model)[1],
      slope_rs_bs = coef(model)[2],
      slope_rs_bs_yr = coef(model)[2]*365,
      intercept_insitu_bs = coef(model2)[1],
      slope_insitu_bs = coef(model2)[2],
      slope_insitu_bs_yr = coef(model2)[2]*365,
      n_samples = nrow(model$model)
    )
  })

# Combine Theil-Sen Slopes with Short-term Trend --------------------------
test <- stats_fc_pv %>%
  left_join(stats_fc_npv, by = c('site_location_name', 'n_samples') ) %>%
  left_join(stats_fc_bs, by = c('site_location_name', 'n_samples')) %>%
  left_join(theil_sen_reg_copy, by = c('site_location_name'))
  
# Graphic -----------------------------------------------------------------
lims <- c(-25, 25)

# PV ----------------------------------------------------------------------
pv_insitu <- ggplot(data = test, mapping = aes(y = pv_filter_slope_yr, x = slope_insitu_pv_yr)) +
  geom_point() + geom_smooth(method =  'lm', size = 0.5) +
  theme_bw() +
  ggpmisc::stat_poly_eq(mapping = use_label(c("R2")), method = 'lm') +
  geom_abline(color = 'red', lty = 2, size = 1) +
  ylab('Long Term Trend (%PV/yr)') +
  xlab('Short Term Trend (in-situ %PV/yr)') +
  coord_fixed(ratio= 1, xlim = lims, ylim = lims)

pv_dea <- ggplot(data = test, mapping = aes(y = pv_filter_slope_yr, x = slope_rs_pv_yr)) +
  geom_point() + geom_smooth(method =  'lm', size = 0.5) +
  theme_bw() +
  ggpmisc::stat_poly_eq(mapping = use_label(c("R2")), method = 'lm') +
  geom_abline(color = 'red', lty = 2, size = 1) +
  ylab('Long Term Trend (%PV/yr)') +
  xlab('Short Term Trend (DEA %PV/yr)') +
  coord_fixed(ratio= 1, xlim = lims, ylim = lims)

# NPV ---------------------------------------------------------------------
npv_insitu <- ggplot(data = test, mapping = aes(y = npv_filter_slope_yr, x = slope_insitu_npv_yr)) +
  geom_point() + geom_smooth(method =  'lm', size = 0.5) + 
  theme_bw() +
  ggpmisc::stat_poly_eq(mapping = use_label(c("R2")), method = 'lm') +
  geom_abline(color = 'red', lty = 2, size = 1) +
  ylab('Long Term Trend (%NPV/yr)') +
  xlab('Short Term Trend (in-situ %NPV/yr)') + 
  coord_fixed(ratio= 1, xlim = lims, ylim = lims)

npv_dea <- ggplot(data = test, mapping = aes(y = npv_filter_slope_yr, x = slope_rs_npv_yr)) +
  geom_point() + geom_smooth(method =  'lm', size = 0.5) + 
  theme_bw() +
  ggpmisc::stat_poly_eq(mapping = use_label(c("R2")), method = 'lm') +
  geom_abline(color = 'red', lty = 2, size = 1) +
  ylab('Long Term Trend (%NPV/yr)') +
  xlab('Short Term Trend (DEA %NPV/yr)') + 
  coord_fixed(ratio= 1, xlim = lims, ylim = lims)

# Bare Soil ---------------------------------------------------------------
bs_insitu <- ggplot(data = test, mapping = aes(y = bs_filter_slope_yr, x = slope_insitu_bs_yr)) +
  geom_point() + geom_smooth(method =  'lm', size = 0.5) + 
  theme_bw() + ggpmisc::stat_poly_eq(mapping = use_label(c("R2")), method = 'lm') +
  geom_abline(color = 'red', lty = 2, size = 1) +
  ylab('Long Term Trend (%BS/yr)') +
  xlab('Short Term Trend (in-situ %BS/yr)') + 
  coord_fixed(ratio= 1, xlim = lims, ylim = lims)

bs_dea <- ggplot(data = test, mapping = aes(y = bs_filter_slope_yr, x = slope_rs_bs_yr)) +
  geom_point() + geom_smooth(method =  'lm', size = 0.5) + 
  theme_bw() + ggpmisc::stat_poly_eq(mapping = use_label(c("R2")), method = 'lm') +
  geom_abline(color = 'red', lty = 2, size = 1) +
  ylab('Long Term Trend (%BS/yr)') +
  xlab('Short Term Trend (DEA %BS/yr)') + 
  coord_fixed(ratio= 1, xlim = lims, ylim = lims)

# Combine Plot ------------------------------------------------------------
combined_plot_insitu <- plot_grid(pv_insitu, npv_insitu, 
                           bs_insitu, ncol = 3,
                           labels = c("a)", "b)", "c)"))
combined_plot_insitu

combined_plot_DEA <- plot_grid(pv_dea, npv_dea, 
                                  bs_dea, ncol = 3,
                                  labels = c("d)", "e)", "f)"))
combined_plot_DEA

# Add PV timeseries of some example sites ---------------------------------
stroke <- 0.7
shape <- 4
alpha <- 0.7

fraction <- 'pv'
if (fraction == 'pv'){
  colour <- 'darkgreen'
  og_name <- 'green'
} else if (fraction == 'npv') {
  colour <- 'steelblue'
  og_name <- 'brown'
} else if (fraction == 'bs') {
  colour <- 'darkred'
  og_name <- 'bare'
}

# SAABHC0004 --------------------------------------------------------------
example_site <- 'SAABHC0004'
dea_fc_path <- 'DATASETS/DEA_FC_PROCESSED/MODELLED_PREPROCESSED/'
dea_fc_path <- paste0(dea_fc_path, 'Input_DataSet_', example_site, '.csv')
dea_fc <- read.csv(dea_fc_path)

evaluation.data.2 <- evaluation_fc %>%
  subset(site_location_name == example_site)

theil_sen_reg_subset <- subset(test, site_location_name == example_site)
pv_max <- max(max(dea_fc$pv), max(evaluation.data.2$green), max(evaluation.data.2$pv_filter))
celing <- pv_max + 30

t <- ggplot(data = dea_fc, aes(x = as.Date(time)))  +
  # the main time series 
  geom_line(aes(y = .data[[paste0('pv', '_filter')]], ), size = 0.2, color = colour) + 
  scale_y_continuous(limits = c(0, celing), breaks = scales::pretty_breaks(n = 5)) +
  scale_x_date(date_labels = "%Y", minor_breaks = ('1 year'),
               date_breaks = ('5 years'),
               limits = c(as.Date('1987-01-01'), 
                          as.Date('2022-12-31'))) +
  xlab('Time') +
  ylab('PV (%)') +
  # The on ground fractional cover measurements 
  geom_point(data = evaluation.data.2,
             aes(x =  as.Date(visit_start_date), 
                 y = .data[[paste0(og_name)]]), 
             shape = shape, 
             color = 'black', 
             size = 3, 
             stroke = stroke,
             alpha = alpha) +
  geom_smooth(data = evaluation.data.2,
              aes(x =  as.Date(visit_start_date), 
                  y = .data[[paste0(og_name)]]),
              method = 'lm',se =F,
              color = 'black', size = 0.5) +
  # The associated timestamp of the Landsat imagery: 
  geom_point(data = evaluation.data.2,
             aes(x =  as.Date(time), 
                 y = .data[[paste0(fraction, '_filter')]]), 
             shape = shape,
             color = 'blue',
             size = 3, 
             stroke = stroke,
             alpha = alpha) +
  geom_smooth(data = evaluation.data.2,
              aes(x =  as.Date(time), 
                  y = .data[[paste0(fraction, '_filter')]]),
              method = 'lm',se =F, size = 0.5) +
  # Thiel-sen Regression
  geom_abline(intercept = theil_sen_reg_subset[[paste0(fraction, '_filter','_intercept')]], 
              slope = theil_sen_reg_subset[[paste0(fraction, '_filter', '_slope')]],
              color = 'red', size = 0.5) +
  theme_bw() +
  annotate('text', x = as.Date('1987-01-01'), 
           y = celing - 5, 
           label = paste0("LT (DEA FC)", ' = ',round(theil_sen_reg_subset[[paste0(fraction, '_filter', '_slope_yr')]],2), ' %PV/yr'), 
           color = "red", size = 3.5,
           hjust = 0) +
  annotate('text', x = as.Date('1987-01-01'), 
           y = celing - 5 - 1*(10), 
           label = paste0('ST (AusPlots FC)', ' = ', round(theil_sen_reg_subset[[paste0('slope_insitu_',fraction ,'_yr')]],2), ' %PV/yr'),
           color = "black", size = 3.5,
           hjust = 0) +
  annotate('text', x = as.Date('1987-01-01'),
           y = celing - 5 - 2*(10), 
           label = paste0('ST (DEA FC)',' = ', round(theil_sen_reg_subset[[paste0('slope_rs_', fraction, '_yr')]],2), ' %PV/yr'),
           color = "blue", 
           size = 3.5,
           hjust = 0) 

# QDAEIUQ0006 -------------------------------------------------------------
example_site <- 'QDAEIU0006'
dea_fc_path <- 'DATASETS/DEA_FC_PROCESSED/MODELLED_PREPROCESSED/'
dea_fc_path <- paste0(dea_fc_path, 'Input_DataSet_', example_site, '.csv')
dea_fc <- read.csv(dea_fc_path)

evaluation.data.2 <- evaluation_fc %>%
  subset(site_location_name == example_site) %>%
  na.omit()

theil_sen_reg_subset <- subset(test, site_location_name == example_site)
pv_max <- max(max(dea_fc$pv), max(evaluation.data.2$green), max(evaluation.data.2$pv_filter))
celing <- pv_max + 30

t2 <- ggplot(data = dea_fc, aes(x = as.Date(time)))  +
  # the main time series 
  geom_line(aes(y = .data[[paste0('pv', '_filter')]], ), size = 0.2, color = colour) + 
  scale_y_continuous(limits = c(0, celing), breaks = scales::pretty_breaks(n = 5)) +
  scale_x_date(date_labels = "%Y", minor_breaks = ('1 year'),
               date_breaks = ('5 years'),
               limits = c(as.Date('1987-01-01'), 
                          as.Date('2022-12-31'))) +
  xlab('Time') +
  ylab('PV (%)') +
  # The on ground fractional cover measurements 
  geom_point(data = evaluation.data.2,
             aes(x =  as.Date(visit_start_date), 
                 y = .data[[paste0(og_name)]]), 
             shape = shape, 
             color = 'black', 
             size = 3, 
             stroke = stroke,
             alpha = alpha) +
  geom_smooth(data = evaluation.data.2,
              aes(x =  as.Date(visit_start_date), 
                  y = .data[[paste0(og_name)]]),
              method = 'lm',se =F,
              color = 'black', size = 0.5) +
  # The associated timestamp of the Landsat imagery: 
  geom_point(data = evaluation.data.2,
             aes(x =  as.Date(time), 
                 y = .data[[paste0(fraction, '_filter')]]), 
             shape = shape,
             color = 'blue',
             size = 3, 
             stroke = stroke,
             alpha = alpha) +
  geom_smooth(data = evaluation.data.2,
              aes(x =  as.Date(time), 
                  y = .data[[paste0(fraction, '_filter')]]),
              method = 'lm',se =F, size = 0.5) +
  # Thiel-sen Regression
  geom_abline(intercept = theil_sen_reg_subset[[paste0(fraction, '_filter','_intercept')]], 
              slope = theil_sen_reg_subset[[paste0(fraction, '_filter', '_slope')]],
              color = 'red', size = 0.5) +
  theme_bw() +
  annotate('text', x = as.Date('1987-01-01'), 
           y = celing - 5, 
           label = paste0("LT (DEA FC)", ' = ',round(theil_sen_reg_subset[[paste0(fraction, '_filter', '_slope_yr')]],2), ' %PV/yr'), 
           color = "red", size = 3.5,
           hjust = 0) +
  annotate('text', x = as.Date('1987-01-01'), 
           y = celing - 5 - 1*(10), 
           label = paste0('ST (AusPlots FC)', ' = ', round(theil_sen_reg_subset[[paste0('slope_insitu_',fraction ,'_yr')]],2), ' %PV/yr'),
           color = "black", size = 3.5,
           hjust = 0) +
  annotate('text', x = as.Date('1987-01-01'),
           y = celing - 5 - 2*(10), 
           label = paste0('ST (DEA FC)',' = ', round(theil_sen_reg_subset[[paste0('slope_rs_', fraction, '_yr')]],2), ' %PV/yr'),
           color = "blue", 
           size = 3.5,
           hjust = 0) 

# Combine all Plots -------------------------------------------------------
final_combined_plot <- plot_grid(combined_plot_insitu,
                                 combined_plot_DEA,
                                 t, t2, cols =1,
          labels = c(' ', ' ', 'g)', 'h)'))
plot(final_combined_plot)
ggsave(plot = final_combined_plot,
       height = 18,
       width = 16.5,
       scale = 1.4,
       filename = 'FIGURES/Figure_8_Short_vs_Long_Term.png',
       dpi = 600,
       units = 'cm',
       bg = "white")

# END ---------------------------------------------------------------------