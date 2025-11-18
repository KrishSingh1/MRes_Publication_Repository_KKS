#### Chapter 2 Group 2 Figures ####
## Krish Singh
# Date: 27-09-2024


# Libraries ---------------------------------------------------------------
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

# Functions ---------------------------------------------------------------

# Main --------------------------------------------------------------------

one_point <- read.csv('../DATASETS/AusPlots_Extracted_Data/Final/DEA_FC_Ground_Truth_Evaluation_complete_Height_Rule_Agg.csv') %>%
  mutate(fraction_type = case_match(
    fraction_type,
    'green' ~ 'PV',
    'brown' ~ 'NPV',
    'bare'  ~ 'BS'
     )) %>%
  mutate(fraction_type = factor(fraction_type,
                                levels = c('PV', 'NPV', 'BS'),
                                ordered = TRUE))

# Evaluation Plot of RS versus INSITU -------------------------------------


# Calc the R2 of the linear model
# Calc the RMSE of the two data points (not of the linear model)
stats_fc <- one_point %>%
  group_by(fraction_type) %>%
  do({
    model <- lm(remote_value ~ on_ground_value, data = .)
    frac_values <- na.omit(.[c('remote_value', 'on_ground_value')])
    data.frame(
      intercept = coef(model)[1],
      slope = coef(model)[2],
      r2 = summary(model)$r.squared,
      rmse = Metrics::rmse(frac_values[['remote_value']], frac_values[['on_ground_value']]),
      bias = mean(frac_values[['remote_value']] - frac_values[['on_ground_value']]),
      r = cor(frac_values[['remote_value']], frac_values[['on_ground_value']]),
      p_value_intercept = summary(model)$coefficients[, "Pr(>|t|)"][1],
      p_value_slope = summary(model)$coefficients[, "Pr(>|t|)"][2]
    )
  })

#

combined_change <- ggplot(data = one_point,
                          mapping = aes(x = on_ground_value, y = remote_value)) + 
  labs(x = "% fractional cover (in-situ)",
       y = "% fractional cover (RS)") +
  geom_point(color = 'black',
             fill="#FDD262",
             shape=21,
             alpha=0.9,
             #stroke = 1
  ) +
  geom_abline(slope = 1, 
              intercept = 0,
              lty = 2,
              color =  '#FF0000',
              size = 1) +
  coord_obs_pred() +
  xlim(c(0,110)) +
  facet_grid(~fraction_type) +
  geom_text(data = stats_fc,
            hjust = 0, 
            size = 3.5,
            mapping = aes(x = 0, y = 100,
                          label =
                            paste0("y = ", round(intercept, 2), 
                                   " + ", round(slope, 2), "x",
                                     ", bias = ", round(bias, 2),
                                   "\nR² = ", round(r2, 2),
                                   "\nRMSE = ", round(rmse, 2)))) +
  theme_bw() +
  geom_smooth(method = 'lm', color = '#295FFF')

ggsave(plot = combined_change,
       height = 6.74,
       width = 16.99,
       scale = 1.2,
       filename = 'C:/Users/krish/Desktop/University/PAPER_1_STUFF/PAPER_1_REPOSITORY/FIGURES/Figure_5_DEA_FC_VS_AusPlots_FC.png',
       dpi = 600,
       units = 'cm')

# By Vegetation Type ------------------------------------------------------

# Calc the R2 of the linear model
# Calc the RMSE of the two data points (not of the linear model)
stats_fc <- one_point %>%
  group_by(vegetation_type, fraction_type) %>%
  do({
    model <- lm(remote_value ~ on_ground_value, data = .)
    frac_values <- na.omit(.[c('remote_value', 'on_ground_value')])
    data.frame(
      intercept = coef(model)[1],
      slope = coef(model)[2],
      r2 = summary(model)$r.squared,
      rmse = Metrics::rmse(frac_values[['remote_value']], frac_values[['on_ground_value']]),
      bias = mean(frac_values[['remote_value']] - frac_values[['on_ground_value']]),
      r = cor(frac_values[['remote_value']], frac_values[['on_ground_value']]),
      p_value_intercept = summary(model)$coefficients[, "Pr(>|t|)"][1],
      p_value_slope = summary(model)$coefficients[, "Pr(>|t|)"][2]
    )
  })


combined_change_veg <- ggplot(
  data = one_point, 
  mapping = aes(x = on_ground_value,
                y = remote_value)) + 
  labs(x = "% fractional cover (in-situ)",
       y = "% fractional cover (RS)") +
  geom_point(color = 'black',
             fill="#FDD262",
             shape=21,
             alpha=0.9,
             #stroke = 1
  ) +
  geom_abline(slope = 1, 
              intercept = 0,
              lty = 2,
              color =  '#FF0000',
              size = 1) +
  coord_obs_pred() +
  xlim(c(0,110)) +
  facet_grid(vegetation_type ~ fraction_type) +
  geom_text(data = stats_fc,
            hjust = 0, 
            size = 3.5,
            mapping = aes(x = 0, y = 100,
                          label =
                            paste0("y = ", round(intercept, 2), 
                                   " + ", round(slope, 2), "x",
                                   ", bias = ", round(bias, 2),
                                   "\nR² = ", round(r2, 2),
                                   "\nRMSE = ", round(rmse, 2)))) +
  theme_bw() +
  geom_smooth(method = 'lm',
              color = '#295FFF')
combined_change_veg

ggsave(plot = combined_change_veg,
       height = 14.50,
       width = 14.50,
       scale = 1.4,
       filename = 'C:/Users/krish/Desktop/University/PAPER_1_STUFF/PAPER_1_REPOSITORY/FIGURES/Figure_6_Veg_DEA_FC_VS_AusPlots_FC.png',
       dpi = 600,
       units = 'cm')

# Evaluation Plot of the change of RS versus INSITU -----------------------

change <- read.csv('../DATASETS/AusPlots_Extracted_Data/Final/Fractional_Cover_Change_Evaluation.csv') %>%
  mutate(fraction_type = case_match(
    fraction_type,
    'green' ~ 'PV',
    'brown' ~ 'NPV',
    'bare'  ~ 'BS'
  ))  %>%
  mutate(fraction_type = factor(fraction_type,
                                     levels = c('PV', 'NPV', 'BS'),
                                     ordered = TRUE))

stats_fc <- change %>%
  group_by(fraction_type) %>%
  do({
    model <- lm(remote_value ~ on_ground_value, data = .)
    frac_values <- na.omit(.[c('remote_value', 'on_ground_value')])
    data.frame(
      intercept = coef(model)[1],
      slope = coef(model)[2],
      r2 = summary(model)$r.squared,
      bias = mean(frac_values[['remote_value']] - frac_values[['on_ground_value']]),
      rmse = Metrics::rmse(frac_values[['remote_value']], frac_values[['on_ground_value']]),
      p_value_intercept = summary(model)$coefficients[, "Pr(>|t|)"][1],
      p_value_slope = summary(model)$coefficients[, "Pr(>|t|)"][2]
    )
  })


combined_change <- ggplot(data = change, mapping = aes(x = on_ground_value, y = remote_value, group = fraction_type)) + 
  labs(x = "\u0394% fractional cover (in-situ)",
       y = "\u0394% fractional cover (RS)") +
  geom_point(color = 'black',
             fill="#FDD262",
             shape=21,
             alpha=0.9,
             #stroke = 1
  ) +
  geom_abline(slope = 1, 
              intercept = 0,
              lty = 2,
              color =  '#FF0000',
              size = 1) +
  coord_obs_pred() +
  xlim(c(-100, 100)) +
  facet_grid(~fraction_type) +
  geom_text(data = stats_fc,
            hjust = 0, 
            size = 3.5,
            mapping = aes(x =-100, y = 80,
                          label =
                            paste0("y = ", round(intercept, 2), 
                                   " + ", round(slope, 2), "x",
                                   ", bias = ", round(bias, 2),
                                   "\nR² = ", round(r2, 2),
                                   "\nRMSE = ", round(rmse, 2)))) +
  theme_bw() +
  geom_smooth(method = 'lm',
              color = '#295FFF',
  )

combined_change


ggsave(plot = combined_change,
       height = 6.74,
       width = 16.99,
       scale = 1.2,
       filename = 'C:/Users/krish/Desktop/University/PAPER_1_STUFF/PAPER_1_REPOSITORY/FIGURES/Figure_7_Change_DEA_FC_VS_AusPlots_FC.png',
       dpi = 600,
       units = 'cm')


# Now by Vegetation type

stats_fc <- change %>%
  group_by(vegetation_type, fraction_type) %>%
  do({
    model <- lm(remote_value ~ on_ground_value, data = .)
    frac_values <- na.omit(.[c('remote_value', 'on_ground_value')])
    data.frame(
      intercept = coef(model)[1],
      slope = coef(model)[2],
      r2 = summary(model)$r.squared,
      bias = mean(frac_values[['remote_value']] - frac_values[['on_ground_value']]),
      rmse = Metrics::rmse(frac_values[['remote_value']], frac_values[['on_ground_value']]),
      p_value_intercept = summary(model)$coefficients[, "Pr(>|t|)"][1],
      p_value_slope = summary(model)$coefficients[, "Pr(>|t|)"][2]
    )
  })

combined_change_veg <- ggplot(data = change, mapping = aes(x = on_ground_value, y = remote_value)) + 
  labs(x = "\u0394% fractional cover (in-situ)",
       y = "\u0394% fractional cover (RS)") +
  geom_point(color = 'black',
             fill="#FDD262",
             shape=21,
             alpha=0.9,
             #stroke = 1
  ) +
  geom_abline(slope = 1, 
              intercept = 0,
              lty = 2,
              color =  '#FF0000',
              size = 1) +
  coord_obs_pred() +
  xlim(c(-100,100)) +
  facet_grid(vegetation_type ~ fraction_type) +
  geom_text(data = stats_fc,
            hjust = 0, 
            size = 3.5,
            mapping = aes(x = -100, y = 80,
                          label =
                            paste0("y = ", round(intercept, 2), 
                                   " + ", round(slope, 2), "x",
                                   ", bias = ", round(bias, 2),
                                   "\nR² = ", round(r2, 2),
                                   "\nRMSE = ", round(rmse, 2)))) +
  theme_bw() +
  geom_smooth(method = 'lm',
              color = '#295FFF',
  )

combined_change_veg
ggsave(plot = combined_change_veg,
       height = 14.50,
       width = 14.50,
       scale = 1.4,
       filename = 'C:/Users/krish/Desktop/University/PAPER_1_STUFF/PAPER_1_REPOSITORY/FIGURES/Figure_8_Change_Veg_DEA_FC_VS_AusPlots_FC.png',
       dpi = 600,
       units = 'cm')

# For SI relating to Group 2 ----------------------------------------------
one_point <- read.csv('../DATASETS/AusPlots_Extracted_Data/Final/DEA_FC_Ground_Truth_Evaluation_complete_Height_Rule_Agg.csv')
one_point <- one_point %>%
  mutate(visit_start_date = as.Date(visit_start_date),
         time = as.Date(time)) %>% mutate(fraction_type = case_match(
  fraction_type,
  'green' ~ 'PV',
  'brown' ~ 'NPV',
  'bare'  ~ 'BS'
))



one_point$time_diff <- abs(as.numeric(one_point$visit_start_date - one_point$time))
summary(one_point$time_diff)
mean(one_point$time_diff)
sd(one_point$time_diff)
summary(one_point$fractional_error)

one_point$fractional_error <- abs((one_point$remote_value - one_point$on_ground_value))
model <- lm(data = one_point[which(one_point$fraction_type == 'green'),], fractional_error ~ time_diff)
summary(model)
plot(model)

ggplot(data = one_point, mapping = aes(x = time_diff, y = fractional_error)) + geom_point() +
  labs(x = 'Absolute Time Difference (days)', y = 'Absolute Error') +
  theme_bw() + facet_wrap(~fraction_type) + geom_smooth(method = 'lm') + facet_grid(~vegetation_type ~fraction_type)

stats_fc <- one_point %>%
  group_by(vegetation_type, fraction_type) %>%
  do({
    model <- lm(fractional_error ~ time_diff, data = .)
    frac_values <- na.omit(.[c('fractional_error')])
    data.frame(
      intercept = coef(model)[1],
      slope = coef(model)[2],
      r2 = summary(model)$r.squared,
      p_value_intercept = summary(model)$coefficients[, "Pr(>|t|)"][1],
      p_value_slope = summary(model)$coefficients[, "Pr(>|t|)"][2]
    )
  })


combined_change_veg <- ggplot(
  data = one_point, 
  mapping = aes(x = time_diff,
                y = fractional_error)) + 
  labs(x = "Absolute Time difference (days)",
       y = "Absolute Error") +
  geom_point(color = 'black',
             fill="#FDD262",
             shape=21,
             alpha=0.9,
             #stroke = 1
  ) +
  xlim(c(0,45)) +
  facet_grid(vegetation_type ~ fraction_type) +
  geom_text(data = stats_fc,
            hjust = 0, 
            size = 3.5,
            mapping = aes(x = 7.5, y = 70,
                          label =
                            paste0("y = ", round(intercept, 2), 
                                   " + (", round(slope, 2), ")x",
                                   "\np = ", round(p_value_slope, 2),
                                   ", R² = ", round(r2, 2)))) +
  theme_bw() +
  geom_smooth(method = 'lm',
              color = '#295FFF')



combined_change_veg

ggsave(plot = combined_change_veg,
       height = 12,
       width = 16,
       scale = 1.4,
       filename = 'C:/Users/krish/Desktop/University/PAPER_1_STUFF/PAPER_1_REPOSITORY/FIGURES/Figure_SI_Absolute_Error_Vs_Time_Difference.png',
       dpi = 600,
       units = 'cm')


# TEST --------------------------------------------------------------------


# TEST

low_bs_selector <- one_point$site_unique[which(one_point$remote_value < 20 & 
                                                 one_point$on_ground_value < 20&
                                                 one_point$fraction_type == 'BS')]
low_bs <- subset(one_point, site_unique %in% low_bs_selector)

stats_fc_low_bs <- low_bs %>%
  group_by(fraction_type) %>%
  do({
    model <- lm(remote_value ~ on_ground_value, data = .)
    frac_values <- na.omit(.[c('remote_value', 'on_ground_value')])
    data.frame(
      intercept = coef(model)[1],
      slope = coef(model)[2],
      r2 = summary(model)$r.squared,
      rmse = Metrics::rmse(frac_values[['remote_value']], frac_values[['on_ground_value']]),
      bias = mean(frac_values[['remote_value']] - frac_values[['on_ground_value']]),
      r = cor(frac_values[['remote_value']], frac_values[['on_ground_value']]),
      p_value_intercept = summary(model)$coefficients[, "Pr(>|t|)"][1],
      p_value_slope = summary(model)$coefficients[, "Pr(>|t|)"][2]
    )
  })

combined_change_low_bs <- ggplot(data = low_bs,
                                 mapping = aes(x = on_ground_value, y = remote_value)) + 
  labs(x = "fractional cover (in-situ)",
       y = "fractional cover (remote)") +
  geom_point(color = 'black',
             fill="#FDD262",
             shape=21,
             alpha=0.9,
             #stroke = 1
  ) +
  geom_abline(slope = 1, 
              intercept = 0,
              lty = 2,
              color =  '#FF0000',
              size = 1) +
  coord_obs_pred() +
  xlim(c(0,110)) +
  facet_grid(~fraction_type) +
  geom_text(data = stats_fc_low_bs,
            hjust = 0, 
            size = 3.5,
            mapping = aes(x = 0, y = 100,
                          label =
                            paste0("y = ", round(intercept, 2), 
                                   " + ", round(slope, 2), "x",
                                   ", bias = ", round(bias, 2),
                                   "\nR² = ", round(r2, 2),
                                   "\nRMSE = ", round(rmse, 2)))) +
  theme_bw() +
  geom_smooth(method = 'lm', color = '#295FFF')

combined_change_low_bs

