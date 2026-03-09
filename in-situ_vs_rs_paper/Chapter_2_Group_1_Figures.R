#### Chapter 2 Group 1 Figures ####
## KRISH sINGH
## 26-04-2024


# Libraries ----------------------------------------------------------------

library(ggplot2)
library(patchwork)
library(cowplot)
library(dplyr)
library(ozmaps)
library(RColorBrewer)
library(ggsignif)
library(grDevices)
library(rlang)
library(magick)



# Functions ----------------------------------------------------------------

# Main --------------------------------------------------------------------

theil_sen_reg <- read.csv('DATASETS/AusPlots_Theil_Sen_Regression_Stats_Signf.csv')
veg_type <- read.csv('../DATASETS/AusPlots_Agg_VegType_PC_Height_Rule.csv')

# Combine the datasets
theil_sen_reg_copy <- theil_sen_reg %>%
  left_join(veg_type, by = 'site_location_name')


# Perform classifications for each fraction based on their significance and slope
# Green if signficant AND slope is positive
# Yellow if NOT Significant
# Red is signficiant AND slope is negative 

# If sigificant 
#   if slope > 0
#        color = green, code 3
#   else
#         color = red, code 1
# else
#    color = yellow, code 2 

# For each fraction
for (i in c('pv_filter', 'npv_filter', 'bs_filter')) {
  signf_name <- paste0(i,'_signf')
  slope_name <- paste0(i, '_slope')
  slope_coding <- paste0(i, '_code')
  
  coding <- apply(theil_sen_reg_copy, MARGIN = 1, FUN = function(row){
    
    slope_sign <- sign(as.numeric(row[[slope_name]]))
    
    if (row[[signf_name]] == T){
      
      if(slope_sign == 1) {
        code <- 3
      } else if (slope_sign == -1){
        code <- 1
      }
    } else {
      code <- 2
    }
    return(code)
  })
  theil_sen_reg_copy[slope_coding] <- as.factor(coding)
}


# FIGURE 2 ----------------------------------------------------------------


sf_oz <- ozmap("states")
annotate_properties <- list(x = 115,
                         y = -10,
                         size = 4)

pl_pv <- ggplot(data = sf_oz) + geom_sf() +
  geom_point(data = theil_sen_reg_copy, 
             mapping = aes(x = longitude, 
                           y = latitude, 
                           fill = pv_filter_code),
             size= 2,
             alpha = 0.5,
             shape = 21,
             color = 'black', 
             stroke = 0.3,
             height = 0.3,
             width = 0.3) + 
  scale_fill_manual("Slope", values = c('1' = 'red', '2' = 'blue','3' = 'green'),
                      labels = c("Negative", "Non Significant", "Positive")) +
  theme_bw() +
  theme(
    legend.key = element_rect(fill = "white", color = "black"),  # Box around legend items
    legend.background = element_rect(fill = "white", color = "black")  # Box around the legend
  ) +
  coord_sf(xlim = c(114, 152), ylim = c(-10, -43) ) +
  do.call(
    annotate,
    c(list("text", label = "a) PV", color = "black"),
      annotate_properties)
    )
  
pl_pv

## For NPV
pl_npv <- ggplot(data = sf_oz) + geom_sf() +
  geom_point(data = theil_sen_reg_copy, 
             mapping = aes(x = longitude, y = latitude, fill = npv_filter_code),
             size= 2.5, alpha = 0.5, shape = 21, color = 'black', stroke = 0.1) + 
  scale_fill_manual("Slope", values = c('1' = 'red', '2' = 'blue','3' = 'green'),
                      labels = c("Negative", "Non Significant", "Positive")) +
  theme_bw() +
  theme(
    legend.key = element_rect(fill = "white", color = "black"),  # Box around legend items
    legend.background = element_rect(fill = "white", color = "black")  # Box around the legend
  ) +
  coord_sf(xlim = c(114, 152), ylim = c(-10, -43) ) +
  do.call(
    annotate,
    c(list("text", label = "b) NPV", color = "black"),
      annotate_properties)
  )
pl_npv

## For BS
pl_bs <- ggplot(data = sf_oz) + geom_sf() +
  geom_point(data = theil_sen_reg_copy, 
             mapping = aes(x = longitude, y = latitude, fill = bs_filter_code),
             size= 2.5, alpha = 0.5, shape = 21, color = 'black', stroke = 0.1) +  
  theme_bw() +
  theme(
    legend.key = element_rect(fill = "white", color = "black"),  # Box around legend items
    legend.background = element_rect(fill = "white", color = "black")  # Box around the legend
  ) +
  scale_fill_manual("Trend", values = c('1' = 'red', '2' = 'blue','3' = 'green'),
                      labels = c("Negative", "Non Significant", "Positive")) + 
  coord_sf(xlim = c(114, 152), ylim = c(-10, -43) ) +
  do.call(
    annotate,
    c(list("text", label = "c) BS", color = "black"),
      annotate_properties)
  ) +
  theme(
    legend.key = element_rect(fill = "white", color = "black"),  # Box around legend items
    legend.background = element_rect(fill = "white", color = "black"),  # Box around the legend
  )
pl_bs

legend <- cowplot::get_legend(pl_bs)

# Set up the plots for a gridded plot 
p1 <- pl_pv + theme(legend.position = "none",
                    axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab('')
p2 <- pl_npv + theme(legend.position = "none",
                     axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab('') 
p3 <- pl_bs + theme(axis.text.x = element_text(angle = 45, hjust = 1),
                    legend.position = "none")


combined_plot <- plot_grid(p1, p2, p3, ncol = 2)
combined_plot


ggsave("FIGURES/Figure_2_Thiel_Sen_Regression_Maps.png",
       plot = combined_plot,
       width = 30,
       height = 30,
       units = "cm",
       bg = 'white',
       dpi = 600)

ggsave("legend.png",
       plot_grid(legend), 
       width = 2,
       height = 2, 
       dpi = 600)


# Composite the Legend image with the plot image
plot_img <- image_read("FIGURES/Figure_2_Thiel_Sen_Regression_Maps.png")
legend_img <- image_read("legend.png")
legend_img <- image_scale(legend_img, "x1200") 

final_img <- image_composite(
  plot_img,
  legend_img,
  offset = "+3900+3400",  
  operator = "atop"    
)

# Ready to write 
image_write(final_img,
            "FIGURES/Figure_2_Thiel_Sen_Regression_Maps.png", 
            format = 'png',
            flatten = T
            )

# FOR TERN SYMPOSIUM

combined_plot <- plot_grid(p1 + ylab(''),
                           p2 + xlab(''), 
                           p3 + xlab('') +
                             ylab(''),
                           ncol = 3)
combined_plot



# FIGURE 3 ----------------------------------------------------------------


generate_mean_string <- function(d, group, slope) {
  
  if (slope == 'pv') {
    slope_name = 'pv_filter_slope_yr'
  } else if (slope == 'npv') {
    slope_name = 'npv_filter_slope_yr'
  } else if (slope == 'bs') {
    slope_name = 'bs_filter_slope_yr'
  }
  
  if (group == 'veg') {
    group_name = 'vegetation_type'
  } else if (group == 'bioclimatic') {
    group_name = 'bioclimatic_region'
  }
  
  agg <- aggregate(x = theil_sen_reg_copy[[slope_name]],
                   by = list(theil_sen_reg_copy[[group_name]]),
                   FUN = mean)
  
  
  mean_str <- c()
  if (group_name == 'bioclimatic_region') {
    abrev <- substr(group_name, start = 1, stop = 3) # Abbreviate
    for (i in 1:nrow(agg)) {
      mean_str <- c(mean_str, paste0('x\u0305(', abrev, i, ') = ' ,
                                     round(agg[i, 'x'], 4)))
      
      #print(mean_str)
    }
  } else if (group_name == 'vegetation_type') {
    print('VEG')
    
    p_values <- sapply(
      split(theil_sen_reg_copy,
            theil_sen_reg_copy$vegetation_type),
      function(x) {
        return (t.test(x[slope_name])$p.value)
      }
    ) %>% as.data.frame()
    
    p_values$Group.1 = rownames(p_values)
    
    agg <- agg %>% left_join(p_values)
    print(agg)
    
    # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
    
    for (i in 1:nrow(agg)) {
      
      if (agg[[i, '.']] < 0.001) {
        signf.code <- '***'
      } else if(agg[[i, '.']] < 0.01) {
        signf.code <- '**'
      } else if(agg[[i, '.']] < 0.05) {
        signf.code <- '*'
      } else if(agg[[i, '.']] < 0.1) {
        signf.code <- '.'
      } else {
        signf.code <- ' '
      }
      
      mean_str <- c(
        mean_str, 
        paste0('x\u0305 = ' ,round(agg[i, 'x'], 4),signf.code)
      )
      #print(mean_str)
    }
  }
  
  return(mean_str)
}


pv_mean_str <- generate_mean_string(theil_sen_reg_copy, 'veg', 'pv')

pl_pv <- ggplot(data = theil_sen_reg_copy, 
                mapping = aes(y = pv_filter_slope_yr, x =vegetation_type)) +  
  theme_bw() + 
  geom_jitter(position = position_jitter(seed = 1, width = 0.15, height = 0), 
              alpha = 0.5, 
              size = 1.5,
              fill = 'gold', 
              pch = 21) +
  geom_violin(alpha = 0.3) +
  labs(y = '%PV/yr', x = 'Vegetation Type') +
  geom_boxplot(width = 0.25, alpha = 0) +
  scale_y_continuous(breaks = seq(-0.8, 1.2, 0.2), limits = c(-0.8, 1.2)) +
  theme(legend.position = "none") +
  annotate("text", x = 1, y = 1, label = "a") +
  annotate("text", x = 2, y = 1, label = "a") +
  annotate("text", x = 3, y = 1, label = "a") +
  stat_summary(fun = mean, geom = "point", shape = 21, 
               size = 1, fill = "red") +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "red") 

for (i in 1:length(pv_mean_str)) {
  pl_pv <- pl_pv + annotate("text",
                            x = i + .2,
                            y = 1.1 ,
                            label = pv_mean_str[[i]],
                            hjust = 1,
                            size = 4) 
}

pl_pv

# Now we are annotating points that show a significant figure 

npv_mean_str <- generate_mean_string(theil_sen_reg_copy, 'veg', 'npv')
pl_npv <- ggplot(data = theil_sen_reg_copy, 
                 mapping = aes(y = npv_filter_slope_yr, x =vegetation_type)) +  
  theme_bw() + 
  geom_jitter(position = position_jitter(seed = 1, width = 0.15, height = 0), 
              alpha = 0.5, 
              size = 1.5,
              fill = 'gold', 
              pch = 21) +
  geom_violin(alpha = 0.3) +
  labs(y = '%NPV/yr', x = 'Vegetation Type') +
  geom_boxplot(width = 0.25, alpha = 0) +
  scale_y_continuous(breaks = seq(-0.8, 1.2, 0.2), limits = c(-0.8, 1.2)) +
  theme(legend.position = "none") +
  annotate("text", x = 1, y = 1, label = "a") +
  annotate("text", x = 2, y = 1, label = "b") +
  annotate("text", x = 3, y = 1, label = "b")  +
  stat_summary(fun = mean, geom = "point", shape = 21, 
               size = 1, fill = "red") +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "red") 

for (i in 1:length(npv_mean_str)) {
  pl_npv <- pl_npv + annotate("text", x = i + .2, y = 1.1 , label = npv_mean_str[i],
                              hjust = 1, size = 4) 
}


pl_npv

bs_mean_str <- generate_mean_string(theil_sen_reg_copy, 'veg', 'bs')
pl_bs <- ggplot(data = theil_sen_reg_copy, 
                mapping = aes(y = bs_filter_slope_yr, x =vegetation_type)) +  
  theme_bw() + 
  geom_jitter(position = position_jitter(seed = 1, width = 0.15, height = 0),
              alpha = 0.5, 
              size = 1.5,
              fill = 'gold', 
              pch = 21) +
  geom_violin(alpha = 0.3) +
  labs(y = '%BS/yr', x = 'Vegetation Type') +
  geom_boxplot(width = 0.25, alpha = 0) +
  scale_y_continuous(breaks = seq(-0.8, 1.2, 0.2), limits = c(-0.8, 1.2)) +
  theme(legend.position = "none") +
  annotate("text", x = 1, y = 1, label = "a") +
  annotate("text", x = 2, y = 1, label = "ab") +
  annotate("text", x = 3, y = 1, label = "b") +
  stat_summary(fun = mean, geom = "point", shape = 21, 
               size = 1, fill = "red") +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "red")

for (i in 1:length(npv_mean_str)) {
  pl_bs <- pl_bs + annotate("text", x = i + .2, y = 1.1 , label = bs_mean_str[i],
                            hjust = 1, size = 4) 
}

combined_plot <- plot_grid(pl_pv, pl_npv, pl_bs, ncol = 2, labels = c("a)", "b)", "c)"))
combined_plot
ggsave(combined_plot, 
       height = 15.28,
       width = 17.2,
       units = 'cm',
       filename = 'FIGURES/Figure_3_Boxplots.png',
       dpi = 600,
       scale = 1.5,
       bg = 'white')



# FIGURE S2 ---------------------------------

pl_pv <- ggplot(data = theil_sen_reg_copy, 
                mapping = aes(x = pv_filter_slope_yr)) +  
  theme_bw() + 
  labs(x = '%PV/yr', y = 'count') +
  geom_histogram(fill = '#3182BD', color = 'black', alpha = 0.9, bins = 40) +
  #scale_y_continuous(breaks = seq(-0.8, 0.8, 0.2), limits = c(-0.8, 0.9)) +
  theme(legend.position = "none") +
  geom_vline(aes(xintercept = mean(pv_filter_slope_yr)), 
             linetype = "dashed", size = 1, color = 'black') +
  geom_vline(aes(xintercept = mean(pv_filter_slope_yr) + sd(pv_filter_slope_yr)), 
             linetype = "dashed", size = 1, color = 'red') +
  geom_vline(aes(xintercept = mean(pv_filter_slope_yr) - sd(pv_filter_slope_yr)), 
             linetype = "dashed", size = 1, color = 'red') +
  scale_y_continuous(breaks = seq(0, 150, 20), limits = c(0, 150)) +
  annotate('text', x = max(theil_sen_reg_copy$pv_filter_slope_yr) * 0.9, 
           y = 140, 
           label = paste0("x\u0305", ' = ',round(mean(theil_sen_reg_copy$pv_filter_slope_yr),4), 
                          '\ns = ', round(sd(theil_sen_reg_copy$pv_filter_slope_yr),4)), 
           color = "black", size = 3)

pl_pv


pl_npv <- ggplot(data = theil_sen_reg_copy, 
                 mapping = aes(x = npv_filter_slope_yr)) +  
  theme_bw() + 
  labs(x = '%NPV/yr', y = 'count') +
  geom_histogram(fill = '#3182BD', color = 'black', alpha = 0.9) +
  #scale_y_continuous(breaks = seq(-0.8, 0.8, 0.2), limits = c(-0.8, 0.9)) +
  theme(legend.position = "none") +
  geom_vline(aes(xintercept = mean(npv_filter_slope_yr)), 
             linetype = "dashed", size = 1, color = 'black') +
  geom_vline(aes(xintercept = mean(npv_filter_slope_yr) + sd(npv_filter_slope_yr)), 
             linetype = "dashed", size = 1, color = 'red') +
  geom_vline(aes(xintercept = mean(npv_filter_slope_yr) - sd(npv_filter_slope_yr)), 
             linetype = "dashed", size = 1, color = 'red') +
  scale_y_continuous(breaks = seq(0, 150, 20), limits = c(0, 150)) +
  annotate('text', x = max(theil_sen_reg_copy$npv_filter_slope_yr) * 0.9, 
           y = 140, 
           label = paste0("x\u0305", ' = ',round(mean(theil_sen_reg_copy$npv_filter_slope_yr),4), 
                          '\ns = ', round(sd(theil_sen_reg_copy$npv_filter_slope_yr),4)), 
           color = "black", size = 3)

pl_npv


pl_bs <- ggplot(data = theil_sen_reg_copy, 
                mapping = aes(x = bs_filter_slope_yr)) +  
  theme_bw() + 
  labs(x = '%BS/yr', y = 'count') +
  geom_histogram(fill = '#3182BD', color = 'black', alpha = 0.9) +
  #scale_y_continuous(breaks = seq(-0.8, 0.8, 0.2), limits = c(-0.8, 0.9)) +
  theme(legend.position = "none") +
  geom_vline(aes(xintercept = mean(bs_filter_slope_yr)), 
             linetype = "dashed", size = 1, color = 'black') +
  geom_vline(aes(xintercept = mean(bs_filter_slope_yr) + sd(bs_filter_slope_yr)), 
             linetype = "dashed", size = 1, color = 'red') +
  geom_vline(aes(xintercept = mean(bs_filter_slope_yr) - sd(bs_filter_slope_yr)), 
             linetype = "dashed", size = 1, color = 'red') +
  scale_y_continuous(breaks = seq(0, 150, 20), limits = c(0, 150)) +
  annotate('text', x = max(theil_sen_reg_copy$bs_filter_slope_yr) * 0.9, 
           y = 140, 
           label = paste0("x\u0305", ' = ',round(mean(theil_sen_reg_copy$bs_filter_slope_yr),4), 
                          '\ns = ', round(sd(theil_sen_reg_copy$bs_filter_slope_yr),4)), 
           color = "black", size = 3)

pl_bs

combined_plot <- plot_grid(pl_pv, pl_npv, pl_bs, ncol = 2, labels = c("a)", "b)", "c)"))
combined_plot

ggsave(combined_plot, 
       height = 10.32,
       width = 14.53,
       units = 'cm',
       filename = 'FIGURES/Figure_S2_FCHistogram.png',
       dpi = 600,
       scale = 1.4,
       bg = 'white')