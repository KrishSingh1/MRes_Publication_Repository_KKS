#### AusPlots Site Classification by Veg Type #### 
# By Krish Singh
# Date: 18-04-25
# Purpose: To classify ausplots sites by veg type


# Libraries ---------------------------------------------------------------

library(ausplotsR)
library(dplyr)
library(fmsb)
library(tidyr)
library(devtools)
#install.packages('extrafont')
#install.packages('C:/Users/Krish Singh (Work)/Downloads/hrbrthemes_0.8.7.tar.gz', type = 'source') #  https://cran.r-project.org/src/contrib/Archive/hrbrthemes/
library(hrbrthemes) # Install packages above as it was removed from cran repo
library(cowplot)
library(grid)

# Functions ---------------------------------------------------------------

get_location_name <- function(site.unique) {
  return(unlist(strsplit(site.unique, split =  '-'))[1])
}

classify <- function(dataset.row) {
  return(names(which.max(dataset.row[c("Grass","Shrub","Tree")])))
}

generate_plot <- function(site_name, 
                          colors = c('#fdcc8a', '#fc8d59', '#d7301f'),
                          site_change_data = site_change_data,
                          panel_label = 1) {
  print(site_name)
  site_change_data <- sites_list[[site_name]]
  number_of_visits <- length(colnames(site_change_data)) - 2 
  veg_type <- c()
  
  site_plot <- ggplot(site_change_data)
  for (n in 1:(number_of_visits)) {
    
    vist_data <- site_change_data[, paste0('visit_', n)]
    veg_name <- site_change_data[, 'vegetation_type']
    veg_number <- which(vist_data == max(vist_data))
    vegetation_type <- paste(veg_name[veg_number], collapse = '/')
    print(veg_number)
    
    if (n < number_of_visits){
      site_plot <- site_plot + 
        geom_segment(aes(x = vegetation_type, 
                         xend = vegetation_type,
                         y = .data[[paste0('visit_', n)]], 
                         yend = .data[[paste0('visit_', n + 1)]]), 
                     color = "grey")
    } 
    site_plot <- site_plot +
      geom_point(aes(y = .data[[paste0('visit_', n)]], x = vegetation_type),
                 color = colors[n], size = 3)
    
    # Basicaly only draw a line at the first visit and at any visit where the dominance changes
    if(n == 1 || (veg_type[length(veg_type)] != vegetation_type && n > 1) ) {
      veg_type <- c(veg_type, vegetation_type)
      max_pcs <- data.frame(y = rep(max(vist_data), length(veg_number)),
                            x = veg_number)
      print(max_pcs)
      
      site_plot <- site_plot + geom_hline(yintercept = max(site_change_data[,paste0('visit_', n)]),
                                          linetype = "dashed", color = colors[n]) +
        geom_point(data = max_pcs, mapping = aes(y = y, x = x),
                   shape = 21, fill =  colors[n], size = 5)
      
    } 
  }
  
  site_plot <- site_plot +
    ylim(c(0, 100)) +
    coord_flip() +
    theme_ipsum() +
    theme(legend.position = "none") +
    xlab("") +
    ylab("Percent Cover (%)") +
    annotate('text', label = paste0(letters[panel_label], ') ', site_name),
             y = 7, 
             x = 3.5,
             size = 3) +
    annotate('text', label = paste(veg_type, collapse = ' to '),
             y = 90, 
             x = 3.5,
             hjust = 1, 
             size = 3)
  
  
  return(site_plot)
}

get_change_in_dominance <- function(site_name,
                                    site_change_data)
{
  site_change_data <- sites_list[[site_name]]
  number_of_visits <- length(colnames(site_change_data)) - 2 
  veg_type <- c()
  
  site_plot <- ggplot(site_change_data)
  for (n in 1:(number_of_visits)) {
    vist_data <- site_change_data[, paste0('visit_', n)]
    veg_name <- site_change_data[, 'vegetation_type']
    vegetation_type <- veg_name[which(vist_data == max(vist_data))]
    vegetation_type <- paste(vegetation_type, collapse = '/')
    
    if(n == 1 || (veg_type[length(veg_type)] != vegetation_type && n > 1) ) {
      veg_type <- c(veg_type, vegetation_type)
    } 
  }
  
  ret_string <- paste(veg_type, collapse = ' to ')
  return(ret_string)
}

classify_all_visit <- function(dataset.row) {
  return(names(which.max(dataset.row[c("mean_grass","mean_shrub","mean_tree")])))
}

# Main --------------------------------------------------------------------


# Datasets ----------------------------------------------------------------

one_point <- read.csv('DATASETS/DEA_FC_Ground_Truth_Evaluation_complete_Height_Rule_Agg.csv') # The evaluation data, detailing what data we have DEA and on-ground FC
growth.form <- read.csv('DATASETS/AusPlots_VegType_PC_Height_Rule.csv') # Every site collected and their PC of growth form 
site_visits <- unique(one_point[,c('site_unique', 'visit_start_date')]) # Get unique number of visits from the evaluation data 
site_info <- readRDS('DATASETS/site_veg_Final2-0-6.rds') # FC data 

# Site Veg Dominance classification ---------------------------------------
growth.form$vegetation_type <- unlist(apply(growth.form, MARGIN = 1, FUN = classify)) # Classify dominant veg type 
growth.form$site_location_name <- unlist(lapply(growth.form$site_unique, FUN = get_location_name)) # Obtain location name from site_unique
growth.form.reduced <- growth.form %>%
  select(site_unique, site_location_name, Grass, Shrub, Tree, vegetation_type) %>%
  filter(site_location_name %in% unique(one_point$site_location_name)) # Now subset the growth.form data by what's avaliable in the eval data 

# Now we want to only extract sites with multiple visits 
site_repeat_names <- growth.form.reduced %>%
  group_by(site_location_name) %>%
  mutate(aval_num_visits = n()) %>%
  mutate(mean_grass = mean(Grass),
         mean_shrub = mean(Shrub),
         mean_tree = mean(Tree)) %>%
  ungroup() 
  
# Classify dominant veg type across all visits
site_repeat_names$avg_veg_type <- unlist(apply(site_repeat_names, MARGIN = 1, FUN = classify_all_visit))  
site_unique_visits <-  site_repeat_names %>%
  select(site_location_name, avg_veg_type, aval_num_visits) %>%
  unique() 

# Ploting the number of visits 
site_visit_dist <- site_unique_visits %>%
  ggplot(mapping = aes(x = aval_num_visits)) +
  geom_bar(fill = '#4a6741') +
  theme_bw() +
  geom_text(stat = 'count',
            mapping = aes(label = ..count..),
            nudge_y = 20)

site_veg_type_visit_dist <- site_unique_visits %>%
  ggplot(mapping = aes(x = aval_num_visits)) +
  geom_bar(fill = '#4a6741') +
  facet_grid(~avg_veg_type) +
  theme_bw() +
  geom_text(stat = 'count',
            mapping = aes(label = ..count..),
            nudge_y = 10)

combined_plot <- plot_grid(site_visit_dist, site_veg_type_visit_dist,  ncol = 1, labels = c("a)", "b)"))
plot(combined_plot)

ggsave(filename = 'Supplementary_Information/site_visit_counts.png',
       dpi = 300, 
       plot = combined_plot,
       bg = 'white')


site_change <- site_repeat_names %>%
  filter(aval_num_visits > 1)
# Now check which sites have actually changed dominance 
sites <- unique(site_change$site_location_name)
changed_sites <- c()
for(s in sites) {
  site_pc <- site_change %>% subset(site_location_name == s) 
  unique_veg_types <- unique(site_pc$vegetation_type) # count the unique dominant vegetation types 
  if(length(unique_veg_types) > 1){
    changed_sites <- c(changed_sites, s)
  }
}
print(length(changed_sites)) # number of sites that changed dominance


# Plot the counts of the change in dominance 
change_in_dom_veg_type <- site_change %>%
  select(site_location_name, avg_veg_type, aval_num_visits) %>%
  unique() %>%
  filter(site_location_name %in% changed_sites) %>%
  ggplot(mapping = aes(x = avg_veg_type)) +
  geom_bar(fill = '#4a6741') +
  theme_bw() +
  geom_text(stat = 'count',
            mapping = aes(label = ..count..),
            nudge_y = 1)

ggsave(filename = 'Supplementary_Information/site_veg_change_dist.png',
       dpi = 300, 
       plot = change_in_dom_veg_type,
       bg = 'white')

# Process data for more advanced plotting ---------------------------------
site_change_long <- site_change %>% 
  subset(site_location_name %in% changed_sites) %>%
  select(c('site_unique', 'site_location_name', 'Tree', 'Shrub', 'Grass')) %>%
  left_join(site_visits) %>%
  mutate(visit_start_date = as.Date(visit_start_date)) %>%
  pivot_longer(cols = c('Tree', 'Shrub', 'Grass')) %>%
  rename(vegetation_type = name,
         visit = value)

# Now I will reformat the dataset that is required for the lolipop plot
# Essentially, a list that has each site's PC of veg type per visit 
sites_list <- list()
for (s in unique(site_change_long$site_location_name)){
  s_subset <- site_change_long %>% subset(site_location_name == s) %>%
    mutate(vegetation_type = factor(vegetation_type, 
                                    levels=c("Grass", "Shrub", "Tree"),
                                    ordered=TRUE))  %>%
    arrange(vegetation_type) # <- we also want to order by vegetation type 
    
  site_visits <- sort(unique(s_subset$visit_start_date)) # <-- so its sorted by chronological order  
  number_of_visits <- length(site_visits)
  
  site_visit_data <- data_frame(site_location_name = rep(s, 3),
                                vegetation_type = c('Grass', 'Shrub', 'Tree'))
  
  for (n in 1:number_of_visits) {
    visit_id <- site_visits[n] 
    visit_data <- s_subset %>% subset(visit_start_date == visit_id)
    print(visit_data)
    new_column_name <- paste0('visit_',n)
    colnames(visit_data)[which(colnames(visit_data) == 'visit')] = new_column_name
    site_visit_data <- cbind(site_visit_data, visit_data[new_column_name])
    
  }
  
  print(site_visit_data)
  sites_list[s] <- list(site_visit_data)
}
sites_list

# Make Lolipop Chart for Change in Dominance ------------------------------
site_names <- names(sites_list)
generate_plot('SAABHC0004',  site_change_data = site_change_data, panel_label = 1)

list_plots <- list()
for(i in 1:length(site_names)) {
  list_plots[i] = list(generate_plot(site_names[i], 
                                colors = c('#fdcc8a', '#fc8d59', '#d7301f'),
                                site_change_data = site_change_data,
                                panel_label = i))
  
  
}


{ # <- please run as code block to apply the seed 
set.seed(2) 
example_site_list_names <- sort(sample(site_names, size = 9))
}

list_plots_examples <- list()
for(i in 1:length(example_site_list_names)) {
  list_plots_examples[i] = list(generate_plot(example_site_list_names[i], 
                                     colors = c('#fdcc8a', '#fc8d59', '#d7301f'),
                                     site_change_data = site_change_data,
                                     panel_label = i))
  
  
}

change_in_veg_dominance <-plot_grid(plotlist = list_plots_examples, ncol = 3, scale = 1.2)
ggsave(filename = 'Supplementary_Information/change_in_veg_dominance.png',
       plot = change_in_veg_dominance,
       dpi = 300,
       scale = 1.2,
       height = 7,
       width = 13)



# Get Change in dominance frequency ---------------------------------------

freq <- as.data.frame(table(unlist(lapply(site_names, FUN = get_change_in_dominance, site_change_data = site_change_data)))) %>%
  arrange(Var1) %>%
  rename(transition = Var1) %>%
  arrange(Freq)

# Plot these transitions using barplots 

site_transitions_plot <- freq %>%
  ggplot(mapping = aes(y = Freq, x = reorder(transition, -Freq))) +
  geom_bar(fill = '#4a6741', stat = "identity" ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  xlab('Transition Type') 
  
ggsave(filename = 'Supplementary_Information/site_transitions_plot.png',
       plot = site_transitions_plot,
       dpi = 300)


# Now Plot the Trends -----------------------------------------------------
# PC vs Visit Number 

site_change_grass <- site_change %>% 
  left_join(site_info$site.info[,c('site_unique', 'visit_date', 'visit_number')]) %>%
  arrange(visit_number) %>%
  group_by(site_location_name) %>%
  do({
  model <- lm(Grass~ visit_number, data = .)
  data.frame(
    intercept = coef(model)[1],
    slope = coef(model)[2],
    r2 = summary(model)$r.squared,
    p_value_intercept = summary(model)$coefficients[, "Pr(>|t|)"][1],
    p_value_slope = summary(model)$coefficients[, "Pr(>|t|)"][2],
    vegetation_type = 'Grass'
  )})

site_change_shrub <- site_change %>% 
  left_join(site_info$site.info[,c('site_unique', 'visit_date', 'visit_number')]) %>%
  arrange(visit_number) %>%
  group_by(site_location_name) %>%
  do({
    model <- lm(Shrub~ visit_number, data = .)
    data.frame(
      intercept = coef(model)[1],
      slope = coef(model)[2],
      r2 = summary(model)$r.squared,
      p_value_intercept = summary(model)$coefficients[, "Pr(>|t|)"][1],
      p_value_slope = summary(model)$coefficients[, "Pr(>|t|)"][2],
      vegetation_type = 'Shrub'
    )})

site_change_tree <- site_change %>% 
  left_join(site_info$site.info[,c('site_unique', 'visit_date', 'visit_number')]) %>%
  arrange(visit_number) %>%
  group_by(site_location_name) %>%
  do({
    model <- lm(Tree~ visit_number, data = .)
    data.frame(
      intercept = coef(model)[1],
      slope = coef(model)[2],
      r2 = summary(model)$r.squared,
      p_value_intercept = summary(model)$coefficients[, "Pr(>|t|)"][1],
      p_value_slope = summary(model)$coefficients[, "Pr(>|t|)"][2],
      vegetation_type = 'Tree'
    )})

site_change_combined <- bind_rows(site_change_grass, site_change_shrub, site_change_tree)



plt_trends <- ggplot(data = site_change_combined, 
                mapping = aes(y = slope, x = vegetation_type)) +  
  theme_bw() + 
  geom_jitter(position = position_jitter(seed = 1, width = 0.15), alpha = 0.5, size = 1.5, color = '#3182BD') +
  geom_violin(alpha = 0.3) +
  labs(y = 'Percent Cover per Visit', x = 'Vegetation Type') +
  geom_boxplot(width = 0.25, alpha = 0) +
  theme(legend.position = "none") +
  stat_summary(fun = mean, geom = "point", shape = 21, 
               size = 1, fill = "red") +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "red")

ggsave(filename = 'Supplementary_Information/pc_per_visit__trends_box_plt.png',
       plot = plt_trends,
       dpi = 300)



# Some stats about trends -------------------------------------------------
summm <- site_change_combined %>%
  group_by(vegetation_type) %>%
  summarise(mean = mean(slope),
            med = median(slope),
            sd = sd(slope),
            min = min(slope),
            max = max(slope)) %>%
  mutate(across(where(is.numeric), round, 2))

t.test(site_change_grass$slope)
# One Sample t-test
# 
# data:  site_change_grass$slope
# t = 0.46518, df = 160, p-value = 0.6424
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   -1.665863  2.692453
# sample estimates:
#   mean of x 
# 0.5132947 

t.test(site_change_shrub$slope)
# One Sample t-test
# 
# data:  site_change_shrub$slope
# t = -2.5077, df = 160, p-value = 0.01315
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   -3.675276 -0.436820
# sample estimates:
#   mean of x 
#-2.056048 

t.test(site_change_tree$slope)
# One Sample t-test
# 
# data:  site_change_tree$slope
# t = -0.24988, df = 160, p-value = 0.803
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   -1.380314  1.070247
# sample estimates:
#   mean of x 
# -0.1550338 


# Do a one way anova 
aov_pc_change_per_visit <- aov(slope~vegetation_type, data = site_change_combined)
  
summary(aov_pc_change_per_visit)
# Df Sum Sq Mean Sq F value Pr(>F)  
# vegetation_type   2    572   286.1   2.344 0.0971 .
# Residuals       480  58597   122.1                 
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


TukeyHSD(aov_pc_change_per_visit)
# $vegetation_type
# diff        lwr       upr     p adj
# Shrub-Grass -2.5693426 -5.4644976 0.3258124 0.0937068
# Tree-Grass  -0.6683285 -3.5634835 2.2268265 0.8502437
# Tree-Shrub   1.9010141 -0.9941409 4.7961691 0.2714738






  
