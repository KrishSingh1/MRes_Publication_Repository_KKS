#### AusPlots Site Classification by Veg Type #### 
# By Krish Singh
# Date: 18-04-25
# Purpose: To classify ausplots sites by veg type


# Libraries ---------------------------------------------------------------

library(ausplotsR)
library(dplyr)
library(fmsb)
library(tidyr)
library(hrbrthemes)

# Functions ---------------------------------------------------------------

get_location_name <- function(site.unique) {
  return(unlist(strsplit(site.unique, split =  '-'))[1])
}

classify <- function(dataset.row) {
  return(names(which.max(dataset.row[c("Grass","Shrub","Tree")])))
}


# Main --------------------------------------------------------------------

# 16-04-25 ----------------------------------------------------------------
# Testing the change in dominance 
one_point <- read.csv('../DATASETS/AusPlots_Extracted_Data/Final/DEA_FC_Ground_Truth_Evaluation_complete_Height_Rule_Agg.csv') 
growth.form <- read.csv('../DATASETS/AusPlots_Extracted_Data/Final/AusPlots_VegType_PC_Height_Rule.csv')

# Give row sum based on the groupings 
growth.form$vegetation_type <- unlist(apply(growth.form, MARGIN = 1, FUN = classify))
growth.form$site_location_name <- unlist(lapply(growth.form$site_unique, FUN = get_location_name))
growth.form.reduced <- growth.form %>%
  select(site_unique, site_location_name, Grass, Shrub, Tree, vegetation_type) %>%
  subset(site_location_name %in% unique(one_point$site_location_name))

# Now we want to only extract sites with multiple visits 
site_repeat_names <- names(which(table(growth.form.reduced$site_location_name) > 1))
site_change <- growth.form.reduced %>%
  subset(site_location_name %in% site_repeat_names)

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

#  SAABHC0004

site_change_long <- site_change %>% 
  subset(site_location_name %in% changed_sites) %>%
  select(c('site_unique', 'site_location_name', 'Tree', 'Shrub', 'Grass')) %>%
  left_join(site_date) %>%
  mutate(visit_date = as.Date(visit_date)) %>%
  pivot_longer(cols = c('Tree', 'Shrub', 'Grass')) %>%
  rename(vegetation_type = name,
         visit = value)
  

site_info <- readRDS('../DATASETS/AusPlots_Extracted_Data/Final/site_veg_Final2-0-6.rds')
site_date <- site_info$site.info %>%
  select(site_unique, site_location_name, visit_date) %>%
  subset(site_location_name %in% unique(one_point$site_location_name))


# Now I will reformat the dataset that is required for the plot 
sites_list <- list()
for (s in unique(site_change_long$site_location_name)){
  s_subset <- site_change_long %>% subset(site_location_name == s) %>%
    mutate(vegetation_type = factor(vegetation_type, 
                                    levels=c("Grass", "Shrub", "Tree"),
                                    ordered=TRUE))  %>%
    arrange(vegetation_type) # <- we also want to order by vegetation type 
    
  site_visits <- sort(unique(s_subset$visit_date)) # <-- so its sorted by chronological order  
  number_of_visits <- length(site_visits)
  
  site_visit_data <- data_frame(site_location_name = rep(s, 3),
                                vegetation_type = c('Grass', 'Shrub', 'Tree'))
  
  for (n in 1:number_of_visits) {
    visit_id <- site_visits[n] 
    visit_data <- s_subset %>% subset(visit_date == visit_id)
    print(visit_data)
    new_column_name <- paste0('visit_',n)
    colnames(visit_data)[which(colnames(visit_data) == 'visit')] = new_column_name
    site_visit_data <- cbind(site_visit_data, visit_data[new_column_name])
    
  }
  
  print(site_visit_data)
  sites_list[s] <- list(site_visit_data)
}
sites_list




# Plot
ggplot(sites_list$NTAARP0004) +
  geom_segment(aes(x = vegetation_type, xend = vegetation_type, y = visit_1, yend = visit_2), color = "grey") +
  geom_point(aes(x = vegetation_type, y = visit_1), color = 'red', size = 3) +
  geom_point(aes(x = vegetation_type, y = visit_2), color = 'green', size = 3) +
  ylim(c(0, 100)) +
  coord_flip() +
  geom_hline(yintercept = max(sites_list$NTAARP0004$visit_1),
             linetype = "dashed", color = 'red') +
  geom_hline(yintercept = max(sites_list$NTAARP0004$visit_2),
             linetype = "dashed", color = 'green') +
  theme_ipsum() +
  theme(legend.position = "none") +
  xlab("") +
  ylab("Percent Cover (%)") +
  annotate('text', label = 'a) NTAARP0004',
           y = 7, 
           x = 3.5) +
  annotate('text', label = 'Grass to Tree',
           y = 90, 
           x = 3.5)

# I want to generalise this code a bit more 



site_name <- 'SAABHC0004'

site_change_data <- sites_list[[site_name]]
number_of_visits <- length(colnames(site_change_data)) - 2 
colors <- c('red', 'green', 'steelblue')
veg_type <- c()

site_plot <- ggplot(site_change_data)
for (n in 1:(number_of_visits)) {
  
  vegetation_type <- site_change_data$vegetation_type[[which.max(site_change_data[, 2 + n])]] ## NOTE: THIS TAKES THE FIRST OCCURANCE! 
  veg_number <- which(site_change_data$vegetation_type == vegetation_type)
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
    site_plot <- site_plot + geom_hline(yintercept = max(site_change_data[,paste0('visit_', n)]),
               linetype = "dotted", color = colors[n]) +
      geom_point(aes(y = max(.data[[paste0('visit_', n)]])), 
                 x = veg_number,
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
  annotate('text', label = paste0(letters[1], ') ', site_name),
           y = 7, 
           x = 3.5) +
  annotate('text', label = paste(veg_type, collapse = ' to '),
           y = 90, 
           x = 3.5)
  
site_plot


# Now that is all generalised, lets make a function:

generate_plot <- function(site_name, 
                          colors = c('#fdcc8a', '#fc8d59', '#d7301f'),
                          site_change_data = site_change_data,
                          panel_label = 1)
{
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
             x = 3.5) +
    annotate('text', label = paste(veg_type, collapse = ' to '),
             y = 100, 
             x = 3.5,
             hjust = 1)
  
  
  return(site_plot)
}


site_names <- names(sites_list)
generate_plot('SAABHC0004',
              site_change_data = site_change_data,
              panel_label = 1)

list_plots <- list()
for(i in 1:length(site_names)) {
  list_plots[i] = list(generate_plot(site_names[i], 
                                colors = c('#fdcc8a', '#fc8d59', '#d7301f'),
                                site_change_data = site_change_data,
                                panel_label = i))
  
  
}

#6baed6
#3182bd
#08519c

library(cowplot)

{ # <- please run as code block to apply the seed 
set.seed(2) 
example_site_list_names <- sort(sample(site_names, size = 10))
}

list_plots_examples <- list()
for(i in 1:length(example_site_list_names)) {
  list_plots_examples[i] = list(generate_plot(example_site_list_names[i], 
                                     colors = c('#fdcc8a', '#fc8d59', '#d7301f'),
                                     site_change_data = site_change_data,
                                     panel_label = i))
  
  
}

plot_grid(plotlist = list_plots_examples, ncol = 2)

library(patchwork)
wrap_plots(list_plots[1], ncol = 1)

wrap_plots(list_plots_examples[1:10], ncol = 2) +
  plot_layout(guides = "collect") &  # use shared legend if any
  theme(plot.margin = margin(2, 2, 2, 2))  # global margin for all

# Getting some stats on how these growth forms transitioned 

get_change_in_dominance <- function(site_name,
                          site_change_data = site_change_data)
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

freq <- as.data.frame(table(unlist(lapply(site_names, FUN = get_change_in_dominance)))) %>%
  arrange(Var1) %>%
  rename(transition = Var1)

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



pl_pv <- ggplot(data = site_change_combined, 
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
pl_pv

summm <- site_change_combined %>%
  group_by(vegetation_type) %>%
  summarise(mean = mean(slope),
            med = median(slope),
            sd = sd(slope),
            min = min(slope),
            max = max(slope)) %>%
  mutate(across(where(is.numeric), round, 2))
  
