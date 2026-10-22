### Ternary Plot ###
# 10/04/25
# Krish Karan Singh
# Purpose: to plot a Ternary plot showing where the collected AusPlots sites lie
#          in the spectrum of grass-shrub-tree cover.


# Library -----------------------------------------------------------------

library(dplyr)
library(Ternary)
library(rsvg)

# Set up ------------------------------------------------------------------

class_order <- c('Grass', 'Shrub', 'Tree')

ausplots_data <- read.csv('DATASETS/AusPlots_Agg_VegType_PC_Height_Rule.csv') %>%
  select(all_of(c('site_location_name', class_order)))

# Make sure to only filter for sites we got DEA FC data for
ausplots_data_original <- read.csv('DATASETS/DEA_FC_Ground_Truth_Evaluation_complete_Height_Rule_Agg.csv') 
common <- intersect(ausplots_data$site_location_name, ausplots_data_original$site_location_name)

ausplots_data <- ausplots_data %>% 
  subset(site_location_name %in% common)

fc <- read.csv('DATASETS/DEA_FC_Ground_Truth_Evaluation_complete_Height_Rule_Agg.csv') %>%
  subset(fraction_type == 'green') %>%
  select(c(site_location_name, on_ground_value)) %>%
  group_by(site_location_name) %>%
  summarise(mean_pv = mean(on_ground_value)) %>%
  as.data.frame()


PC_agg_norm <- ausplots_data %>% left_join(fc) %>%
  na.omit()


# Main --------------------------------------------------------------------


# Figure S3 ---------------------------------------------------------------
spectrumBins <- 100 # Number of bins to use
mySpectrum <- hcl.colors(spectrumBins, palette = "viridis")
binnedGrowthForm <- cut(PC_agg_norm$mean_pv, spectrumBins)
pointCol <- mySpectrum[binnedGrowthForm]

middle_triangle <- matrix(c(
  50, 50, 0,
  50, 0, 50,
  0, 50, 50,
  50, 50, 0
), ncol = 3, byrow = TRUE)


ragg::agg_png(
  "FIGURES/Figure_S3_Ternary_Plot.png",
  width = 12,
  height = 12,
  units = "cm",
  res = 600
)

par(mar = c(0.5, 0.5, 0.5, 0.5), family = "serif")
Ternary::TernaryPlot(alab = class_order[1], 
            blab = class_order[2], 
            clab = class_order[3],
            grid.minor.lines = 0)
Ternary::TernaryPoints(coordinates = PC_agg_norm[,class_order], col = pointCol, pch = 16)
Ternary::TernaryLines(middle_triangle, col = "red", lwd = 2)

# Legend for colour scale
PlotTools::SpectrumLegend(
  "topleft",
  cex = 0.8, # Font size
  palette = mySpectrum,
  legend = paste(
    round(seq(from = max(PC_agg_norm$mean_pv, spectrumBins), to = min(0, spectrumBins),
              length.out = 5)),
    "%"
  ),
  bty = "n",
  xpd = NA, 
  title = "Photosynthetic Vegetation Cover (%)"
)

dev.off()
