# Load preliminaries
source("00_Utils.R")

# Load yellowfin tuna full dataset
YFT_ds <- read_csv("Output/YFT_full.csv", show_col_types = FALSE)

# Always build models with known data only
YFT_filtered <- YFT_ds %>% 
  dplyr::filter(!is.na(abundance)) %>% # filter those with data!
  dplyr::mutate(across(where(is.character), ~factor(.))) %>% # convert all characters to factors
  dplyr::mutate(abundance_presence = case_when(abundance > 0 ~ 1,
                                      abundance == 0 ~ 0)) %>%  # mutate the abundance data into 1s and 0s
  dplyr::select(-geometry) %>% 
  as.data.frame() #gbm.step doesn't work if it's a tibble...

#### Build the models ####
# check the index numbers of the columns
colnames(YFT_filtered)

# higher AUC better. A model with AUC values closer to 0 have more wrong predictions.
# see: https://rspatial.org/raster/sdm/9_sdm_brt.html for interpreting results
# model 1: all predictors
YFT_model0 <- dismo::gbm.step(data = YFT_filtered, gbm.x = 4:12,
                              gbm.y = 13, family = "bernoulli", tree.complexity = 5,
                              learning.rate = 0.01, bag.fraction = 0.5, n.folds = 5)
length(YFT_model0$fitted)
YFT_model0$cv.statistics
summary(YFT_model0)

gbm.plot(YFT_model0, n.plots=11, plot.layout=c(4, 3), write.title = FALSE)
gbm.plot.fits(YFT_model0)

# Check the plot
YFT_filtered$model0 <- YFT_model0$fitted
YFT_sf <- grid_YFT %>% # convert to sf so we can plot
  dplyr::select(-species, -abundance, -season, -longitude, -latitude) %>% 
  dplyr::left_join(YFT_filtered, ., by = "cellID") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

ggmodel <- ggplot() + 
  geom_sf(data = YFT_sf, aes(fill = model0, color = model0), size = 0.1) +
  scale_fill_cmocean(name = "matter", aesthetics = c("color", "fill")) +
  geom_sf(data = landmass, fill = "black", color = NA, size = 0.01) +
  theme_bw()
ggsave(plot = ggmodel, filename = "Figures/model0.png", width = 15, height = 8, dpi = 300)

library(RColorBrewer) # TODO: Add this in utils
ggabundance <- ggplot() + 
  geom_sf(data = YFT_sf, aes(fill = as.factor(abundance), color = as.factor(abundance)), size = 0.1) +
  scale_fill_brewer(aesthetics = c("fill", "color"),
                    palette = "RdPu") +
  geom_sf(data = landmass, fill = "black", color = NA, size = 0.01) +
  theme_bw()
ggsave(plot = ggabundance, filename = "Figures/abundance.png", width = 15, height = 8, dpi = 300)

ggpresence <- ggplot() +
  geom_sf(data = YFT_sf, aes(fill = as.factor(abundance_presence), color = as.factor(abundance_presence)), size = 0.1) +
  scale_color_manual(aesthetics = c("color", "fill"),
    values = c("pink", "red")
                     ) +
  geom_sf(data = landmass, fill = "black", color = NA, size = 0.01) +
  theme_bw()

ggsave(plot = ggpresence, filename = "Figures/abundance_presence.png", width = 15, height = 8, dpi = 300)
