# Load preliminaries
source("00_Utils.R")

# Load yellowfin tuna full dataset
YFT_ds <- read_csv("Output/YFT_full.csv", show_col_types = FALSE)

# Always build models with known data only
YFT_filtered <- YFT_ds %>% 
  dplyr::filter(!is.na(abundance)) %>% # filter those with data!
  dplyr::mutate(across(where(is.character), ~factor(.))) %>% # convert all characters to factors
  dplyr::mutate(abundance = case_when(abundance > 0 ~ 1,
                                      abundance == 0 ~ 0)) %>%  # mutate the abundance data into 1s and 0s
  as.data.frame() #gbm.step doesn't work if it's a tibble...

# Predict on the rest of the map
YFT_predict <- YFT_ds %>% 

#### Build the models ####
# check the index numbers of the columns
colnames(YFT_filtered)

# higher AUC better. A model with AUC values closer to 0 have more wrong predictions.
# see: https://rspatial.org/raster/sdm/9_sdm_brt.html for interpreting results
# model 0: season, longitude, and latitude as predictors
YFT_model0 <- dismo::gbm.step(data = YFT_filtered, gbm.x = 4:6,
                              gbm.y = 3, family = "bernoulli", tree.complexity = 5,
                              learning.rate = 0.01, bag.fraction = 0.5)
YFT_model0$fitted
YFT_model0$cv.statistics
summary(YFT_model0)

# model 1: season, longitude, latitude, climate data, bathymetry, distance to coast
YFT_model1 <- dismo::gbm.step(data = YFT_filtered, gbm.x = 4:11,
                              gbm.y = 3, family = "bernoulli", tree.complexity = 5,
                              learning.rate = 0.01, bag.fraction = 0.5)
YFT_model1$cv.statistics
summary(YFT_model1)

# model 2: model 1 without longitude and latitude
YFT_model2 <- dismo::gbm.step(data = YFT_filtered, gbm.x = c(4, 7:11),
                              gbm.y = 3, family = "bernoulli", tree.complexity = 5,
                              learning.rate = 0.01, bag.fraction = 0.5)
YFT_model2$cv.statistics
summary(YFT_model2)
