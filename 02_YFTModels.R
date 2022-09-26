# Load preliminaries
#source("01a_DataLayers_Assembling.R")

# always build models with known data only!
yft_full <- YFT_full %>% 
  tibble::as_tibble() %>% 
  dplyr::filter(!is.na(abundance)) %>%  # filter those with data!
  dplyr::mutate(abundance = case_when(abundance > 0 ~ 1,
                                      abundance == 0 ~ 0)) %>% # mutate the abundance data into 1s and 0s
  as.data.frame() # gbm.step doesn't work if it's a tibble

# dummy
# see: https://rspatial.org/raster/sdm/9_sdm_brt.html for interpreting results
YFT.model0 <- dismo::gbm.step(data = yft_full, gbm.x = 4:6,
                              gbm.y = 3, family = "bernoulli", tree.complexity = 5,
                              learning.rate = 0.01, bag.fraction = 0.5)
YFT.model0$fitted
YFT.model0$cv.statistics
summary(YFT.model0)

# climate with coordinates
YFT.model1 <- dismo::gbm.step(data = yft_full, gbm.x = 4:10,
                              gbm.y = 2, family = "bernoulli", tree.complexity = 5,
                              learning.rate = 0.01, bag.fraction = 0.5)
summary(YFT.model1)
