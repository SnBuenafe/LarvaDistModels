## Assemble climate data

#### Calling climatology data ####


# joining all climatology data with the YFT abundance data
join <- dplyr::left_join(tos, o2os, by = "geometry") %>% 
  dplyr::left_join(., phos, by = "geometry") %>% 
  dplyr::mutate(cellID = row_number()) %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% # make it into an sf
  sf::st_join(., YFTsf, join = st_contains_properly, left = TRUE) %>%  # join with the abundance data
  dplyr::select(cellID, abundance, species, season, longitude, latitude, tos, o2os, phos, everything()) # arrange columns

#### Calling GEBCO (bathymetry) data
# call raster file


# aggregate, factor of 10
# convert to sf
# interpolate into PUs so we get the mean depth in each cell and use that?

# always build models with known data only!
yft_full <- join %>% 
  tibble::as_tibble() %>% 
  dplyr::filter(!is.na(abundance)) %>%  # filter those with data!
  as.data.frame() # gbm.step doesn't work if it's a tibble

# dummy
# see: https://rspatial.org/raster/sdm/9_sdm_brt.html for interpreting results
YFT.model0 <- dismo::gbm.step(data = yft_full, gbm.x = 4:6,
                              gbm.y = 2, family = "bernoulli", tree.complexity = 5,
                              learning.rate = 0.01, bag.fraction = 0.5)
YFT.model0$fitted
YFT.model0$cv.statistics
summary(YFT.model0)

# climate with coordinates
YFT.model1 <- dismo::gbm.step(data = yft_full, gbm.x = 4:9,
                              gbm.y = 2, family = "bernoulli", tree.complexity = 5,
                              learning.rate = 0.01, bag.fraction = 0.5)
summary(YFT.model1)
