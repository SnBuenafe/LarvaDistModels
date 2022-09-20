source("00_DataLayers_Utils.R")

inPath <- "Data/"

# assemble data
YFTsf1 <- readRDS(paste0(inPath, "Fish/", "VectorFile_", "yellowfin-tuna", "_jan-mar.rds")) %>% 
  tibble::as_tibble()
YFTsf2 <- readRDS(paste0(inPath, "Fish/", "VectorFile_", "yellowfin-tuna", "_apr-jun.rds")) %>% 
  tibble::as_tibble()
YFTsf3 <- readRDS(paste0(inPath, "Fish/", "VectorFile_", "yellowfin-tuna", "_jul-sept.rds")) %>% 
  tibble::as_tibble()
YFTsf4 <- readRDS(paste0(inPath, "Fish/", "VectorFile_", "yellowfin-tuna", "_oct-dec.rds")) %>% 
  tibble::as_tibble()

# join all data.frames
YFT <- dplyr::bind_rows(YFTsf1, YFTsf2, YFTsf3, YFTsf4) %>% 
  dplyr::mutate(across(where(is.character), ~factor(.)),
                abundance = ifelse(abundance > 0, yes = 1, no = 0)) %>% 
  dplyr::select(species, abundance, everything()) #%>% 
#  as.data.frame()
YFTsf <- YFT %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  sf::st_centroid()

#### Calling climatology data ####
tos <- nc2sf(model = "ACCESS-ESM1-5",
             expt = "ssp585",
             var = "tos") %>% 
  dplyr::rename(tos = mean) %>% 
  tibble::as_tibble() #%>% 
  #sf::st_join(., YFTsf, join = st_contains_properly, left = TRUE) # join with the abundance data
o2os <- nc2sf(model = "ACCESS-ESM1-5",
              expt = "ssp585",
              var = "o2os") %>% 
  dplyr::rename(o2os = mean) %>% 
  tibble::as_tibble() #%>% 
  #sf::st_join(., YFTsf, join = st_contains_properly, left = TRUE) # join with the abundance data
phos <- nc2sf(model = "CMCC-ESM2",
              expt = "ssp585",
              var = "phos") %>% 
  dplyr::rename(phos = mean) %>% 
  tibble::as_tibble()# %>% 
 # sf::st_join(., YFTsf, join = st_contains_properly, left = TRUE) # join with the abundance data

join <- dplyr::left_join(tos, o2os, by = "geometry") %>% 
  dplyr::left_join(., phos, by = "geometry") %>% 
  dplyr::mutate(cellID = row_number()) %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% # make it into an sf
  sf::st_join(., YFTsf, join = st_contains_properly, left = TRUE) %>%  # join with the abundance data
  dplyr::select(cellID, abundance, species, season, longitude, latitude, tos, o2os, phos, everything()) # arrange columns

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
