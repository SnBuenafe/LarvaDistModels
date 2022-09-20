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
  dplyr::mutate(cellID = factor(row_number())) %>% 
  dplyr::mutate(across(where(is.character), ~factor(.)),
                abundance = ifelse(abundance > 0, yes = 1, no = 0)) %>% 
  dplyr::select(cellID, species, abundance, everything()) #%>% 
#  as.data.frame()
YFTsf <- YFT %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  sf::st_centroid()

#### Calling climatology data ####
tos <- nc2sf(model = "ACCESS-ESM1-5",
             expt = "ssp585",
             var = "tos") %>% 
  dplyr::mutate(cellID = row_number()) %>% 
  dplyr::rename(tos = mean)
o2os <- nc2sf(model = "ACCESS-ESM1-5",
              expt = "ssp585",
              var = "o2os") %>% 
  dplyr::mutate(cellID = row_number()) %>% 
  dplyr::rename(o2os = mean)
phos <- nc2sf(model = "CMCC-ESM2",
              expt = "ssp585",
              var = "phos") %>% 
  dplyr::mutate(cellID = row_number()) %>% 
  dplyr::rename(phos = mean)

tos_df <- tibble::as_tibble(tos)
YFT_df <- tibble::as_tibble(YFTsf)

join <- dplyr::left_join(tos_df, YFT_df, by = "geometry")

join <- sf::st_join(tos, YFTsf, join = st_contains_properly, left = TRUE) %>% 
  dplyr::filter(!is.na(abundance))

# dummy
YFT.model0 <- dismo::gbm.step(data = YFT, gbm.x = 4:6,
                              gbm.y = 3, family = "bernoulli", tree.complexity = 5,
                              learning.rate = 0.01, bag.fraction = 0.5)


angaus.tc5.lr01 <- gbm.step(data = Anguilla_train, gbm.x = 3:13, gbm.y = 2,
                            family = "bernoulli", tree.complexity = 5,
                            learning.rate = 0.01, bag.fraction = 0.5)
