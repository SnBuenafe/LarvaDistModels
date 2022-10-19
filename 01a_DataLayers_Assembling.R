# Load preliminaries
source("00_Utils.R")

####################
# Fish data #
####################
load = TRUE # if TRUE, scripts are only reloaded; none are reran

if(isTRUE(load)) {
  # 1. Assemble yellowfin tuna data
  YFT_sf <- combineFish(species = "yellowfin-tuna") %>% 
    sf::st_transform(crs = moll) %>% 
    sf::st_centroid() # transform into point data
  
  grid_YFT <- sf::st_join(grid, YFT_sf, join = st_contains_properly, left = TRUE) %>% # join with the grid data if the centroid is contained within the grid
    dplyr::as_tibble()
  
  # 2. Assemble albacore data
  ALB_sf <- combineFish(species = "albacore") %>% 
    sf::st_transform(crs = moll) %>% 
    sf::st_centroid()# transform into point data
  
  grid_ALB <- sf::st_join(grid, ALB_sf, join = st_contains_properly, left = TRUE) %>% # join with the grid data if the centroid is contained within the grid
    dplyr::as_tibble()
  
  # 3. Assemble skipjack tuna
  SKP_sf <- combineFish(species = "skipjack-tuna") %>% 
    sf::st_transform(crs = moll) %>% 
    sf::st_centroid()# transform into point data
  
  grid_SKP <- sf::st_join(grid, SKP_sf, join = st_contains_properly, left = TRUE) %>% # join with the grid data if the centroid is contained within the grid
    dplyr::as_tibble()
  
  # 4. Assemble swordfish
  SWO_sf <- combineFish(species = "swordfish") %>% 
    sf::st_transform(crs = moll) %>% 
    sf::st_centroid()# transform into point data
  
  grid_SWO <- sf::st_join(grid, SWO_sf, join = st_contains_properly, left = TRUE) %>% # join with the grid data if the centroid is contained within the grid
    dplyr::as_tibble()
  
} else {

####################
# Predictor data #
####################
# Reprocess?
reprocess = FALSE # Change to FALSE if no (reprocess=TRUE takes a while to run)

if(isTRUE(reprocess)) {
  # Climate
  # 1. Temperature
  tos_rs <- stars::read_ncdf("Data/Climatology/ensemble/tos_ensemble.nc") %>% 
    terra::rast()
  names(tos_rs) <- paste0("X", seq(1956, 1984, by = 1))
  tos <- rs2sf(tos_rs) %>% 
    dplyr::rename(tos = mean) %>% # using the mean of the models
    sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
    dplyr::as_tibble() %>% 
    dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
    sf::st_as_sf(crs = moll) %>% 
    replaceNN(., grid, "tos") %>%
    dplyr::as_tibble() %>% 
    dplyr::select(cellID, tos_transformed, geometry)
  saveRDS(tos, "Data/Climatology/sf/tos_interpolated.rds")
  
  # 2. Oxygen
  o2os_rs <- stars::read_ncdf("Data/Climatology/ensemble/o2os_ensemble.nc") %>% 
    terra::rast()
  names(o2os_rs) <- paste0("X", seq(1956, 1984, by = 1))
  o2os <- rs2sf(o2os_rs) %>% 
    dplyr::rename(o2os = mean) %>% # using the mean of the models
    sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
    dplyr::as_tibble() %>% 
    dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
    sf::st_as_sf(crs = moll) %>% 
    replaceNN(., grid, "o2os") %>%
    dplyr::as_tibble() %>% 
    dplyr::select(cellID, o2os_transformed, geometry)
  saveRDS(o2os, "Data/Climatology/sf/o2os_interpolated.rds")
  
  # 3. pH
  phos_rs <- stars::read_ncdf("Data/Climatology/ensemble/phos_ensemble.nc") %>% 
    terra::rast()
  names(phos_rs) <- paste0("X", seq(1956, 1984, by = 1))
  phos <- rs2sf(phos_rs) %>% 
    dplyr::rename(phos = mean) %>% # using the mean of the models
    sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
    dplyr::as_tibble() %>% 
    dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
    sf::st_as_sf(crs = moll) %>% 
    replaceNN(., grid, "phos") %>%
    dplyr::as_tibble() %>% 
    dplyr::select(cellID, phos_transformed, geometry)
  saveRDS(phos, "Data/Climatology/sf/phos_interpolated.rds")
  
  # 4. chlorophyll-a
  chlos_rs <- stars::read_ncdf("Data/Climatology/ensemble/chlos_ensemble.nc") %>% 
    terra::rast()
  names(chlos_rs) <- paste0("X", seq(1956, 1984, by = 1))
  chlos <- rs2sf(chlos_rs) %>% 
    dplyr::rename(chlos = mean) %>% # using the mean of the models
    sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
    dplyr::as_tibble() %>% 
    dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
    sf::st_as_sf(crs = moll) %>% 
    replaceNN(., grid, "chlos") %>%
    dplyr::as_tibble() %>% 
    dplyr::select(cellID, chlos_transformed, geometry)
  saveRDS(chlos, "Data/Climatology/sf/chlos_interpolated.rds")
  
  # Bathymetry
  bathy <- gebcoConvert(grid, 2500) # bathymetry data is extrapolated depending on the grid area provided
  
  # Coastline
  dist2coast <- calculateDist2Coast(grid) # distance to coast is calculated depending on the grid area provided
} else {
  
  # Climatology
  tos <- readRDS("Data/Climatology/sf/tos_interpolated.rds") %>% 
    dplyr::select(-geometry)
  o2os <- readRDS("Data/Climatology/sf/o2os_interpolated.rds") %>% 
    dplyr::select(-geometry)
  phos <- readRDS("Data/Climatology/sf/phos_interpolated.rds") %>% 
    dplyr::select(-geometry)
  chlos <- readRDS("Data/Climatology/sf/chlos_interpolated.rds") %>% 
    dplyr::select(-geometry)
  
  # Bathymetry
  bathy <- readRDS("Data/GEBCO/gebco2500.rds") %>% 
    dplyr::as_tibble() %>% 
    dplyr::select(-geometry)
  
  # Coastline
  
  dist2coast <- readRDS("Data/CoastDistance.rds") %>% 
    dplyr::as_tibble() %>% 
    dplyr::select(-geometry)
}

###############################################################
# Joining all predictor and response per fish species #
###############################################################

# Joining all the data with the species data
# 1. Yellowfin
YFT_full <- dplyr::left_join(tos, o2os, by = "cellID") %>% 
  dplyr::left_join(., phos, by = "cellID") %>% 
  dplyr::left_join(., chlos, by = "cellID") %>% 
  dplyr::left_join(., bathy, by = "cellID") %>% 
  dplyr::left_join(., dist2coast, by = "cellID") %>% 
  dplyr::left_join(grid_YFT, ., by = "cellID") %>%  # Join with species data
  dplyr::select(cellID, species, abundance, season, longitude, latitude, tos_transformed, o2os_transformed, phos_transformed, chlos_transformed, meanDepth, coastDistance, geometry) # arrange columns

write_csv(YFT_full, file = "Output/YFT_full.csv") # save full data

# 2. Albacore
ALB_full <- dplyr::left_join(tos, o2os, by = "cellID") %>% 
  dplyr::left_join(., phos, by = "cellID") %>% 
  dplyr::left_join(., chlos, by = "cellID") %>% 
  dplyr::left_join(., bathy, by = "cellID") %>% 
  dplyr::left_join(., dist2coast, by = "cellID") %>% 
  dplyr::left_join(grid_ALB, ., by = "cellID") %>%  # Join with species data
  dplyr::select(cellID, species, abundance, season, longitude, latitude, tos_transformed, o2os_transformed, phos_transformed, chlos_transformed, meanDepth, coastDistance, geometry) # arrange columns

write_csv(ALB_full, file = "Output/ALB_full.csv") # save full data

# 3. Skipjack tuna
SKP_full <- dplyr::left_join(tos, o2os, by = "cellID") %>% 
  dplyr::left_join(., phos, by = "cellID") %>% 
  dplyr::left_join(., chlos, by = "cellID") %>% 
  dplyr::left_join(., bathy, by = "cellID") %>% 
  dplyr::left_join(., dist2coast, by = "cellID") %>% 
  dplyr::left_join(grid_SKP, ., by = "cellID") %>%  # Join with species data
  dplyr::select(cellID, species, abundance, season, longitude, latitude, tos_transformed, o2os_transformed, phos_transformed, chlos_transformed, meanDepth, coastDistance, geometry) # arrange columns

write_csv(SKP_full, file = "Output/SKP_full.csv") # save full data

# 4. Swordfish
SWO_full <- dplyr::left_join(tos, o2os, by = "cellID") %>% 
  dplyr::left_join(., phos, by = "cellID") %>% 
  dplyr::left_join(., chlos, by = "cellID") %>% 
  dplyr::left_join(., bathy, by = "cellID") %>% 
  dplyr::left_join(., dist2coast, by = "cellID") %>% 
  dplyr::left_join(grid_SWO, ., by = "cellID") %>%  # Join with species data
  dplyr::select(cellID, species, abundance, season, longitude, latitude, tos_transformed, o2os_transformed, phos_transformed, chlos_transformed, meanDepth, coastDistance, geometry) # arrange columns

write_csv(SWO_full, file = "Output/SWO_full.csv") # save full data
}
