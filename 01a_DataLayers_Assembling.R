# Load preliminaries
#source("00_Utils.R")

Dict <- tibble::tribble(
  ~Species, ~Code, ~Trees, ~LR, ~TC, ~BF, ~AUC,
  "yellowfin-tuna", "YFT", 4950, 0.005, 5, 0.5, 0.78393,
  "skipjack-tuna", "SKP", 6200, 0.005, 5, 0.5, 0.79526,
  "albacore", "ALB", 3800, 0.005, 5, 0.5, 0.88791,
  "swordfish", "SWO", 2450, 0.005, 5, 0.5, 0.80528,
  "blue-marlin", "BLUM", 5500, 0.005, 5, 0.5, 0.78891,
  "shortbill-spearfish", "SHOS", 5500, 0.005, 5, 0.5, 0.78866,
  "frigate-tuna", "FRI", 4950, 0.005, 5, 0.5, 0.78486,
  "bigeye-tuna", "BET", 5300, 0.005, 5, 0.5, 0.78726
)

####################
# Fish data #
####################
# 1. Assemble yellowfin tuna data
YFT_sf <- combineFish(species = "yellowfin-tuna") %>% 
  sf::st_transform(crs = moll) %>% 
  sf::st_centroid() # transform into point data

grid_YFT <- assembleGrid(grid, YFT_sf)

# 2. Assemble albacore data
ALB_sf <- combineFish(species = "albacore") %>% 
  sf::st_transform(crs = moll) %>% 
  sf::st_centroid() # transform into point data

grid_ALB <- assembleGrid(grid, ALB_sf)

# 3. Assemble skipjack tuna
SKP_sf <- combineFish(species = "skipjack-tuna") %>% 
  sf::st_transform(crs = moll) %>% 
  sf::st_centroid()# transform into point data

grid_SKP <- assembleGrid(grid, SKP_sf)

# 4. Assemble swordfish
SWO_sf <- combineFish(species = "swordfish") %>% 
  sf::st_transform(crs = moll) %>% 
  sf::st_centroid()# transform into point data

grid_SWO <- assembleGrid(grid, SWO_sf)

# 5. Assemble blue marlin
BLUM_sf <- combineFish(species = "blue-marlin") %>% 
  sf::st_transform(crs = moll) %>% 
  sf::st_centroid()# transform into point data

grid_BLUM <- assembleGrid(grid, BLUM_sf)

# 6. Assemble shortbill spearfish
SHOS_sf <- combineFish(species = "shortbill-spearfish") %>% 
  sf::st_transform(crs = moll) %>% 
  sf::st_centroid()# transform into point data

grid_SHOS <- assembleGrid(grid, SHOS_sf)

# 7. Assemble frigate tuna
FRI_sf <- combineFish(species = "frigate-tuna") %>% 
  sf::st_transform(crs = moll) %>% 
  sf::st_centroid()# transform into point data

grid_FRI <- assembleGrid(grid, FRI_sf)

# 8. Assemble bigeye tuna
BET_sf <- combineFish(species = "bigeye-tuna") %>% 
  sf::st_transform(crs = moll) %>% 
  sf::st_centroid()# transform into point data

grid_BET <- assembleGrid(grid, BET_sf)

load = TRUE # if TRUE, scripts above are only reloaded; none are reran
if(isFALSE(load)) {

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
YFT_full <- joinPredictors(grid = grid_YFT, 
                           tos = tos, 
                           o2os = o2os, 
                           phos = phos, 
                           chlos = chlos, 
                           bathy = bathy,
                           dist2coast = dist2coast
)
write_csv(YFT_full, file = "Output/YFT_full.csv") # save full data

# 2. Albacore
ALB_full <- joinPredictors(grid = grid_ALB, 
                           tos = tos, 
                           o2os = o2os, 
                           phos = phos, 
                           chlos = chlos, 
                           bathy = bathy,
                           dist2coast = dist2coast
)

write_csv(ALB_full, file = "Output/ALB_full.csv") # save full data

# 3. Skipjack tuna
SKP_full <- joinPredictors(grid = grid_SKP, 
                           tos = tos, 
                           o2os = o2os, 
                           phos = phos, 
                           chlos = chlos, 
                           bathy = bathy,
                           dist2coast = dist2coast
)

write_csv(SKP_full, file = "Output/SKP_full.csv") # save full data

# 4. Swordfish
SWO_full <- joinPredictors(grid = grid_SWO, 
                           tos = tos, 
                           o2os = o2os, 
                           phos = phos, 
                           chlos = chlos, 
                           bathy = bathy,
                           dist2coast = dist2coast
)

write_csv(SWO_full, file = "Output/SWO_full.csv") # save full data

# 5. Blue marlin
BLUM_full <- joinPredictors(grid = grid_BLUM, 
                            tos = tos, 
                            o2os = o2os, 
                            phos = phos, 
                            chlos = chlos, 
                            bathy = bathy,
                            dist2coast = dist2coast
                            )
write_csv(BLUM_full, file = "Output/BLUM_full.csv") # save full data

# 6. Shortbill spearfish
SHOS_full <- joinPredictors(grid = grid_SHOS, 
                            tos = tos, 
                            o2os = o2os, 
                            phos = phos, 
                            chlos = chlos, 
                            bathy = bathy,
                            dist2coast = dist2coast
)
write_csv(SHOS_full, file = "Output/SHOS_full.csv") # save full data

# 7. Frigate tuna
FRI_full <- joinPredictors(grid = grid_FRI, 
                            tos = tos, 
                            o2os = o2os, 
                            phos = phos, 
                            chlos = chlos, 
                            bathy = bathy,
                            dist2coast = dist2coast
)
write_csv(FRI_full, file = "Output/FRI_full.csv") # save full data

# 8. Bigeye tuna
BET_full <- joinPredictors(grid = grid_BET, 
                            tos = tos, 
                            o2os = o2os, 
                            phos = phos, 
                            chlos = chlos, 
                            bathy = bathy,
                            dist2coast = dist2coast
)
write_csv(BET_full, file = "Output/BET_full.csv") # save full data

} else {
  
}