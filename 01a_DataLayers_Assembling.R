# Load preliminaries
#source("00_Utils.R")

Dict <- tibble::tribble(
  ~Species, ~Code, ~Trees, ~LR, ~TC, ~BF, ~AUC,
  "yellowfin-tuna", "YFT", 4950, 0.005, 5, 0.5, 0.78393,
  "skipjack-tuna", "SKP", 6200, 0.005, 5, 0.5, 0.79526,
  "albacore", "ALB", 3800, 0.005, 5, 0.5, 0.88791,
  "swordfish", "SWO", 2450, 0.005, 5, 0.5, 0.80528,
  "blue-marlin", "BLUM", 6800, 0.005, 5, 0.5, 0.84797,
  "shortbill-spearfish", "SHOS", 3150, 0.005, 5, 0.5, 0.8464,
  "frigate-tuna", "FRI", 4150, 0.005, 5, 0.75, 0.8889,
  "bigeye-tuna", "BET", 4250, 0.005, 5, 0.5, 0.82867,
  "striped-marlin", "STRM", 2850, 0.005, 5, 0.5, 0.85267, 
  "sauries", "SAU", 2850, 0.005, 5, 0.75, 0.95151, 
  "sailfish", "SAIL", 2700, 0.005, 5, 0.5, 0.87756, 
  "longfin-escolar", "LESC", 1250, 0.005, 5, 0.5, 0.82454,
  "bluefin-tuna", "BFT", 1450, 0.005, 5, 0.5, 0.96989,
  "little-tuna", "LIT", 1900, 0.005, 5, 0.75, 0.90922,
  "southern-bluefin-tuna", "SBFT", 1500, 0.005, 5, 0.75, 0.98466,
  "slender-tuna", "SLT", 1300, 0.005, 5, 0.75, 0.98202,
  "bonitos", "BON", 200, 0.005, 3, 0.5, 0.8255 # 300 TREES; POSSIBLY OVERFITTED
  # CAN'T BUILD MODELS FOR BLACK MARLIN
  
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

# 9. Assemble striped marlin & white marlin
STRM_sf <- combineFish(species = "striped-marlin") %>% 
  sf::st_transform(crs = moll) %>% 
  sf::st_centroid()# transform into point data

grid_STRM <- assembleGrid(grid, STRM_sf)

# 10. Assemble sauries
SAU_sf <- combineFish(species = "sauries") %>% 
  sf::st_transform(crs = moll) %>% 
  sf::st_centroid()# transform into point data

grid_SAU <- assembleGrid(grid, SAU_sf)

# 11. Assemble sailfish
SAIL_sf <- combineFish(species = "sailfish") %>% 
  sf::st_transform(crs = moll) %>% 
  sf::st_centroid()# transform into point data

grid_SAIL <- assembleGrid(grid, SAIL_sf)

# 12. Assemble longfin escolar
LESC_sf <- combineFish(species = "longfin-escolar") %>% 
  sf::st_transform(crs = moll) %>% 
  sf::st_centroid()# transform into point data

grid_LESC <- assembleGrid(grid, LESC_sf)

# 13. Assemble bluefin tuna
BFT_sf <- combineFish(species = "bluefin-tuna") %>% 
  sf::st_transform(crs = moll) %>% 
  sf::st_centroid()# transform into point data

grid_BFT <- assembleGrid(grid, BFT_sf)

# 14. Assemble little tuna
LIT_sf <- combineFish(species = "little-tuna") %>% 
  sf::st_transform(crs = moll) %>% 
  sf::st_centroid()# transform into point data

grid_LIT <- assembleGrid(grid, LIT_sf)

# 15. Assemble southern bluefin tuna
SBFT_sf <- combineFish(species = "southern-bluefin-tuna") %>% 
  sf::st_transform(crs = moll) %>% 
  sf::st_centroid()# transform into point data

grid_SBFT <- assembleGrid(grid, SBFT_sf)

# 16. Assemble slender tuna
SLT_sf <- combineFish(species = "slender-tuna") %>% 
  sf::st_transform(crs = moll) %>% 
  sf::st_centroid()# transform into point data

grid_SLT <- assembleGrid(grid, SLT_sf)

# 17. Assemble bonitos
BON_sf <- combineFish(species = "bonitos") %>% 
  sf::st_transform(crs = moll) %>% 
  sf::st_centroid()# transform into point data

grid_BON <- assembleGrid(grid, BON_sf)

# 18. Assemble black marlin
BLAM_sf <- combineFish(species = "black-marlin") %>% 
  sf::st_transform(crs = moll) %>% 
  sf::st_centroid()# transform into point data

grid_BLAM <- assembleGrid(grid, BLAM_sf)

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
  
  # A. Historical (to fit the model)
  tos_rs <- stars::read_ncdf("Data/Climatology/ensemble/tos_historical_1956_1984_ensemble.nc") %>% 
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
  saveRDS(tos, "Data/Climatology/sf/tos_historical_interpolated.rds")
  
  # B. Present (2017-2026)
  tos_rs <- stars::read_ncdf("Data/Climatology/ensemble/tos_ssp585_2017_2026_ensemble.nc") %>% 
    terra::rast()
  names(tos_rs) <- paste0("X", seq(2017, 2026, by = 1))
  tos <- rs2sf(tos_rs) %>% 
    dplyr::rename(tos = mean) %>% # using the mean of the models
    sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
    dplyr::as_tibble() %>% 
    dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
    sf::st_as_sf(crs = moll) %>% 
    replaceNN(., grid, "tos") %>%
    dplyr::as_tibble() %>% 
    dplyr::select(cellID, tos_transformed, geometry)
  saveRDS(tos, "Data/Climatology/sf/tos_present_interpolated.rds")
  
  # C. Mid-century (2046-2055)
  tos_rs <- stars::read_ncdf("Data/Climatology/ensemble/tos_ssp585_2046_2055_ensemble.nc") %>% 
    terra::rast()
  names(tos_rs) <- paste0("X", seq(2046, 2055, by = 1))
  tos <- rs2sf(tos_rs) %>% 
    dplyr::rename(tos = mean) %>% # using the mean of the models
    sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
    dplyr::as_tibble() %>% 
    dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
    sf::st_as_sf(crs = moll) %>% 
    replaceNN(., grid, "tos") %>%
    dplyr::as_tibble() %>% 
    dplyr::select(cellID, tos_transformed, geometry)
  saveRDS(tos, "Data/Climatology/sf/tos_midCentury_interpolated.rds")
  
  # D. End of the century (2091-2100)
  tos_rs <- stars::read_ncdf("Data/Climatology/ensemble/tos_ssp585_2091_2100_ensemble.nc") %>% 
    terra::rast()
  names(tos_rs) <- paste0("X", seq(2091, 2100, by = 1))
  tos <- rs2sf(tos_rs) %>% 
    dplyr::rename(tos = mean) %>% # using the mean of the models
    sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
    dplyr::as_tibble() %>% 
    dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
    sf::st_as_sf(crs = moll) %>% 
    replaceNN(., grid, "tos") %>%
    dplyr::as_tibble() %>% 
    dplyr::select(cellID, tos_transformed, geometry)
  saveRDS(tos, "Data/Climatology/sf/tos_endCentury_interpolated.rds")
  
  # 2. Oxygen
  
  # A. Historical (to fit the model)
  o2os_rs <- stars::read_ncdf("Data/Climatology/ensemble/o2os_historical_1956_1984_ensemble.nc") %>% 
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
  saveRDS(o2os, "Data/Climatology/sf/o2os_historical_interpolated.rds")
  
  # B. Present (2017-2026)
  o2os_rs <- stars::read_ncdf("Data/Climatology/ensemble/o2os_ssp585_2017_2026_ensemble.nc") %>% 
    terra::rast()
  names(o2os_rs) <- paste0("X", seq(2017, 2026, by = 1))
  o2os <- rs2sf(o2os_rs) %>% 
    dplyr::rename(o2os = mean) %>% # using the mean of the models
    sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
    dplyr::as_tibble() %>% 
    dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
    sf::st_as_sf(crs = moll) %>% 
    replaceNN(., grid, "o2os") %>%
    dplyr::as_tibble() %>% 
    dplyr::select(cellID, o2os_transformed, geometry)
  saveRDS(o2os, "Data/Climatology/sf/o2os_present_interpolated.rds")  
  
  # C. Mid-century (2046-2055)
  o2os_rs <- stars::read_ncdf("Data/Climatology/ensemble/o2os_ssp585_2046_2055_ensemble.nc") %>% 
    terra::rast()
  names(o2os_rs) <- paste0("X", seq(2046, 2055, by = 1))
  o2os <- rs2sf(o2os_rs) %>% 
    dplyr::rename(o2os = mean) %>% # using the mean of the models
    sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
    dplyr::as_tibble() %>% 
    dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
    sf::st_as_sf(crs = moll) %>% 
    replaceNN(., grid, "o2os") %>%
    dplyr::as_tibble() %>% 
    dplyr::select(cellID, o2os_transformed, geometry)
  saveRDS(o2os, "Data/Climatology/sf/o2os_midCentury_interpolated.rds")
  
  # D. End of the century (2091-2100)
  o2os_rs <- stars::read_ncdf("Data/Climatology/ensemble/o2os_ssp585_2091_2100_ensemble.nc") %>% 
    terra::rast()
  names(o2os_rs) <- paste0("X", seq(2091, 2100, by = 1))
  o2os <- rs2sf(o2os_rs) %>% 
    dplyr::rename(o2os = mean) %>% # using the mean of the models
    sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
    dplyr::as_tibble() %>% 
    dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
    sf::st_as_sf(crs = moll) %>% 
    replaceNN(., grid, "o2os") %>%
    dplyr::as_tibble() %>% 
    dplyr::select(cellID, o2os_transformed, geometry)
  saveRDS(o2os, "Data/Climatology/sf/o2os_endCentury_interpolated.rds")
  
  # 3. pH
  
  # A. Historical (to fit the models)
  phos_rs <- stars::read_ncdf("Data/Climatology/ensemble/phos_historical_1956_1984_ensemble.nc") %>% 
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
  saveRDS(phos, "Data/Climatology/sf/phos_historical_interpolated.rds")
  
  # B. Present (2017-2026)
  phos_rs <- stars::read_ncdf("Data/Climatology/ensemble/phos_ssp585_2017_2026_ensemble.nc") %>% 
    terra::rast()
  names(phos_rs) <- paste0("X", seq(2017, 2026, by = 1))
  phos <- rs2sf(phos_rs) %>% 
    dplyr::rename(phos = mean) %>% # using the mean of the models
    sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
    dplyr::as_tibble() %>% 
    dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
    sf::st_as_sf(crs = moll) %>% 
    replaceNN(., grid, "phos") %>%
    dplyr::as_tibble() %>% 
    dplyr::select(cellID, phos_transformed, geometry)
  saveRDS(phos, "Data/Climatology/sf/phos_present_interpolated.rds")
  
  # C. Mid-century (2046-2055)
  phos_rs <- stars::read_ncdf("Data/Climatology/ensemble/phos_ssp585_2046_2055_ensemble.nc") %>% 
    terra::rast()
  names(phos_rs) <- paste0("X", seq(2046, 2055, by = 1))
  phos <- rs2sf(phos_rs) %>% 
    dplyr::rename(phos = mean) %>% # using the mean of the models
    sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
    dplyr::as_tibble() %>% 
    dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
    sf::st_as_sf(crs = moll) %>% 
    replaceNN(., grid, "phos") %>%
    dplyr::as_tibble() %>% 
    dplyr::select(cellID, phos_transformed, geometry)
  saveRDS(phos, "Data/Climatology/sf/phos_midCentury_interpolated.rds")
  
  # D. End of the century (2091-2100)
  phos_rs <- stars::read_ncdf("Data/Climatology/ensemble/phos_ssp585_2091_2100_ensemble.nc") %>% 
    terra::rast()
  names(phos_rs) <- paste0("X", seq(2091, 2100, by = 1))
  phos <- rs2sf(phos_rs) %>% 
    dplyr::rename(phos = mean) %>% # using the mean of the models
    sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
    dplyr::as_tibble() %>% 
    dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
    sf::st_as_sf(crs = moll) %>% 
    replaceNN(., grid, "phos") %>%
    dplyr::as_tibble() %>% 
    dplyr::select(cellID, phos_transformed, geometry)
  saveRDS(phos, "Data/Climatology/sf/phos_endCentury_interpolated.rds")
  
  # 4. chlorophyll-a
  
  # A. Historical (to fit the model)
  chlos_rs <- stars::read_ncdf("Data/Climatology/ensemble/chlos_historical_1956_1984_ensemble.nc") %>% 
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
  saveRDS(chlos, "Data/Climatology/sf/chlos_historical_interpolated.rds")
  
  # B. Present (2017-2026)
  chlos_rs <- stars::read_ncdf("Data/Climatology/ensemble/chlos_ssp585_2017_2026_ensemble.nc") %>% 
    terra::rast()
  names(chlos_rs) <- paste0("X", seq(2017, 2026, by = 1))
  chlos <- rs2sf(chlos_rs) %>% 
    dplyr::rename(chlos = mean) %>% # using the mean of the models
    sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
    dplyr::as_tibble() %>% 
    dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
    sf::st_as_sf(crs = moll) %>% 
    replaceNN(., grid, "chlos") %>%
    dplyr::as_tibble() %>% 
    dplyr::select(cellID, chlos_transformed, geometry)
  saveRDS(chlos, "Data/Climatology/sf/chlos_present_interpolated.rds")
  
  # C. Mid-century (2046-2055)
  chlos_rs <- stars::read_ncdf("Data/Climatology/ensemble/chlos_ssp585_2046_2055_ensemble.nc") %>% 
    terra::rast()
  names(chlos_rs) <- paste0("X", seq(2046, 2055, by = 1))
  chlos <- rs2sf(chlos_rs) %>% 
    dplyr::rename(chlos = mean) %>% # using the mean of the models
    sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
    dplyr::as_tibble() %>% 
    dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
    sf::st_as_sf(crs = moll) %>% 
    replaceNN(., grid, "chlos") %>%
    dplyr::as_tibble() %>% 
    dplyr::select(cellID, chlos_transformed, geometry)
  saveRDS(chlos, "Data/Climatology/sf/chlos_midCentury_interpolated.rds")
  
  # D. End of the century (2091-2100)
  chlos_rs <- stars::read_ncdf("Data/Climatology/ensemble/chlos_ssp585_2091_2100_ensemble.nc") %>% 
    terra::rast()
  names(chlos_rs) <- paste0("X", seq(2091, 2100, by = 1))
  chlos <- rs2sf(chlos_rs) %>% 
    dplyr::rename(chlos = mean) %>% # using the mean of the models
    sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
    dplyr::as_tibble() %>% 
    dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
    sf::st_as_sf(crs = moll) %>% 
    replaceNN(., grid, "chlos") %>%
    dplyr::as_tibble() %>% 
    dplyr::select(cellID, chlos_transformed, geometry)
  saveRDS(chlos, "Data/Climatology/sf/chlos_endCentury_interpolated.rds")
  
  # Bathymetry
  bathy <- gebcoConvert(grid, 2500) # bathymetry data is extrapolated depending on the grid area provided
  
  # Coastline
  dist2coast <- calculateDist2Coast(grid) # distance to coast is calculated depending on the grid area provided
} else {
  
  # Climatology
  # Temperature
  tos_historical <- readRDS("Data/Climatology/sf/tos_historical_interpolated.rds") %>% 
    dplyr::select(-geometry)
  tos_present <- readRDS("Data/Climatology/sf/tos_present_interpolated.rds") %>% 
    dplyr::select(-geometry)
  tos_midCentury <- readRDS("Data/Climatology/sf/tos_midCentury_interpolated.rds") %>% 
    dplyr::select(-geometry)
  tos_endCentury <- readRDS("Data/Climatology/sf/tos_endCentury_interpolated.rds") %>% 
    dplyr::select(-geometry)
  
  # Oxygen
  o2os_historical <- readRDS("Data/Climatology/sf/o2os_historical_interpolated.rds") %>% 
    dplyr::select(-geometry)
  o2os_present <- readRDS("Data/Climatology/sf/o2os_present_interpolated.rds") %>% 
    dplyr::select(-geometry)
  o2os_midCentury <- readRDS("Data/Climatology/sf/o2os_midCentury_interpolated.rds") %>% 
    dplyr::select(-geometry)
  o2os_endCentury <- readRDS("Data/Climatology/sf/o2os_endCentury_interpolated.rds") %>% 
    dplyr::select(-geometry)
  
  # pH
  phos_historical <- readRDS("Data/Climatology/sf/phos_historical_interpolated.rds") %>% 
    dplyr::select(-geometry)
  phos_present <- readRDS("Data/Climatology/sf/phos_present_interpolated.rds") %>% 
    dplyr::select(-geometry)
  phos_midCentury <- readRDS("Data/Climatology/sf/phos_midCentury_interpolated.rds") %>% 
    dplyr::select(-geometry)
  phos_endCentury <- readRDS("Data/Climatology/sf/phos_endCentury_interpolated.rds") %>% 
    dplyr::select(-geometry)
  
  # chlorophyll-a
  chlos_historical <- readRDS("Data/Climatology/sf/chlos_historical_interpolated.rds") %>% 
    dplyr::select(-geometry)
  chlos_present <- readRDS("Data/Climatology/sf/chlos_present_interpolated.rds") %>% 
    dplyr::select(-geometry)
  chlos_midCentury <- readRDS("Data/Climatology/sf/chlos_midCentury_interpolated.rds") %>% 
    dplyr::select(-geometry)
  chlos_endCentury <- readRDS("Data/Climatology/sf/chlos_endCentury_interpolated.rds") %>% 
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
                           tos = tos_historical, 
                           o2os = o2os_historical, 
                           phos = phos_historical, 
                           chlos = chlos_historical, 
                           bathy = bathy,
                           dist2coast = dist2coast
)
write_csv(YFT_full, file = "Output/CSV/YFT_historical_full.csv") # save full data

YFT_full <- joinPredictors(grid = grid_YFT, 
                           tos = tos_present, 
                           o2os = o2os_present, 
                           phos = phos_present, 
                           chlos = chlos_present, 
                           bathy = bathy,
                           dist2coast = dist2coast
)
write_csv(YFT_full, file = "Output/CSV/YFT_present_full.csv") # save full data

YFT_full <- joinPredictors(grid = grid_YFT, 
                           tos = tos_midCentury, 
                           o2os = o2os_midCentury, 
                           phos = phos_midCentury, 
                           chlos = chlos_midCentury, 
                           bathy = bathy,
                           dist2coast = dist2coast
)
write_csv(YFT_full, file = "Output/CSV/YFT_midCentury_full.csv") # save full data

YFT_full <- joinPredictors(grid = grid_YFT, 
                           tos = tos_endCentury, 
                           o2os = o2os_endCentury, 
                           phos = phos_endCentury, 
                           chlos = chlos_endCentury, 
                           bathy = bathy,
                           dist2coast = dist2coast
)
write_csv(YFT_full, file = "Output/CSV/YFT_endCentury_full.csv") # save full data

# 2. Albacore
ALB_full <- joinPredictors(grid = grid_ALB, 
                           tos = tos_historical, 
                           o2os = o2os_historical, 
                           phos = phos_historical, 
                           chlos = chlos_historical, 
                           bathy = bathy,
                           dist2coast = dist2coast
)

write_csv(ALB_full, file = "Output/CSV/ALB_historical_full.csv") # save full data

ALB_full <- joinPredictors(grid = grid_ALB, 
                           tos = tos_present, 
                           o2os = o2os_present, 
                           phos = phos_present, 
                           chlos = chlos_present, 
                           bathy = bathy,
                           dist2coast = dist2coast
)

write_csv(ALB_full, file = "Output/CSV/ALB_present_full.csv") # save full data

ALB_full <- joinPredictors(grid = grid_ALB, 
                           tos = tos_midCentury, 
                           o2os = o2os_midCentury, 
                           phos = phos_midCentury, 
                           chlos = chlos_midCentury, 
                           bathy = bathy,
                           dist2coast = dist2coast
)

write_csv(ALB_full, file = "Output/CSV/ALB_midCentury_full.csv") # save full data

ALB_full <- joinPredictors(grid = grid_ALB, 
                           tos = tos_endCentury, 
                           o2os = o2os_endCentury, 
                           phos = phos_endCentury, 
                           chlos = chlos_endCentury, 
                           bathy = bathy,
                           dist2coast = dist2coast
)

write_csv(ALB_full, file = "Output/CSV/ALB_endCentury_full.csv") # save full data

# 3. Skipjack tuna
SKP_full <- joinPredictors(grid = grid_SKP, 
                           tos = tos_historical, 
                           o2os = o2os_historical, 
                           phos = phos_historical, 
                           chlos = chlos_historical, 
                           bathy = bathy,
                           dist2coast = dist2coast
)

write_csv(SKP_full, file = "Output/CSV/SKP_historical_full.csv") # save full data

SKP_full <- joinPredictors(grid = grid_SKP, 
                           tos = tos_present, 
                           o2os = o2os_present, 
                           phos = phos_present, 
                           chlos = chlos_present, 
                           bathy = bathy,
                           dist2coast = dist2coast
)

write_csv(SKP_full, file = "Output/CSV/SKP_present_full.csv") # save full data

SKP_full <- joinPredictors(grid = grid_SKP, 
                           tos = tos_midCentury, 
                           o2os = o2os_midCentury, 
                           phos = phos_midCentury, 
                           chlos = chlos_midCentury, 
                           bathy = bathy,
                           dist2coast = dist2coast
)

write_csv(SKP_full, file = "Output/CSV/SKP_midCentury_full.csv") # save full data

SKP_full <- joinPredictors(grid = grid_SKP, 
                           tos = tos_endCentury, 
                           o2os = o2os_endCentury, 
                           phos = phos_endCentury, 
                           chlos = chlos_endCentury, 
                           bathy = bathy,
                           dist2coast = dist2coast
)

write_csv(SKP_full, file = "Output/CSV/SKP_endCentury_full.csv") # save full data

# 4. Swordfish
SWO_full <- joinPredictors(grid = grid_SWO, 
                           tos = tos_historical, 
                           o2os = o2os_historical, 
                           phos = phos_historical, 
                           chlos = chlos_historical, 
                           bathy = bathy,
                           dist2coast = dist2coast
)

write_csv(SWO_full, file = "Output/CSV/SWO_historical_full.csv") # save full data

SWO_full <- joinPredictors(grid = grid_SWO, 
                           tos = tos_present, 
                           o2os = o2os_present, 
                           phos = phos_present, 
                           chlos = chlos_present, 
                           bathy = bathy,
                           dist2coast = dist2coast
)

write_csv(SWO_full, file = "Output/CSV/SWO_present_full.csv") # save full data

SWO_full <- joinPredictors(grid = grid_SWO, 
                           tos = tos_midCentury, 
                           o2os = o2os_midCentury, 
                           phos = phos_midCentury, 
                           chlos = chlos_midCentury, 
                           bathy = bathy,
                           dist2coast = dist2coast
)

write_csv(SWO_full, file = "Output/CSV/SWO_midCentury_full.csv") # save full data

SWO_full <- joinPredictors(grid = grid_SWO, 
                           tos = tos_endCentury, 
                           o2os = o2os_endCentury, 
                           phos = phos_endCentury, 
                           chlos = chlos_endCentury, 
                           bathy = bathy,
                           dist2coast = dist2coast
)

write_csv(SWO_full, file = "Output/CSV/SWO_endCentury_full.csv") # save full data

# 5. Blue marlin
BLUM_full <- joinPredictors(grid = grid_BLUM, 
                            tos = tos_historical, 
                            o2os = o2os_historical, 
                            phos = phos_historical, 
                            chlos = chlos_historical, 
                            bathy = bathy,
                            dist2coast = dist2coast
                            )
write_csv(BLUM_full, file = "Output/CSV/BLUM_historical_full.csv") # save full data

BLUM_full <- joinPredictors(grid = grid_BLUM, 
                            tos = tos_present, 
                            o2os = o2os_present, 
                            phos = phos_present, 
                            chlos = chlos_present, 
                            bathy = bathy,
                            dist2coast = dist2coast
)
write_csv(BLUM_full, file = "Output/CSV/BLUM_present_full.csv") # save full data

BLUM_full <- joinPredictors(grid = grid_BLUM, 
                            tos = tos_midCentury, 
                            o2os = o2os_midCentury, 
                            phos = phos_midCentury, 
                            chlos = chlos_midCentury, 
                            bathy = bathy,
                            dist2coast = dist2coast
)
write_csv(BLUM_full, file = "Output/CSV/BLUM_midCentury_full.csv") # save full data

BLUM_full <- joinPredictors(grid = grid_BLUM, 
                            tos = tos_endCentury, 
                            o2os = o2os_endCentury, 
                            phos = phos_endCentury, 
                            chlos = chlos_endCentury, 
                            bathy = bathy,
                            dist2coast = dist2coast
)
write_csv(BLUM_full, file = "Output/CSV/BLUM_endCentury_full.csv") # save full data

# 6. Shortbill spearfish
SHOS_full <- joinPredictors(grid = grid_SHOS, 
                            tos = tos_historical, 
                            o2os = o2os_historical, 
                            phos = phos_historical, 
                            chlos = chlos_historical, 
                            bathy = bathy,
                            dist2coast = dist2coast
)
write_csv(SHOS_full, file = "Output/CSV/SHOS_historical_full.csv") # save full data

SHOS_full <- joinPredictors(grid = grid_SHOS, 
                            tos = tos_present, 
                            o2os = o2os_present, 
                            phos = phos_present, 
                            chlos = chlos_present, 
                            bathy = bathy,
                            dist2coast = dist2coast
)
write_csv(SHOS_full, file = "Output/CSV/SHOS_present_full.csv") # save full data

SHOS_full <- joinPredictors(grid = grid_SHOS, 
                            tos = tos_midCentury, 
                            o2os = o2os_midCentury, 
                            phos = phos_midCentury, 
                            chlos = chlos_midCentury, 
                            bathy = bathy,
                            dist2coast = dist2coast
)
write_csv(SHOS_full, file = "Output/CSV/SHOS_midCentury_full.csv") # save full data

SHOS_full <- joinPredictors(grid = grid_SHOS, 
                            tos = tos_endCentury, 
                            o2os = o2os_endCentury, 
                            phos = phos_endCentury, 
                            chlos = chlos_endCentury, 
                            bathy = bathy,
                            dist2coast = dist2coast
)
write_csv(SHOS_full, file = "Output/CSV/SHOS_endCentury_full.csv") # save full data

# 7. Frigate tuna
FRI_full <- joinPredictors(grid = grid_FRI, 
                            tos = tos_historical, 
                            o2os = o2os_historical, 
                            phos = phos_historical, 
                            chlos = chlos_historical, 
                            bathy = bathy,
                            dist2coast = dist2coast
)
write_csv(FRI_full, file = "Output/CSV/FRI_historical_full.csv") # save full data

FRI_full <- joinPredictors(grid = grid_FRI, 
                           tos = tos_present, 
                           o2os = o2os_present, 
                           phos = phos_present, 
                           chlos = chlos_present, 
                           bathy = bathy,
                           dist2coast = dist2coast
)
write_csv(FRI_full, file = "Output/CSV/FRI_present_full.csv") # save full data

FRI_full <- joinPredictors(grid = grid_FRI, 
                           tos = tos_midCentury, 
                           o2os = o2os_midCentury, 
                           phos = phos_midCentury, 
                           chlos = chlos_midCentury, 
                           bathy = bathy,
                           dist2coast = dist2coast
)
write_csv(FRI_full, file = "Output/CSV/FRI_midCentury_full.csv") # save full data

FRI_full <- joinPredictors(grid = grid_FRI, 
                           tos = tos_endCentury, 
                           o2os = o2os_endCentury, 
                           phos = phos_endCentury, 
                           chlos = chlos_endCentury, 
                           bathy = bathy,
                           dist2coast = dist2coast
)
write_csv(FRI_full, file = "Output/CSV/FRI_endCentury_full.csv") # save full data

# 8. Bigeye tuna
BET_full <- joinPredictors(grid = grid_BET, 
                            tos = tos_historical, 
                            o2os = o2os_historical, 
                            phos = phos_historical, 
                            chlos = chlos_historical, 
                            bathy = bathy,
                            dist2coast = dist2coast
)
write_csv(BET_full, file = "Output/CSV/BET_historical_full.csv") # save full data

BET_full <- joinPredictors(grid = grid_BET, 
                           tos = tos_present, 
                           o2os = o2os_present, 
                           phos = phos_present, 
                           chlos = chlos_present, 
                           bathy = bathy,
                           dist2coast = dist2coast
)
write_csv(BET_full, file = "Output/CSV/BET_present_full.csv") # save full 

BET_full <- joinPredictors(grid = grid_BET, 
                           tos = tos_midCentury, 
                           o2os = o2os_midCentury, 
                           phos = phos_midCentury, 
                           chlos = chlos_midCentury, 
                           bathy = bathy,
                           dist2coast = dist2coast
)
write_csv(BET_full, file = "Output/CSV/BET_midCentury_full.csv") # save full data

BET_full <- joinPredictors(grid = grid_BET, 
                           tos = tos_endCentury, 
                           o2os = o2os_endCentury, 
                           phos = phos_endCentury, 
                           chlos = chlos_endCentury, 
                           bathy = bathy,
                           dist2coast = dist2coast
)
write_csv(BET_full, file = "Output/CSV/BET_endCentury_full.csv") # save full data

# 9. Striped marlin and white marlin
STRM_full <- joinPredictors(grid = grid_STRM, 
                           tos = tos_historical, 
                           o2os = o2os_historical, 
                           phos = phos_historical, 
                           chlos = chlos_historical, 
                           bathy = bathy,
                           dist2coast = dist2coast
)
write_csv(STRM_full, file = "Output/CSV/STRM_historical_full.csv") # save full data

STRM_full <- joinPredictors(grid = grid_STRM, 
                            tos = tos_present, 
                            o2os = o2os_present, 
                            phos = phos_present, 
                            chlos = chlos_present, 
                            bathy = bathy,
                            dist2coast = dist2coast
)
write_csv(STRM_full, file = "Output/CSV/STRM_present_full.csv") # save full data

STRM_full <- joinPredictors(grid = grid_STRM, 
                            tos = tos_midCentury, 
                            o2os = o2os_midCentury, 
                            phos = phos_midCentury, 
                            chlos = chlos_midCentury, 
                            bathy = bathy,
                            dist2coast = dist2coast
)
write_csv(STRM_full, file = "Output/CSV/STRM_midCentury_full.csv") # save full data

STRM_full <- joinPredictors(grid = grid_STRM, 
                            tos = tos_endCentury, 
                            o2os = o2os_endCentury, 
                            phos = phos_endCentury, 
                            chlos = chlos_endCentury, 
                            bathy = bathy,
                            dist2coast = dist2coast
)
write_csv(STRM_full, file = "Output/CSV/STRM_endCentury_full.csv") # save full data

# 10. Sauries
SAU_full <- joinPredictors(grid = grid_SAU, 
                             tos = tos_historical, 
                             o2os = o2os_historical, 
                             phos = phos_historical, 
                             chlos = chlos_historical, 
                             bathy = bathy,
                             dist2coast = dist2coast
)
write_csv(SAU_full, file = "Output/CSV/SAU_historical_full.csv") # save full data

SAU_full <- joinPredictors(grid = grid_SAU, 
                           tos = tos_present, 
                           o2os = o2os_present, 
                           phos = phos_present, 
                           chlos = chlos_present, 
                           bathy = bathy,
                           dist2coast = dist2coast
)
write_csv(SAU_full, file = "Output/CSV/SAU_present_full.csv") # save full data

SAU_full <- joinPredictors(grid = grid_SAU, 
                           tos = tos_midCentury, 
                           o2os = o2os_midCentury, 
                           phos = phos_midCentury, 
                           chlos = chlos_midCentury, 
                           bathy = bathy,
                           dist2coast = dist2coast
)
write_csv(SAU_full, file = "Output/CSV/SAU_midCentury_full.csv") # save full data

SAU_full <- joinPredictors(grid = grid_SAU, 
                           tos = tos_endCentury, 
                           o2os = o2os_endCentury, 
                           phos = phos_endCentury, 
                           chlos = chlos_endCentury, 
                           bathy = bathy,
                           dist2coast = dist2coast
)
write_csv(SAU_full, file = "Output/CSV/SAU_endCentury_full.csv") # save full data

# 11. Sailfish
SAIL_full <- joinPredictors(grid = grid_SAIL, 
                           tos = tos_historical, 
                           o2os = o2os_historical, 
                           phos = phos_historical, 
                           chlos = chlos_historical, 
                           bathy = bathy,
                           dist2coast = dist2coast
)
write_csv(SAIL_full, file = "Output/CSV/SAIL_historical_full.csv") # save full data

SAIL_full <- joinPredictors(grid = grid_SAIL, 
                            tos = tos_present, 
                            o2os = o2os_present, 
                            phos = phos_present, 
                            chlos = chlos_present, 
                            bathy = bathy,
                            dist2coast = dist2coast
)
write_csv(SAIL_full, file = "Output/CSV/SAIL_present_full.csv") # save full data

SAIL_full <- joinPredictors(grid = grid_SAIL, 
                            tos = tos_midCentury, 
                            o2os = o2os_midCentury, 
                            phos = phos_midCentury, 
                            chlos = chlos_midCentury, 
                            bathy = bathy,
                            dist2coast = dist2coast
)
write_csv(SAIL_full, file = "Output/CSV/SAIL_midCentury_full.csv") # save full data

SAIL_full <- joinPredictors(grid = grid_SAIL, 
                            tos = tos_endCentury, 
                            o2os = o2os_endCentury, 
                            phos = phos_endCentury, 
                            chlos = chlos_endCentury, 
                            bathy = bathy,
                            dist2coast = dist2coast
)
write_csv(SAIL_full, file = "Output/CSV/SAIL_endCentury_full.csv") # save full data

# 12. Longfin escolar
LESC_full <- joinPredictors(grid = grid_LESC, 
                            tos = tos_historical, 
                            o2os = o2os_historical, 
                            phos = phos_historical, 
                            chlos = chlos_historical, 
                            bathy = bathy,
                            dist2coast = dist2coast
)
write_csv(LESC_full, file = "Output/CSV/LESC_historical_full.csv") # save full data

LESC_full <- joinPredictors(grid = grid_LESC, 
                            tos = tos_present, 
                            o2os = o2os_present, 
                            phos = phos_present, 
                            chlos = chlos_present, 
                            bathy = bathy,
                            dist2coast = dist2coast
)
write_csv(LESC_full, file = "Output/CSV/LESC_present_full.csv") # save full data

LESC_full <- joinPredictors(grid = grid_LESC, 
                            tos = tos_midCentury, 
                            o2os = o2os_midCentury, 
                            phos = phos_midCentury, 
                            chlos = chlos_midCentury, 
                            bathy = bathy,
                            dist2coast = dist2coast
)
write_csv(LESC_full, file = "Output/CSV/LESC_midCentury_full.csv") # save full data

LESC_full <- joinPredictors(grid = grid_LESC, 
                            tos = tos_endCentury, 
                            o2os = o2os_endCentury, 
                            phos = phos_endCentury, 
                            chlos = chlos_endCentury, 
                            bathy = bathy,
                            dist2coast = dist2coast
)
write_csv(LESC_full, file = "Output/CSV/LESC_endCentury_full.csv") # save full data

# 13. Bluefin tuna
BFT_full <- joinPredictors(grid = grid_BFT, 
                            tos = tos_historical, 
                            o2os = o2os_historical, 
                            phos = phos_historical, 
                            chlos = chlos_historical, 
                            bathy = bathy,
                            dist2coast = dist2coast
)
write_csv(BFT_full, file = "Output/CSV/BFT_historical_full.csv") # save full data

BFT_full <- joinPredictors(grid = grid_BFT, 
                           tos = tos_present, 
                           o2os = o2os_present, 
                           phos = phos_present, 
                           chlos = chlos_present, 
                           bathy = bathy,
                           dist2coast = dist2coast
)
write_csv(BFT_full, file = "Output/CSV/BFT_present_full.csv") # save full data

BFT_full <- joinPredictors(grid = grid_BFT, 
                           tos = tos_midCentury, 
                           o2os = o2os_midCentury, 
                           phos = phos_midCentury, 
                           chlos = chlos_midCentury, 
                           bathy = bathy,
                           dist2coast = dist2coast
)
write_csv(BFT_full, file = "Output/CSV/BFT_midCentury_full.csv") # save full data

BFT_full <- joinPredictors(grid = grid_BFT, 
                           tos = tos_endCentury, 
                           o2os = o2os_endCentury, 
                           phos = phos_endCentury, 
                           chlos = chlos_endCentury, 
                           bathy = bathy,
                           dist2coast = dist2coast
)
write_csv(BFT_full, file = "Output/CSV/BFT_endCentury_full.csv") # save full data

# 14. Little tuna
LIT_full <- joinPredictors(grid = grid_LIT, 
                            tos = tos_historical, 
                            o2os = o2os_historical, 
                            phos = phos_historical, 
                            chlos = chlos_historical, 
                            bathy = bathy,
                            dist2coast = dist2coast
)
write_csv(LIT_full, file = "Output/CSV/LIT_historical_full.csv") # save full data

LIT_full <- joinPredictors(grid = grid_LIT, 
                           tos = tos_present, 
                           o2os = o2os_present, 
                           phos = phos_present, 
                           chlos = chlos_present, 
                           bathy = bathy,
                           dist2coast = dist2coast
)
write_csv(LIT_full, file = "Output/CSV/LIT_present_full.csv") # save full data

LIT_full <- joinPredictors(grid = grid_LIT, 
                           tos = tos_midCentury, 
                           o2os = o2os_midCentury, 
                           phos = phos_midCentury, 
                           chlos = chlos_midCentury, 
                           bathy = bathy,
                           dist2coast = dist2coast
)
write_csv(LIT_full, file = "Output/CSV/LIT_midCentury_full.csv") # save full data

LIT_full <- joinPredictors(grid = grid_LIT, 
                           tos = tos_endCentury, 
                           o2os = o2os_endCentury, 
                           phos = phos_endCentury, 
                           chlos = chlos_endCentury, 
                           bathy = bathy,
                           dist2coast = dist2coast
)
write_csv(LIT_full, file = "Output/CSV/LIT_endCentury_full.csv") # save full data

# 15. Southern bluefin tuna
SBFT_full <- joinPredictors(grid = grid_SBFT, 
                            tos = tos_historical, 
                            o2os = o2os_historical, 
                            phos = phos_historical, 
                            chlos = chlos_historical, 
                            bathy = bathy,
                            dist2coast = dist2coast
)
write_csv(SBFT_full, file = "Output/CSV/SBFT_historical_full.csv") # save full data

SBFT_full <- joinPredictors(grid = grid_SBFT, 
                            tos = tos_present, 
                            o2os = o2os_present, 
                            phos = phos_present, 
                            chlos = chlos_present, 
                            bathy = bathy,
                            dist2coast = dist2coast
)
write_csv(SBFT_full, file = "Output/CSV/SBFT_present_full.csv") # save full data

SBFT_full <- joinPredictors(grid = grid_SBFT, 
                            tos = tos_midCentury, 
                            o2os = o2os_midCentury, 
                            phos = phos_midCentury, 
                            chlos = chlos_midCentury, 
                            bathy = bathy,
                            dist2coast = dist2coast
)
write_csv(SBFT_full, file = "Output/CSV/SBFT_midCentury_full.csv") # save full data

SBFT_full <- joinPredictors(grid = grid_SBFT, 
                            tos = tos_endCentury, 
                            o2os = o2os_endCentury, 
                            phos = phos_endCentury, 
                            chlos = chlos_endCentury, 
                            bathy = bathy,
                            dist2coast = dist2coast
)
write_csv(SBFT_full, file = "Output/CSV/SBFT_endCentury_full.csv") # save full data

# 16. Slender tuna
SLT_full <- joinPredictors(grid = grid_SLT, 
                            tos = tos_historical, 
                            o2os = o2os_historical, 
                            phos = phos_historical, 
                            chlos = chlos_historical, 
                            bathy = bathy,
                            dist2coast = dist2coast
)
write_csv(SLT_full, file = "Output/CSV/SLT_historical_full.csv") # save full data

SLT_full <- joinPredictors(grid = grid_SLT, 
                           tos = tos_present, 
                           o2os = o2os_present, 
                           phos = phos_present, 
                           chlos = chlos_present, 
                           bathy = bathy,
                           dist2coast = dist2coast
)
write_csv(SLT_full, file = "Output/CSV/SLT_present_full.csv") # save full data

SLT_full <- joinPredictors(grid = grid_SLT, 
                           tos = tos_midCentury, 
                           o2os = o2os_midCentury, 
                           phos = phos_midCentury, 
                           chlos = chlos_midCentury, 
                           bathy = bathy,
                           dist2coast = dist2coast
)
write_csv(SLT_full, file = "Output/CSV/SLT_midCentury_full.csv") # save full data

SLT_full <- joinPredictors(grid = grid_SLT, 
                           tos = tos_endCentury, 
                           o2os = o2os_endCentury, 
                           phos = phos_endCentury, 
                           chlos = chlos_endCentury, 
                           bathy = bathy,
                           dist2coast = dist2coast
)
write_csv(SLT_full, file = "Output/CSV/SLT_endCentury_full.csv") # save full data

# 17. Bonitos
BON_full <- joinPredictors(grid = grid_BON, 
                            tos = tos_historical, 
                            o2os = o2os_historical, 
                            phos = phos_historical, 
                            chlos = chlos_historical, 
                            bathy = bathy,
                            dist2coast = dist2coast
)
write_csv(BON_full, file = "Output/CSV/BON_historical_full.csv") # save full data

BON_full <- joinPredictors(grid = grid_BON, 
                           tos = tos_present, 
                           o2os = o2os_present, 
                           phos = phos_present, 
                           chlos = chlos_present, 
                           bathy = bathy,
                           dist2coast = dist2coast
)
write_csv(BON_full, file = "Output/CSV/BON_present_full.csv") # save full data

BON_full <- joinPredictors(grid = grid_BON, 
                           tos = tos_midCentury, 
                           o2os = o2os_midCentury, 
                           phos = phos_midCentury, 
                           chlos = chlos_midCentury, 
                           bathy = bathy,
                           dist2coast = dist2coast
)
write_csv(BON_full, file = "Output/CSV/BON_midCentury_full.csv") # save full data

BON_full <- joinPredictors(grid = grid_BON, 
                           tos = tos_endCentury, 
                           o2os = o2os_endCentury, 
                           phos = phos_endCentury, 
                           chlos = chlos_endCentury, 
                           bathy = bathy,
                           dist2coast = dist2coast
)
write_csv(BON_full, file = "Output/CSV/BON_endCentury_full.csv") # save full data

# 18. Black marlin
BLAM_full <- joinPredictors(grid = grid_BLAM, 
                            tos = tos_historical, 
                            o2os = o2os_historical, 
                            phos = phos_historical, 
                            chlos = chlos_historical, 
                            bathy = bathy,
                            dist2coast = dist2coast
)
write_csv(BLAM_full, file = "Output/CSV/BLAM_historical_full.csv") # save full data

BLAM_full <- joinPredictors(grid = grid_BLAM, 
                            tos = tos_present, 
                            o2os = o2os_present, 
                            phos = phos_present, 
                            chlos = chlos_present, 
                            bathy = bathy,
                            dist2coast = dist2coast
)
write_csv(BLAM_full, file = "Output/CSV/BLAM_present_full.csv") # save full data

BLAM_full <- joinPredictors(grid = grid_BLAM, 
                            tos = tos_midCentury, 
                            o2os = o2os_midCentury, 
                            phos = phos_midCentury, 
                            chlos = chlos_midCentury, 
                            bathy = bathy,
                            dist2coast = dist2coast
)
write_csv(BLAM_full, file = "Output/CSV/BLAM_midCentury_full.csv") # save full data

BLAM_full <- joinPredictors(grid = grid_BLAM, 
                            tos = tos_endCentury, 
                            o2os = o2os_endCentury, 
                            phos = phos_endCentury, 
                            chlos = chlos_endCentury, 
                            bathy = bathy,
                            dist2coast = dist2coast
)
write_csv(BLAM_full, file = "Output/CSV/BLAM_endCentury_full.csv") # save full data

} else {
  
}