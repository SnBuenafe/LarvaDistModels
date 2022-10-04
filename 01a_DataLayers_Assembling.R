# Load preliminaries
source("00_Utils.R")

####################
# Fish data #
####################
# Assemble yellowfin tuna data
YFT_sf <- combineFish(species = "yellowfin-tuna") %>% 
  sf::st_transform(crs = moll) %>% 
  sf::st_centroid() # transform into point data

grid_YFT <- sf::st_join(grid, YFT_sf, join = st_contains_properly, left = TRUE) %>% # join with the grid data if the centroid is contained within the grid
  dplyr::as_tibble()

####################
# Predictor data #
####################
# Climate
# 1. Temperature
# If needed to reprocess, run all of these...
# Convert all models to sf
# tos_model_list <- c("BCC-CSM2-MR", "CMCC-CM2-SR5", "CMCC-ESM2", "FGOALS-f3-L", "FGOALS-g3", "MIROC6", "MIROC-ES2L",
#                     "MRI-ESM2-0", "NorESM2-LM")
# # Create them only for historical for now
# createLayers(tos_model_list, "historical", "tos") # 1956-1981 averages
# # Create the ensemble
# createEnsemble("historical", "tos")

tos <- readRDS("Data/Climatology/ensemble/Ensemble-historical-tos.rds") %>% 
  dplyr::rename(tos = EnsembleMean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "tos") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, tos_transformed, geometry)

# 2. Oxygen
# # If needed to reprocess, run all of these...
# o2os_model_list <- c("CMCC-ESM2", "MIROC-ES2L", "MRI-ESM2-0", "NorESM2-LM")
# # Create them only for historical for now
# createLayers(o2os_model_list, "historical", "o2os") # 1956-1981 averages
# # Create the ensemble
# createEnsemble("historical", "o2os")

o2os <- readRDS("Data/Climatology/ensemble/Ensemble-historical-o2os.rds") %>% 
  dplyr::rename(o2os = EnsembleMean) %>% # using the mean of the models 
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "o2os") %>%
  dplyr::as_tibble( )%>% 
  dplyr::select(cellID, o2os_transformed, geometry)

# 3. pH
# # If needed to reprocess, run all of these...
# phos_model_list <- c("CMCC-ESM2", "MIROC-ES2L", "MRI-ESM2-0", "NorESM2-LM")
# # Create them only for historical for now
# createLayers(phos_model_list, "historical", "phos") # 1956-1981 averages
# # Create the ensemble
# createEnsemble("historical", "phos")

phos <- readRDS("Data/Climatology/ensemble/Ensemble-historical-phos.rds") %>% 
  dplyr::rename(phos = EnsembleMean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "phos") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, phos_transformed, geometry)

# 4. chlorophyll-a
# # If needed to reprocess, run all of these...
# chlos_model_list <- c("CMCC-ESM2", "MIROC-ES2L", "MRI-ESM2-0", "NorESM2-LM")
# # Create them only for historical for now
# createLayers(chlos_model_list, "historical", "chlos")
# # Create the ensemble
# createEnsemble("historical", "chlos")

chlos <- readRDS("Data/Climatology/ensemble/Ensemble-historical-chlos.rds") %>% 
  dplyr::rename(chlos = EnsembleMean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "chlos") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, chlos_transformed, geometry)

# Bathymetry
#bathy <- gebcoConvert(grid, 2500) # bathymetry data is extrapolated depending on the grid area provided
bathy <- readRDS("Data/GEBCO/gebco2500.rds") %>% 
  dplyr::as_tibble()

# Coastline
#dist2coast <- calculateDist2Coast(grid) # distance to coast is calculated depending on the grid area provided
dist2coast <- readRDS("Data/CoastDistance.rds") %>% 
  dplyr::as_tibble() %>% 
  dplyr::select(-geometry)

###############################################################
# Joining all predictor and response per fish species #
###############################################################

# Joining all the data with the species data
YFT_full <- dplyr::left_join(tos, o2os, by = c("cellID", "geometry")) %>% 
  dplyr::left_join(., phos, by = c("cellID", "geometry")) %>% 
  dplyr::left_join(., chlos, by = c("cellID", "geometry")) %>% 
  dplyr::left_join(., bathy, by = c("cellID", "geometry")) %>% 
  dplyr::left_join(., dist2coast, by = c("cellID")) %>% 
  dplyr::left_join(grid_YFT, ., by = c("cellID", "geometry")) %>%  # Join with species data
  dplyr::select(cellID, species, abundance, season, longitude, latitude, tos_transformed, o2os_transformed, phos_transformed, chlos_transformed, meanDepth, coastDistance, geometry) # arrange columns

write_csv(YFT_full, file = "Output/YFT_full.csv") # save full data
