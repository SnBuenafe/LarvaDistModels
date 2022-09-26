# Load preliminaries
source("00_Utils.R")

#######################
# Establish grid data #
#######################
# landmass, let's do medium?
landmass <- rnaturalearth::ne_countries(scale = "medium") %>% 
  sf::st_as_sf(crs = lonlat) %>% 
  sf::st_transform(crs = moll)

# Using the mollweide projection (moll)
Bndry <- spatialplanr::SpatPlan_Get_Boundary(Limits = "Global",
                                             Type = NA)

grid <- spatialplanr::SpatPlan_Get_PlanningUnits(Bndry,
                                                 landmass,
                                                 CellArea = 2500, # let's do half a degree? (~ 50 km x 50 km)
                                                 Shape = "square",
                                                 inverse = FALSE)

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
tos <- nc2sf(model = "ACCESS-ESM1-5",
             expt = "ssp585",
             var = "tos") %>% 
  dplyr::rename(tos = mean) %>% 
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "tos") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, tos_transformed, geometry)

# 2. Oxygen
o2os <- nc2sf(model = "ACCESS-ESM1-5",
              expt = "ssp585",
              var = "o2os") %>% 
  dplyr::rename(o2os = mean) %>% 
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "o2os") %>%
  dplyr::as_tibble( )%>% 
  dplyr::select(cellID, o2os_transformed, geometry)

# 3. pH
phos <- nc2sf(model = "CMCC-ESM2",
              expt = "ssp585",
              var = "phos") %>% 
  dplyr::rename(phos = mean) %>% 
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "phos") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, phos_transformed, geometry)

# Bathymetry
#bathy <- gebcoConvert(grid, 2500) # bathymetry data is extrapolated depending on the grid area provided
bathy <- readRDS("Data/GEBCO/gebco2500.rds") %>% 
  dplyr::as_tibble()
###############################################################
# Joining all predictor and response per fish species #
###############################################################

# Joining all the data with the species data
YFT_full <- dplyr::left_join(tos, o2os, by = c("cellID", "geometry")) %>% 
  dplyr::left_join(., phos, by = c("cellID", "geometry")) %>% 
  dplyr::left_join(., bathy, by = c("cellID", "geometry")) %>% 
  dplyr::left_join(grid_YFT, ., by = c("cellID", "geometry")) %>%  # Join with species data
  dplyr::select(cellID, species, abundance, season, longitude, latitude, tos_transformed, o2os_transformed, phos_transformed, meanDepth, everything()) # arrange columns
