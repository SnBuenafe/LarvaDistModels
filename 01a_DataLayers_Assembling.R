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

grid_YFT <- sf::st_join(grid, YFT_sf, join = st_contains_properly, left = TRUE) # join with the grid data if the centroid is contained within the grid

####################
# Predictor data #
####################
# Climate
# 1. Temperature
tos <- nc2sf(model = "ACCESS-ESM1-5",
             expt = "ssp585",
             var = "tos") %>% 
  dplyr::rename(tos = mean) %>% 
  tibble::as_tibble()

# 2. Oxygen
o2os <- nc2sf(model = "ACCESS-ESM1-5",
              expt = "ssp585",
              var = "o2os") %>% 
  dplyr::rename(o2os = mean) %>% 
  tibble::as_tibble()

# 3. pH
phos <- nc2sf(model = "CMCC-ESM2",
              expt = "ssp585",
              var = "phos") %>% 
  dplyr::rename(phos = mean) %>% 
  tibble::as_tibble()

# Bathymetry
bathy <- gebcoConvert(grid, 2500) # bathymetry data is extrapolated depending on the grid area provided

###############################################################
# Joining all predictor and response per fish species #
###############################################################


# then save as data.frames perhaps to be called in later scripts