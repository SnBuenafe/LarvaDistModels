# Call packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(ncdf4)
  library(terra)
  library(sf)
  library(rnaturalearth)
  library(cmocean)
  library(patchwork)
  library(magrittr)
  library(raster)
  library(dismo) # for boosted regression trees
  library(RColorBrewer)
  library(patchwork)
  library(Hmisc)
  library(corrplot)
  library(VoCC)
  library(stars)
  library(spatialplanr)
  library(ggpattern)
  library(here)
})

# Load all helper functions
utils <- list.files(path = here::here("Utils"), pattern = "*.R", full.names = TRUE)
sapply(X = utils, FUN = source) %>% invisible()

# Define map projections
sf_use_s2(FALSE)
lonlat <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
pc_pacific <- "+proj=eqc +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs"
cCRS = pc_pacific # Use equidistant projection, Plate Careé

# Load worldwide landmass
landmass <- rnaturalearth::ne_countries(scale = "medium") %>% 
  sf::st_as_sf(crs = lonlat) %>% 
  fSpatPlan_Convert2PacificCentered(., cCRS = pc_pacific)

# Load worldwide ocean
oceans <- sf::read_sf("Data/ne_50m_geography_marine_polys/ne_50m_geography_marine_polys.shp") %>%
  dplyr::select(label) %>% 
  sf::st_as_sf(crs = lonlat) %>% 
  fSpatPlan_Convert2PacificCentered(., cCRS = pc_pacific)

# Establish the grid
Bndry <- spatialplanr::SpatPlan_Get_Boundary(Limits = c(xmin = -40, xmax = 40, ymax = 40, ymin = -40),
                                             cCRS = pc_pacific) 
  
grid <- spatialplanr::SpatPlan_Get_PlanningUnits(Bndry,
                                                 oceans,
                                                 CellArea = 10000, # let's do a degree? (~ 100 km x 100 km)
                                                 Shape = "square",
                                                 inverse = TRUE)

# Filter water bodies within the Indian and Pacific Oceans
tmp <- sf::st_nearest_feature(grid, oceans)
grid %<>%
  dplyr::mutate(ocean = oceans$label[tmp[cellID]]) %>% 
  dplyr::filter(ocean %in% c('Andaman Sea', 'Arabian Sea', 'Arafura Sea', 'Banda Sea', 'Bay of Bengal', 
                              'Bay of Plenty', 'Bering Sea', 'Bismarck Sea', 'Bo Hai', 'Bristol Bay',
                             'Celebes Sea', 'Ceram Sea', 'Coral Sea', 'East China Sea', 
                             'INDIAN OCEAN', 'Java Sea', 'Korea Strait', 'Laccadive Sea', 'Golfo de California',
                             'Great Australian Bight', 'Gulf of Alaska', 'Gulf of Carpentaria', 'Gulf of Kutch',
                             'Gulf of Mannar', 'Golfo de Panamá', 'Gulf of Thailand', 'Gulf of Tonkin',
                             'Makassar Strait', 'Molucca Sea', 'Mozambique Channel', 'NORTH PACIFIC OCEAN',
                             'Philippine Sea', 'Sea of Japan', 'Sea of Okhotsk', 'Shelikhova Gulf',
                             'Solomon Sea', 'South China Sea', 'Strait of Singapore',
                             'SOUTH PACIFIC OCEAN', 'Strait of Malacca',
                             'Sulu Sea', 'Taiwan Strait', 'Tasman Sea', 'Timor Sea', 'Yellow Sea')) %>% 
  dplyr::mutate(cellID = row_number())

