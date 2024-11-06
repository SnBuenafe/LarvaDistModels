# DESCRIPTION: Preliminary file for setting the study area

# Load packages
# install.packages("pacman")
# devtools::install_github("ropensci/rnaturalearthhires")
pacman::p_load(here, sf, rnaturalearth, rnaturalearthhires, tidyverse, magrittr)
source("functions/fSpatPlan_Convert2PacificCentered.R")
source("functions/spatialplanrfxns.R")

# Define map projections
sf_use_s2(FALSE)
lonlat <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
moll_pacific <- "+proj=moll +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs"
ll_pacific <- "+proj=longlat +lon_0=180 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
cCRS = ll_pacific 

# Load worldwide landmass
landmass <- ne_countries(scale = "medium") %>% 
  st_as_sf(crs = lonlat) %>% 
  fSpatPlan_Convert2PacificCentered(., cCRS = cCRS)

# Load worldwide ocean
# oceans <- sf::read_sf("Data/ne_50m_geography_marine_polys/ne_50m_geography_marine_polys.shp")
oceans <- ne_download(category = "physical", scale = "medium", type = "geography_marine_polys", returnclass='sf') %>%
  dplyr::select(label) %>% 
  st_as_sf(crs = lonlat) %>% 
  fSpatPlan_Convert2PacificCentered(., cCRS = cCRS) # Pacific-centered

# Establish the grid
Bndry <- SpatPlan_Get_Boundary(Limits = c(xmin = -40, xmax = 40, ymax = 40, ymin = -40),
                                             cCRS = cCRS) 

grid <- st_make_grid(Bndry,
                         square = TRUE,
                         cellsize = c(1,1),
                         what = "polygons") %>%
  st_sf()

# First get all the PUs partially/wholly within the planning region
logi_Reg <- st_centroid(grid) %>%
  st_intersects(Bndry) %>%
  lengths > 0 # Get logical vector instead of sparse geometry binary

grid <- grid[logi_Reg, ] # Get TRUE

# Second, get all the pu's with < 50 % area on land (approximated from the centroid)
logi_Ocean <- st_centroid(grid) %>%
  st_intersects(oceans) %>%
  lengths > 0 # Get logical vector instead of sparse geometry binary

grid <- grid[logi_Ocean==TRUE, ] # Get TRUE

grid <- grid %>%
  mutate(cellID = row_number()) # Add a cell ID reference

# Filter water bodies within the Indian and Pacific Oceans
tmp <- st_nearest_feature(grid, oceans)
grid %<>%
  mutate(ocean = oceans$label[tmp[cellID]]) %>% 
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

# Building 10x10 grid for increasing confidence in models
grid_100 <- st_make_grid(Bndry,
                             square = TRUE,
                             cellsize = c(10,10),
                             what = "polygons") %>%
  st_sf()

# First get all the PUs partially/wholly within the planning region
logi_Reg <- st_centroid(grid_100) %>%
  st_intersects(Bndry) %>%
  lengths > 0 # Get logical vector instead of sparse geometry binary

grid_100 <- grid_100[logi_Reg, ] # Get TRUE

grid_100 %<>%
  mutate(cellID = row_number()) # Add a cell ID reference
