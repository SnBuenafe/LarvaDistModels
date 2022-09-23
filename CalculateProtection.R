# Calculating the % protection for deep and shallow waters...
# Load up packages
library(terra)
library(tidyverse)
library(sf)
library(spatialplanr)
library(magrittr)
library(stars)
library(doParallel)
library(purrr)

# Define projections used
lonlat <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
moll <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs"

# Load worldwide landmass
landmass <- rnaturalearth::ne_countries() %>% 
  sf::st_as_sf(crs = lonlat) %>% 
  sf::st_transform(crs = moll)

# Load WDPA data
WDPA <- sf::st_read("Data/WDPA/WDPA_Sep2022_Public_shp-polygons.shp") %>% 
  dplyr::filter(IUCN_CAT %in% c("Ia", "Ib", "II", "III", "IV")) %>% 
  dplyr::filter(MARINE > 0) %>% # remove terrestrial reserves
  dplyr::filter(STATUS %in% c("Designated", "Established", "Inscribed")) %>%  # from spatialplanr
  sf::st_transform(crs = lonlat) %>% 
  sf::st_transform(crs = moll)

# Define the grid
Bndry <- spatialplanr::SpatPlan_Get_Boundary(Limits = "Global",
Type = NA)

grid <- spatialplanr::SpatPlan_Get_PlanningUnits(Bndry,
                                                 landmass,
                                                 CellArea = 1000, # Default was 1000
                                                 Shape = "square",
                                                 inverse = FALSE)
  
path <- "Data/GEBCO"
list <- list.files(path)
x <- apply(outer(list, ".tif", stringr::str_detect), 1, all) %>% as.numeric()
file <- which(x == 1)

grid_tibble <- grid %>% 
  tibble::as_tibble()
grid_filled <- list()

for(i in 1:length(file)) {
  
  gebco <- terra::rast(file.path(path, list[file[i]])) %>% 
    terra::aggregate(., fact = 10)
  
  # convert to sf object
  gebco_reclassified_sf <- gebco %>% 
    terra::as.polygons(trunc = FALSE, dissolve = FALSE, na.rm = FALSE) %>% 
    sf::st_as_sf(crs = lonlat) %>% 
    sf::st_transform(crs = moll)
  
  colnames(gebco_reclassified_sf)[1] <- "depth" # change column name of the first column
  
  time <- system.time(grid_filled[[i]] <- gebco_reclassified_sf %>% 
                        st_interpolate_aw(grid, extensive = FALSE) %>% 
                        tibble::as_tibble() %>% 
                        left_join(grid_tibble, ., by = "geometry"))
  
  print(time)
  
  print("Interpolated")
}

  
# rename columns
for(i in 1:8) {
  name = paste0("depth", i)
  grid_filled[[i]] %<>% dplyr::rename(!!sym(name) := depth)
}
  
joined <- purrr::reduce(grid_filled, left_join, by = c("cellID", "geometry")) # join all 8 files
joined %<>% # get the mean depth
  dplyr::mutate(meanDepth = rowMeans(joined[, 3:10], na.rm = TRUE))
  
joined_sf <- joined %>% # converting into sf
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  dplyr::select(cellID, meanDepth, geometry)

saveRDS(joined_sf, "Data/GEBCO/gebco1000.rds") # saving the file
#joined_sf <- readRDS("Data/GEBCO/gebco1000.rds")

joined_tibble <- joined_sf %>% # converting it into a tibble
  tibble::as_tibble()

# We intersect the WDPA data with our 1000km^2 grid
WDPA_int <- sf::st_intersection(grid, WDPA) %>% 
  dplyr::select(WDPAID, cellID)

#saveRDS(WDPA_int, "Data/wdpa_int.rds")
#WDPA_int <- readRDS("Data/wdpa_int.rds")

WDPA_int$Area <- sf::st_area(WDPA_int) %>%  # getting the area of intersection between grid and protected areas
  units::set_units(km^2)

# get the area for each grid cell
area <- WDPA_int %>% 
  dplyr::group_by(cellID) %>% 
  dplyr::summarise(sumArea = sum(Area, na.rm = TRUE))

area_tibble <- area %>% # convert into tibble so we can left_join
  tibble::as_tibble() %>% 
  dplyr::select(-geometry)

# join the protected area database with the grid
join_WDPA <- left_join(joined_tibble, area_tibble, by = "cellID")

# count how many cells have positive depths
nrow(join_WDPA %>% dplyr::filter(meanDepth > 0)) # remove the positive depths?
# what's the maximum elevation?
max(join_WDPA$meanDepth)

# what is the area of each planning unit in km2
PU_area <- sf::st_area(joined_sf[1,]) %>% 
  units::set_units(km^2)

classify <- join_WDPA %>% 
  dplyr::mutate(depth = case_when(meanDepth > -200 ~ "epipelagic", # I decided to assign all PUs with meanDepths >= 0 as epipelagic since I already took out "land" when creating the grid
                                  meanDepth <= -200 ~ "deep"))

classify_sf <- classify %>% # created an sf object
  sf::st_as_sf(sf_column_name = "geometry")

# what is the total ocean surface?
(OCEAN = sum(sf::st_area(classify_sf)) %>% 
    units::set_units(km^2))

# what is the total epipelagic surface?
(EPIPELAGIC = sum(sf::st_area(classify_sf %>% dplyr::filter(depth == "epipelagic"))) %>% 
    units::set_units(km^2))
# what is the percentage?
EPIPELAGIC*100/OCEAN

# what is the total deep surface?
(DEEP = sum(sf::st_area(classify_sf %>% dplyr::filter(depth == "deep"))) %>% 
    units::set_units(km^2))
DEEP*100/OCEAN

# what is the total protected area in the deep ocean
(DEEP_PROTECTED = sum((classify_sf %>% dplyr::filter(depth == "deep"))$sumArea, na.rm = TRUE)) # filter planning units that are considered deep and add all the protected surface! that's better I think
DEEP_PROTECTED*100/DEEP

# what is the total protected area in the epipelagic ocean
(EPI_PROTECTED = sum((classify_sf %>% dplyr::filter(depth == "epipelagic"))$sumArea, na.rm = TRUE)) # filter planning units that are considered deep and add all the protected surface! that's better I think
EPI_PROTECTED*100/EPIPELAGIC
