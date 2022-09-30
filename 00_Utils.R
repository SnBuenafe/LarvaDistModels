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
})

lonlat <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
moll <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs"

# Load worldwide landmass
landmass <- rnaturalearth::ne_countries(scale = "medium") %>% 
  sf::st_as_sf(crs = lonlat) %>% 
  sf::st_transform(crs = moll)

# Establish the grid
# Using the mollweide projection (moll)
Bndry <- spatialplanr::SpatPlan_Get_Boundary(Limits = "Global",
                                             Type = NA)

grid <- spatialplanr::SpatPlan_Get_PlanningUnits(Bndry,
                                                 landmass,
                                                 CellArea = 2500, # let's do half a degree? (~ 50 km x 50 km)
                                                 Shape = "square",
                                                 inverse = FALSE)

#### Helper functions ####

# Convert netcdf to sf objects
nc2sf <- function(model, expt, var) {
  path <- "Data/Climatology/final1/"
  list <- list.files(path)
  param <- c(model, expt, var)
  
  file <- apply(outer(list, param, stringr::str_detect), 1, all) %>% as.numeric()
  file <- which(file == 1)
  
  rs <- terra::rast(paste0(path, list[file])) # Create RasterStack
  
  names(rs) <- paste("X", seq(from = 2015, to = 2100, by = 1), sep = "")
  
  sf <- rs %>%  
    terra::subset(., paste("X", seq(from = 2022, to = 2050, by = 1), sep = "")) %>% # 2022-2050
    terra::as.polygons(trunc = FALSE, dissolve = FALSE, na.rm=FALSE) %>% # Convert to sf polygon
    sf::st_as_sf() %>% 
    sf::st_make_valid()
  
  sf::st_crs(sf) <- lonlat # set the CRS
  sf %<>% sf::st_transform(crs = moll) # then transform
  
 # Removing degree grids that wholly or partially intersect with the landmass object.
  logi_Reg <- sf::st_centroid(sf) %>%
    sf::st_intersects(landmass) %>%
    lengths > 0 # Get logical vector instead of sparse geometry binary
  
  int <- sf[!logi_Reg, ]
  
  fin <- int %>% 
    dplyr::mutate(mean = rowMeans(across(1:(ncol(int)-1)), na.rm = FALSE)) %>% 
    dplyr::select(mean)
  
  return(fin)
}

# combine seasonal data.frames of species into one sf object
combineFish <- function(species) {
  
  path <- "Data/Fish"
  list <- list.files(path)
  param <- c(species, ".rds")
  x <- apply(outer(list, param, stringr::str_detect), 1, all) %>% as.numeric()
  file <- which(x == 1)
  
  df <- list()
  for(i in 1:length(file)) {
    df[[i]] <- readRDS(file.path(path, list[file[i]])) %>% 
      tibble::as_tibble()
  }
  combined <- do.call(bind_rows, df) %>% 
    dplyr::mutate(across(where(is.character), ~factor(.)) #, # change all character columns in  to factors
                  #abundance = ifelse(abundance > 0, yes = 1, no = 0)
                  ) %>% # convert data into binary
    dplyr::select(species, abundance, everything())
  
  sf <- combined %>% # convert into sf
    sf::st_as_sf(sf_column_name = "geometry")
  
  return(sf)
  
}

# Convert bathymetry data (.TIFF) to sf (based on the grid provided)
gebcoConvert <- function(grid, area) {
  
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
    
    joined %<>% 
      dplyr::mutate(meanDepth = rowMeans(joined[, 3:10], na.rm = TRUE))
    
    joined_sf <- joined %>% # converting into sf
      sf::st_as_sf(sf_column_name = "geometry") %>% 
      dplyr::select(cellID, meanDepth, geometry)
    
    saveRDS(joined_sf, paste0(path, "/gebco", area, ".rds"))
    
    return(joined_sf)
  
}

# Replacing NAs with the nearest neighborhood value
replaceNN <- function(climate, grid, colname) {
  filtered <- climate %>% dplyr::filter(!is.na(!!sym(colname)))
  vector <- purrr::as_vector(sf::st_nearest_feature(grid, filtered))
  
  mutatedDF <- climate %>% 
    dplyr::mutate(!!sym(paste0(colname, "_transformed")) := ifelse(is.na(!!sym(colname)),
                                                            yes = (filtered[[ colname ]])[vector[cellID]],
                                                            no = !!sym(colname)))
}

# Calculate the distance to the nearest coastline
calculateDist2Coast <- function(grid) {
  # Load coast
  coast <- rnaturalearth::ne_coastline(scale = 'large') %>% 
    sf::st_as_sf(crs = lonlat) %>% 
    sf::st_transform(crs = moll)
  
  # Convert grid to points (centroids)
  grid_centroid <- grid %>% 
    sf::st_centroid()
  
  # Find the nearest coast for all the grid cells
  nearest <- sf::st_nearest_feature(grid_centroid, coast)
  
  dists <- c()
  for(i in 1:nrow(grid_centroid)) {
    # get the distance and populate an empty vector with it
    dists[i] <- sf::st_distance(grid_centroid[i, ], coast[nearest[i], ])
    print(dists[i])
  }
  
  # then add that in the grid_centroid df
  grid_centroid$coastDistance <- dists
  
  saveRDS(grid_centroid, "Data/CoastDistance.rds") # save the coast distance df
}
