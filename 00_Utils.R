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

# Convert rs to sf objects
rs2sf <- function(rs) {
  sf <- rs %>% 
    terra::as.polygons(trunc = FALSE, dissolve = FALSE, na.rm=FALSE) %>% # Convert to sf polygon
    sf::st_as_sf()
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

# assemble the fish data wrt grid
assembleGrid <- function(grid, sf) {
  grid_sf <- sf::st_join(grid, sf, join = st_contains_properly, left = TRUE)# join with the grid data if the centroid is contained within the grid
  
  centroid <- grid_sf %>% 
    sf::st_centroid() %>% # Convert to points
    sf::st_transform(crs = lonlat) # Transform back to lonlat
  
  centroid_coordinates <- sf::st_coordinates(centroid) # Get cooridnates
  
  grid_sf %<>% dplyr::mutate(longitude = ifelse(!is.na(longitude), 
                                                yes = longitude, 
                                                no = centroid_coordinates[cellID, "X"]),
                             latitude = ifelse(!is.na(latitude), 
                                               yes = latitude, 
                                               no = centroid_coordinates[cellID, "Y"])) %>% 
    dplyr::as_tibble()
  
  return(grid_sf)
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

##########
# Plotting
##########

# Plot the model
plotModel <- function(sf, saveFile) {
  #palette = brewer.pal(9, "YlOrBr")
  ggmodel <- ggplot() + 
    geom_sf(data = sf, aes(fill = model, color = model), size = 0.1) +
    #scale_fill_gradientn(name = "Probabilities",
    #                     colors = palette,
    #                     aesthetics = c("fill", "color")) +
    scale_fill_cmocean("Probabilities",
                       name = "tempo", 
                       aesthetics = c("color", "fill")) +
    geom_sf(data = landmass, fill = "black", color = NA, size = 0.01) +
    theme_bw()
  ggsave(plot = ggmodel, filename = saveFile, width = 15, height = 8, dpi = 300)
  
  return(ggmodel)
}

# Plot the abundance
plotAbundance <- function(sf, saveFile) {
  ggabundance <- ggplot() + 
    geom_sf(data = sf, aes(fill = as.factor(abundance), color = as.factor(abundance)), size = 0.1) +
    scale_fill_brewer(name = "Abundance",
                      aesthetics = c("fill", "color"),
                      palette = "RdPu") +
    geom_sf(data = landmass, fill = "black", color = NA, size = 0.01) +
    theme_bw()
  ggsave(plot = ggabundance, filename = saveFile, width = 15, height = 8, dpi = 300)
  
  return(ggabundance)
}

# Plot the presence/absence
plotPA <- function(sf, saveFile) {
  ggpresence <- ggplot() +
    geom_sf(data = sf, aes(fill = as.factor(abundance_presence), color = as.factor(abundance_presence)), size = 0.1) +
    scale_color_manual(name = "Presence/Absence",
                       aesthetics = c("color", "fill"),
                       values = c("#feebe2", "#7a0177")) +
    geom_sf(data = landmass, fill = "black", color = NA, size = 0.01) +
    theme_bw()
  
  ggsave(plot = ggpresence, filename = saveFile, width = 15, height = 8, dpi = 300)
  
  return(ggpresence)
}

# Plot predictions
plotPredictions <- function(sf, saveFile) {
  ggpreds <- ggplot() + 
    geom_sf(data = sf, aes(fill = predictions, color = predictions), size = 0.1) +
    scale_fill_cmocean("Predictions",
                       name = "tempo", 
                       aesthetics = c("color", "fill")) +
    geom_sf(data = landmass, fill = "black", color = NA, size = 0.01) +
    theme_bw()
  ggsave(plot = ggpreds, filename = saveFile, width = 15, height = 8, dpi = 300)
  
  return(ggpreds)
}
