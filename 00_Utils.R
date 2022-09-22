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

# Load worldwide landmass
landmass <- rnaturalearth::ne_countries() %>% 
  sf::st_as_sf(crs = lonlat)

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
    sf::st_as_sf(crs = lonlat)
  
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
    sf <- readRDS(file.path(path, list[file[i]])) %>% 
      tibble::as_tibble()
  }
  combined <- do.call(bind_rows, df) %>% 
    dplyr::mutate(across(where(is.character), ~factor(.)), # change all character columns in  to factors
                  abundance = ifelse(abundance > 0, yes = 1, no = 0)) %>% # convert data into binary
    dplyr::select(species, abundance, everything())
  
  sf <- combined %>% # convert into sf
    sf::st_as_sf(sf_column_name = "geometry") %>% 
    sf::st_centroid()
  
  return(sf)
  
}

# Convert bathymetry data (.TIFF) to sf
gebcoConvert <- function() {
  path <- "Data/GEBCO"
  list <- list.files(path)
  x <- apply(outer(list, ".tif", stringr::str_detect), 1, all) %>% as.numeric()
  file <- which(x == 1)
  
  gebco <- terra::rast(file.path(path, list[file[i]])) %>%  # call raster file
    terra::aggregate(., fact = 10) # aggregate by a factor of 10
  
  # convert into sf
  gebco_sf <- gebco %>% 
    terra::as.polygons(trunc = FALSE, dissolve = FALSE, na.rm = FALSE) %>% 
    sf::st_as_sf(crs = lonlat)
  
  # TODO: interpolate into grid cells
  
}