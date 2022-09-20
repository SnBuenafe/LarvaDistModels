suppressPackageStartupMessages({
  library(tidyverse)
  library(ncdf4)
  library(terra)
  library(sf)
  library(rnaturalearth)
  library(cmocean)
  library(patchwork)
  library(magrittr)
  library(FNN)
  library(raster)
  library(VoCC)
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
