suppressPackageStartupMessages({
  library(tidyverse)
  library(ncdf4)
  library(terra)
  library(sf)
  library(rnaturalearth)
  library(cmocean)
  library(patchwork)
  library(magrittr)
})

# Load worldwide landmass
landmass <- rnaturalearth::ne_countries() %>% 
  sf::st_as_sf(crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

# Convert netcdf data (across years) to the model's 30-year mean
nc2sf <- function(model, expt, var) {
  path <- "Data/final/"
  list <- list.files(path)
  param <- c(model, expt, var)
  
  file <- apply(outer(list, param, stringr::str_detect), 1, all) %>% as.numeric()
  file <- which(file == 1)
  
  rs <- terra::rast(paste0(path, list[file])) %>%  # Create RasterStack
    terra::as.polygons(trunc = FALSE, dissolve = FALSE, na.rm=FALSE) %>%
    sf::st_as_sf(crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  
  sf_use_s2(FALSE)
  x <- st_intersects(rs, landmass, sparse = FALSE) %>% 
    apply(., 1, any) # returns indices of rows in the data that have intersected with the landmass
  rs[which(x==TRUE), 1:30] <- NA # Change those that intersect with land with NAs.
  
  fin <- rs %>% 
    dplyr::mutate(mean = rowMeans(across(1:30), na.rm = FALSE)) %>% 
    dplyr::select(mean)
  
  return(fin)
}
