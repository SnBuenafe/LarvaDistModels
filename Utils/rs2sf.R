# Description: Convert raster to sf objects

rs2sf <- function(rs) {
  sf <- rs %>% 
    terra::as.polygons(trunc = FALSE, dissolve = FALSE, na.rm=FALSE) %>% # Convert to sf polygon
    sf::st_as_sf()
  sf::st_crs(sf) <- lonlat # set the CRS
  
  sf %<>% fSpatPlan_Convert2PacificCentered(., cCRS = cCRS) # then transform
  
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