# Description: Convert spatial gradients to sf objects
spat2sf <- function(rs) {
  sf <- rs %>% 
    as.polygons(trunc = FALSE, dissolve = FALSE, na.rm=FALSE) %>% # Convert to sf polygon
    st_as_sf()
  st_crs(sf) <- lonlat # set the CRS
  
  sf %<>% fSpatPlan_Convert2PacificCentered(., cCRS = cCRS) # then transform
  
  # Removing degree grids that wholly or partially intersect with the landmass object.
  logi_Reg <- st_centroid(sf) %>%
    st_intersects(landmass) %>%
    lengths > 0 # Get logical vector instead of sparse geometry binary
  
  int <- sf[!logi_Reg, ]
  
  return(int)
}