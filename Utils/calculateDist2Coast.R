# Description: Calculate the distance to the nearest coastline

calculateDist2Coast <- function(grid) {
  # Load coast
  coast <- rnaturalearth::ne_coastline(scale = 'large') %>% 
    sf::st_as_sf(crs = lonlat) %>% 
    fSpatPlan_Convert2PacificCentered(., cCRS = cCRS)
  
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
  
  saveRDS(grid_centroid, here::here("Data", "CoastDistance.rds")) # save the coast distance df
  
  return(grid_centroid)
}