# Description: Calculate the distance to the nearest coastline

calculateDist2Coast <- function(grid) {
  # Load coast
  coast <- rnaturalearth::ne_coastline(scale = 'large') %>% 
    sf::st_as_sf(crs = lonlat) %>% 
    fSpatPlan_Convert2PacificCentered(., cCRS = moll_pacific)
  
  # Convert grid to points (centroids)
  grid_centroid <- grid %>% 
    sf::st_transform(crs = moll_pacific) %>% # Convert to Mollweide specifically for calculating the distance
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
  
  grid_out <- dplyr::left_join(grid %>% 
                                 dplyr::as_tibble(), 
                               grid_centroid %>%
                                 dplyr::as_tibble() %>% 
                                 dplyr::select(-geometry)) %>%  # join that with the grid cells
    dplyr::select(cellID, ocean, coastDistance, geometry) %>% # arrange columns
    sf::st_as_sf(crs = cCRS)
  
  saveRDS(grid_out, here::here(output_dir, "CoastDistance_new.rds")) # save the coast distance df
  
  return(grid_out)
}