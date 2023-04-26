# TODO: fix this 
# Description: Assemble the fish data on the grid
assembleGrid <- function(grid, sf) {
  grid_sf <- sf::st_join(grid, sf, join = st_contains_properly, left = TRUE) # join with the grid data if the centroid is contained within the grid
  
  centroid <- grid_sf %>% 
    sf::st_centroid() %>% # Convert to points
    sf::st_transform(crs = lonlat) %>%  # Transform back to lonlat
    dplyr::distinct(cellID, .keep_all = TRUE) # Select only distinct cellIDs, we're only interested in the geographic locations and not the seasons
  
  centroid_coordinates <- sf::st_coordinates(centroid) # Get coordinates
  
  grid_sf %<>% dplyr::mutate(longitude = ifelse(!is.na(longitude), 
                                                yes = longitude, 
                                                no = centroid_coordinates[cellID, "X"]),
                             latitude = ifelse(!is.na(latitude), 
                                               yes = latitude, 
                                               no = centroid_coordinates[cellID, "Y"])) %>% 
    dplyr::mutate(longitude = ifelse(longitude < 0, yes = longitude + 360, no = longitude)) %>%  # convert longitude to 0-360
    dplyr::as_tibble()
  
  return(grid_sf)
}
