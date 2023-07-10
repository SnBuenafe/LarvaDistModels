# Restrict area of seasonal distribution map to 10x10 grid cells containing 1x1 sampling points.

restrictArea <- function(seas_map, # seasonal map
                         assoc_grid # 1x1 sf object with associated 10x10 grid categories
) {
  new_grid <- assoc_grid %>% 
    dplyr::filter(!is.na(abundance)) %>% 
    sf::st_as_sf()
  
  grid100_samppoint <- new_grid$grid_100_category # Get grid_100 categories that have sampling points in it
  
  # Associate model data with grid100 categories
  seas_filt <- seas_map %>% 
    dplyr::as_tibble() %>% 
    dplyr::left_join(., assoc_grid) %>% 
    dplyr::select(cellID, ocean, model, grid_100_category, geometry) %>% 
    dplyr::filter(grid_100_category %in% grid100_samppoint) %>% 
    sf::st_as_sf()
  
  return(seas_filt)
}