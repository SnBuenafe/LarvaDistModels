# Restrict remaining area to just 10x10 grid cells that have sampling points within a certain threshold

restrictThreshold <- function(area, # sampling area that we want to restrict
                              seas_map, # seasonal map
                              threshold # in %
                              ) {
  
  filt_ngrid <- area %>% 
    dplyr::as_tibble() %>% 
    dplyr::mutate(samp_point = ifelse(!is.na(abundance), yes = 1, no = abundance)) %>% # counting sampling points
    dplyr::group_by(grid_100_category) %>% 
    dplyr::mutate(samp_point = sum(samp_point, na.rm = TRUE), count = n()) %>% # get number of sampling points per 10x10 grid and number of 1x1 ocean cells in that 10x10 grid
    dplyr::mutate(grid_1_perc = samp_point*100/count) %>% # find out how much of each 10x10 grid cell has 1x1 sampling points
    ungroup()
    
  
  filt_ID <- filt_ngrid %>% 
    dplyr::filter(grid_1_perc > threshold) %>% 
    dplyr::select(grid_100_category) %>% 
    unique() %>% 
    pull()
  
  gg_filt <- seas_map %>% 
    dplyr::as_tibble() %>% 
    dplyr::left_join(., area) %>% 
    dplyr::select(cellID, ocean, model, grid_100_category, geometry) %>% 
    dplyr::filter(grid_100_category %in% filt_ID) %>% 
    sf::st_as_sf()
  
  return(gg_filt)
  
}
