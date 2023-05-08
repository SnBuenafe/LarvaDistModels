# DESCRIPTION: Get the sum of spp per grid cell

determineHotspots <- function(season) {
  full <- read_csv(here::here(pred_dir, paste0("FULL_predictions_", season, ".csv"))) %>% # load predictions
    dplyr::left_join(., dummy) 
  
  dum_list <- list()
  
  for(i in 1:length(spp)) {
    dum_list[[i]] <- full %>% 
      dplyr::mutate(!!sym(spp[i]) := case_when(!!sym(spp[i]) >= median(df[[spp[i]]]) ~ 1, TRUE ~ 0)) %>% 
      dplyr::select(cellID, grid_100_category, geometry, !!sym(spp[i]))
  }
  
  bind_df <- purrr::reduce(dum_list, dplyr::left_join) %>% 
    sf::st_as_sf(crs = cCRS) %>% 
    rowwise() %>% 
    dplyr::mutate(sum = sum(c_across(4:ncol(.)))) %>% 
    dplyr::select(cellID, grid_100_category, sum, geometry) %>% 
    dplyr::rename(!!sym(season) := sum) %>% 
    ungroup() %>% 
    dplyr::as_tibble()
}
