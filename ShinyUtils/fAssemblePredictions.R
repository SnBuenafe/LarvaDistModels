fAssemblePredictions <- function(spp, # vector of species
                                 season # chosen season
) {
  
  full <- list()
  
  for(i in 1:length(spp)) {
    full[[i]] <- readRDS(here::here(preds_dir, base::paste0(spp[i], "_", season, ".rds"))) %>% 
      dplyr::as_tibble() %>% 
      dplyr::select(cellID, grid_100_category, model, geometry) %>% 
      dplyr::rename(!!sym(tolower(spp[i])) := model)
  }
  
  df <- purrr::reduce(full, 
                      dplyr::left_join, 
                      by = c('cellID', 'grid_100_category', 'geometry')) %>% # join all
    dplyr::relocate(cellID) %>%
    dplyr::relocate(grid_100_category, .after = cellID) %>% 
    dplyr::relocate(geometry, .after = last_col()) # arrange columns
  
  return(df)
  
}