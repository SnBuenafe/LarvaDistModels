# DESCRIPTION: Get the sum of species per grid cell

determineHotspots <- function(season, spp_list) {
  
  df <- list() # empty list
  for(i in 1:length(spp_list)) {
    obj <- readRDS(here::here(preds_dir, paste(toupper(spp_list[i]), paste0(season, ".rds"), sep = "_")))
    df[[i]] <- prepare_components(obj, spp_list[i])
  }
  
  full <- purrr::reduce(df, dplyr::left_join, by = c("cellID", "grid_100_category", "geometry"))
  
  dum_list <- list() # empty list
  for(i in 1:length(spp_list)) {
    dum_list[[i]] <- full %>% 
      dplyr::mutate(!!sym(spp_list[i]) := case_when(!!sym(spp_list[i]) >= mean(full[[spp_list[i]]], na.rm = TRUE) ~ 1, # probabilities that are >= mean are presences
                                               !!sym(spp_list[i]) < mean(full[[spp_list[i]]], na.rm = TRUE) ~ 0, # probabilities that are < mean are absences
                                               is.na(!!sym(spp_list[i])) ~ NA)) %>% 
      dplyr::select(cellID, !!sym(spp_list[i]))
  }
  
  bind_df <- purrr::reduce(dum_list, dplyr::left_join) %>% 
    dplyr::left_join(grid, ., by = "cellID") %>% 
    dplyr::as_tibble() %>% 
    dplyr::mutate(sum = rowSums(dplyr::across(tidyselect::all_of(spp_list)), na.rm = TRUE)) %>%
    dplyr::mutate(count = rowSums(is.na(dplyr::across(tidyselect::all_of(spp_list))))) %>% # count the number of NAs (should be the same for all species)
    dplyr::mutate(count = ifelse(count > 0, yes = 1, no = 0)) %>% # we just need to know if grid cell is an NA per season
    dplyr::select(cellID, sum, count, geometry) %>% 
    dplyr::rename(!!sym(season) := sum,
                  !!sym(paste("count", season, sep = "_")) := count)
  
  return(bind_df)
}
