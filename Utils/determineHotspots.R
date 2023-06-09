# DESCRIPTION: Get the sum of spp per grid cell

determineHotspots <- function(season) {
  full <- readr::read_csv(here::here(preds_dir, paste0("FULL_predictions_", season, ".csv"))) %>% # load predictions
    dplyr::select(-1)  # delete empty column?
  
  dum_list <- list()
  
  for(i in 1:length(spp)) {
    dum_list[[i]] <- full %>% 
      dplyr::mutate(!!sym(spp[i]) := case_when(!!sym(spp[i]) >= median(full[[spp[i]]], na.rm = TRUE) ~ 1, 
                                               !!sym(spp[i]) < median(full[[spp[i]]], na.rm = TRUE) ~ 0,
                                               is.na(!!sym(spp[i])) ~ NA)) %>% 
      dplyr::select(cellID, !!sym(spp[i]))
  }
  
  bind_df <- purrr::reduce(dum_list, dplyr::left_join) %>% 
    dplyr::left_join(grid, ., by = "cellID") %>% 
    dplyr::as_tibble() %>% 
    dplyr::mutate(sum = rowSums(dplyr::across(tidyselect::all_of(spp)), na.rm = TRUE)) %>%
    dplyr::mutate(count = rowSums(is.na(dplyr::across(tidyselect::all_of(spp))))) %>% # count the number of NAs (should be the same for all species)
    dplyr::mutate(count = ifelse(count > 0, yes = 1, no = 0)) %>% # we just need to know if grid cell is an NA per season
    dplyr::mutate(count = ifelse(count > 0, yes = 1, no = 0)) %>% # count number of NAs
    dplyr::select(cellID, sum, count, geometry) %>% 
    dplyr::rename(!!sym(season) := sum,
                  !!sym(paste("count", season, sep = "_")) := count)
}
