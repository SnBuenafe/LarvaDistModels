fDetermineHotspots <- function(df,
                               spp,
                               season
) {
  
  spec <- base::tolower(spp)
  
  full <- df %>% 
    dplyr::as_tibble() %>% 
    dplyr::left_join(grid, .)
  
  dum_list <- list()
  
  for(i in 1:length(spec)) {
    dum_list[[i]] <- full %>% 
      dplyr::as_tibble() %>% 
      dplyr::mutate(!!sym(spec[i]) := case_when(!!sym(spec[i]) >= median(full[[spec[i]]], na.rm = TRUE) ~ 1, 
                                                !!sym(spec[i]) < median(full[[spec[i]]], na.rm = TRUE) ~ 0,
                                                is.na(!!sym(spec[i])) ~ NA)) %>% 
      dplyr::select(cellID, grid_100_category, geometry, !!sym(spec[i]))
  }
  
  bind_df <- purrr::reduce(dum_list, dplyr::left_join) %>% 
    dplyr::mutate(sum = rowSums(dplyr::across(tidyselect::all_of(spec)), na.rm = TRUE)) %>%
    dplyr::mutate(count = rowSums(is.na(dplyr::across(tidyselect::all_of(spec))))) %>% # count the number of NAs (should be the same for all species)
    dplyr::mutate(count = ifelse(count > 0, yes = 1, no = 0)) %>% # we just need to know if grid cell is an NA per season
    dplyr::select(cellID, grid_100_category, sum, count, geometry) %>% 
    dplyr::rename(!!sym(season) := sum,
                  !!sym(paste("count", season, sep = "_")) := count)
}