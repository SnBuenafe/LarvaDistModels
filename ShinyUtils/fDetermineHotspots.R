fDetermineHotspots <- function(df,
                               spp,
                               season
) {
  
  spec <- base::tolower(spp)
  
  full <- df %>% 
    dplyr::as_tibble()
  
  dum_list <- list()
  
  for(i in 1:length(spec)) {
    dum_list[[i]] <- full %>% 
      dplyr::mutate(!!sym(spec[i]) := case_when(!!sym(spec[i]) >= median(full[[spec[i]]], na.rm = TRUE) ~ 1, 
                                                !!sym(spec[i]) < median(full[[spec[i]]], na.rm = TRUE) ~ 0,
                                                is.na(!!sym(spec[i])) ~ NA)) %>% 
      dplyr::select(cellID, grid_100_category, geometry, !!sym(spec[i]))
  }
  
  bind_df <- purrr::reduce(dum_list, dplyr::left_join) %>% 
    sf::st_as_sf(crs = cCRS) %>% 
    rowwise() %>% 
    dplyr::mutate(sum = sum(c_across(4:ncol(.)), na.rm = TRUE)) %>% 
    dplyr::mutate(count = sum(is.na(c_across(4:ncol(.))))) %>% # count the number of NAs (should be the same for all species)
    dplyr::mutate(count = ifelse(count > 0, yes = 1, no = 0)) %>% # count number of NAs
    dplyr::select(cellID, grid_100_category, sum, count, geometry) %>% 
    dplyr::rename(!!sym(season) := sum) %>% 
    dplyr::rename(!!sym(paste("count", season, sep = "_")) := count) %>% 
    ungroup() %>% 
    dplyr::as_tibble()
}