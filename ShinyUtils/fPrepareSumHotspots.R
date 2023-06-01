fPrepareSumHotspots <- function(jm, # Jan-March heatmap
                                aj, # Apr-June heatmap
                                js, # July-September heatmap
                                od # October-December heatmap
) {
  
  # Take the union of grid_100 cells that were selected at least once
  rep <- list(jm, aj, js, od)
  filt <- list() # empty list
  
  for(i in 1:length(rep)) {
    filt[[i]] <- rep[[i]] %>% 
      dplyr::select(grid_100_category) %>% 
      unique()
  }
  
  filt <- purrr::reduce(filt, dplyr::bind_rows) %>% 
    unique() %>% 
    pull()
  
  seas_list <- c("jan-mar", "apr-jun", "jul-sept", "oct-dec")
  new_names <- c("jm", "aj", "js", "od")
  
  df <- purrr::reduce(rep, dplyr::full_join) %>% 
    dplyr::relocate(geometry, .after = last_col()) %>% 
    dplyr::rename_with(~ new_names, all_of(seas_list)) %>% 
    dplyr::mutate(count = rowSums(across(starts_with("count_")))) %>% 
    dplyr::mutate(jm = ifelse(jm == 0, 1, jm),
                  aj = ifelse(aj == 0, 1, aj),
                  js = ifelse(js == 0, 1, js),
                  od = ifelse(od == 0, 1, od)) %>%  # change all 0s to 1s
    dplyr::rowwise() %>% 
    dplyr::mutate(gm = ifelse(count == 4, yes = NA,
                              no = (jm * aj * js * od)^(1/(4-count)))) %>%  # get the geometric mean only for cells where there are values
    dplyr::select(cellID, grid_100_category, gm, all_of(new_names), geometry) %>% 
    dplyr::filter(grid_100_category %in% filt) # make sure we only have cells that were filtered at least in one season
  
  quant <- quantile(df$gm, c(0.5, 0.8, 0.95), na.rm = TRUE) # get quantiles for categories
  
  df %<>%
    dplyr::mutate(hotspot_cat = case_when(gm <= quant[[1]] ~ "low",
                                          (gm > quant[[1]] & gm <= quant[[2]]) ~ "mid",
                                          (gm > quant[[2]] & gm <= quant[[3]]) ~ "high",
                                          (gm > quant[[3]] ~ "core"))) %>% # assign hotspot categories
    dplyr::mutate(hotspot_cat = fct_relevel(hotspot_cat, c("low", "mid", "high", "core"))) %>%
    sf::st_as_sf(crs = cCRS)
  
  return(df)
}