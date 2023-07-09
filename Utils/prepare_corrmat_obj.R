# DESCRIPTION: Prepare the object that would feed into plotting the correlation matrix

prepare_corrmat_obj <- function(season, axis) {
  
  res <- read_csv(here::here(pc_dir, paste("hotspots", season, "loadings.csv", sep = "_"))) %>% 
    dplyr::rename(Species = `...1`) %>% 
    dplyr::select(Species, !!sym(axis)) %>% # just select species and principal component of interest
    dplyr::arrange(desc(!!sym(axis))) %>% # arrange in decreasing order of positive correlation
    dplyr::mutate(!!sym(axis) := as.numeric(!!sym(axis)))
  
  spp_str <- res[,1] %>% 
    pull() # get string of species
  
  res <- res[,2] %>%  # just get loadings
    as.matrix()
  
  idx <- match(toupper(spp_str), spec_dict$code) # get indices of match
  spp_str_new <- spec_dict[idx,] # rearrange the species
  
  rownames(res) <- spp_str_new[,2] %>% 
    pull()
  
  return(res)
  
}