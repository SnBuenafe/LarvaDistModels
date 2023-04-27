# Description: Preparing and assembling data frames for PCA

prepare_components <- function(obj) {
  
  fin <- obj %>% 
    dplyr::as_tibble() %>% 
    dplyr::select(model) %>% 
    pull()
  
}