# Description: Preparing and assembling data frames for PCA

prepare_components <- function(obj, name) {
  
  fin <- obj %>% 
    dplyr::as_tibble() %>% 
    dplyr::select(cellID, model) %>% 
    dplyr::rename(!!sym(name) := model)
  
}
