# Description: Preparing and assembling data frames for PCA

prepare_components <- function(obj, name) {
  
  fin <- obj %>% 
    dplyr::as_tibble() %>% 
    dplyr::select(cellID, grid_100_category, model, geometry) %>% 
    dplyr::rename(!!sym(name) := model)
  
}
