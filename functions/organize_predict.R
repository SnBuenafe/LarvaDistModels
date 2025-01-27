# Description: Organizing data set for predicting
organize_predict <- function(x) {
  x %<>%
    dplyr::filter(is.na(abundance)) %>% 
    organize_build()
}