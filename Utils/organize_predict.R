# Description: Organizing data set for predicting
organize_predict <- function(x) {
  x %<>%
    dplyr::filter(is.na(abundance)) %>% 
    organize_build()
}

# TODO: Get rid of this once I get annual values from cdo
organize_predict_tentative <- function(x) {
  x %<>%
    dplyr::filter(is.na(species)) %>% 
    organize_build()
}