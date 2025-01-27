# Description: Organizing data set for building model
organize_build <- function(x) {
  x %<>%
    dplyr::mutate(across(where(is.character), ~factor(.))) %>% # convert all characters to factors
    dplyr::mutate(abundance_presence = case_when(abundance > 0 ~ 1,
                                                 abundance == 0 ~ 0), 
                  row = row_number()) %>%  # mutate the abundance data into 1s and 0s
    dplyr::select(-geometry) %>% 
    dplyr::select(row, cellID, species, abundance, abundance_presence, ocean, longitude, latitude, season, everything()) %>% # arrange columns
    as.data.frame() #gbm.step doesn't work if it's a tibble...
}