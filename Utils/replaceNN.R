# Description: Replacing NAs with the nearest neighborhood value
replaceNN <- function(climate, grid, colname) {
  filtered <- climate %>% 
    dplyr::filter(!is.na(!!sym(colname)))
  vector <- purrr::as_vector(sf::st_nearest_feature(grid, filtered))
  
  mutatedDF <- climate %>% 
    dplyr::mutate(!!sym(paste0(colname, "_transformed")) := ifelse(is.na(!!sym(colname)),
                                                                   yes = (filtered[[ colname ]])[vector[cellID]],
                                                                   no = !!sym(colname)))
}