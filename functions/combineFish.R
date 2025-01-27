# Description: Combine seasonal data of species into one sf object
combineFish <- function(species) {
  
  # path <- here::here("data_input", "fish")
  # list <- list.files(path)
  # param <- c(species, ".rds")
  # x <- apply(outer(list, param, stringr::str_detect), 1, all) %>% as.numeric()
  # file <- which(x == 1)
  # 
  # df <- list()
  # for(i in 1:length(file)) {
  #   df[[i]] <- readRDS(file.path(path, list[file[i]])) %>% 
  #     tibble::as_tibble()
  # 
  # combined <- do.call(bind_rows, df) %>% 
  
  
  path <- here::here("data_input", "fish")
  list <- list.files(path, pattern = species, full.names = TRUE)
  
  combined <- purrr::map(list, readRDS) %>% 
    bind_rows() %>% 
    dplyr::mutate(
      season = dplyr::case_match(
        season,
        "jul-sept" ~ "jul-sep", 
        .default = season),
      across(where(is.character), ~factor(.)) #, # change all character columns in  to factors
      #abundance = ifelse(abundance > 0, yes = 1, no = 0)
    ) %>% # convert data into binary
    dplyr::select(species, abundance, everything())
  
  sf <- combined %>% # convert into sf
    sf::st_as_sf(sf_column_name = "geometry")
  
  return(sf)
  
}