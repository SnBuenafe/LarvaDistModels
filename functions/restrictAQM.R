# DESCRIPTION: Restricting the model outputs to the adult's species' AquaMaps ranges

restrictAQM <- function(mdl, sp){
  
  range_limits <- readRDS(file.path("data_output", "RangeLimits.rds")) %>% 
    filter(Species == sp)
  
  mdl_sp <- mdl %>% 
    mutate(Longitude = sf::st_centroid(.) %>% sf::st_coordinates(.) %>% as.data.frame() %>% pull("X"),
           Latitude = sf::st_centroid(.) %>% sf::st_coordinates(.)%>% as.data.frame() %>% pull("Y"))
  
  
  # ggplot() + geom_sf(data = mdl_sp, aes(fill = model, colour = model))
  # ggplot() + geom_sf(data = m2, aes(fill = model, colour = model))
  
  
  if (range_limits$MaxLon < 0){
    mdl_sp <- mdl_sp %>% 
      dplyr::filter(
        (Longitude >= ((180-range_limits$MinLon)* -1) & 
           Longitude <= 180 + range_limits$MaxLon) &
          (Latitude >= range_limits$MinLat & Latitude <= range_limits$MaxLat)) %>% 
      dplyr::select(-c("Longitude", "Latitude"))
  } else {# When the data is only in the Eastern Hemisphere
    mdl_sp <- mdl_sp %>% 
      dplyr::filter(
        ((Longitude >= ((180-range_limits$MinLon)* -1))
         & Longitude <= 180-range_limits$MaxLon) &
          (Latitude >= range_limits$MinLat & Latitude <= range_limits$MaxLat)) %>% 
      dplyr::select(-c("Longitude", "Latitude"))
  }
  
  # ggplot() + geom_sf(data = mdl_sp, aes(fill = model, colour = model))
  return(mdl_sp)
  
}
