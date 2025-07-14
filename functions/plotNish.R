# DESCRIPTION:Plot Nishikawa data

plotNish <- function(x) {
  
  df <- x %>% 
    dplyr::select(cellID, abundance, geometry) %>% 
    na.omit() %>% 
    dplyr::mutate(abundance_presence = ifelse(abundance > 0, yes = "Present", no = "Absent")) %>% 
    sf::st_as_sf() %>% 
    dplyr::select(cellID, abundance_presence, everything()) # arrange
  
  nish <- ggplot() +
    geom_sf(data = df, aes(fill = as.factor(abundance_presence), color = as.factor(abundance_presence))) +
    scale_discrete_manual(name = "",
                          values = c(`Present` = "#216E5E", `Absent` = "#EBE0D4"),
                          aesthetics = c("fill", "color")) +
    geom_sf(data = landmass, fill = "black", color = "black", size = 1) +
    change_gglayout()
  
  return(nish)
  
}
