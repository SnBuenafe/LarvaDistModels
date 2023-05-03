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
    coord_sf(xlim = st_bbox(grid)$xlim, ylim = st_bbox(grid)$ylim) +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.title = element_blank(),
          legend.text = element_text(size = 18, color = "black"),
          legend.title = element_text(size = 25, color = "black"),
          axis.text = element_text(size = 12, color = "black"),
          panel.border = element_rect(linewidth = 2, color = "black"),
          plot.margin = unit(c(0,0.5,0,0.5), "cm"))
  
  return(nish)
  
}
