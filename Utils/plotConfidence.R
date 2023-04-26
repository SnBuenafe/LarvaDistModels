# DESCRIPTION: Add hatches over areas of lower confidence.

plotConfidence <- function(gg_filt, # filtered map
                           assoc_grid # associated grid
) {
  filt_cells <- gg_filt$grid_100_category # get cellIDs of filtered cells
  
  filt_100 <- assoc_grid %>% 
    dplyr::filter(grid_100_category %in% filt_cells) %>% 
    sf::st_as_sf() %>% 
    sf::st_union()
  
  plot <- plotModel(gg_filt) +
    geom_sf(data = filt_100, color = "black", size = 0.01, fill = NA) +
  # geom_sf_pattern(data = filt_100,
  #                 pattern = "stripe",
  #                 pattern_fill = "grey55",
  #                 pattern_color = "grey55",
  #                 fill = NA,
  #                 color = "grey55",
  #                 size = 0.1,
  #                 pattern_spacing = 0.03) +
    coord_sf(xlim = st_bbox(grid)$xlim, ylim = st_bbox(grid)$ylim) 
  
  return(plot)
}
