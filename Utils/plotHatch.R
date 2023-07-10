# Description: Add hatch to areas that are unlikely to be habitats for adults

plotHatch <- function(gg,
                      sf, # seasonal species sf object
                      df # seasonal species data.frame
) {
  x <- sf %>%
    restrict_adult(df, .) %>%
    dplyr::filter(adult_cat == 0) %>%
    sf::st_union()
  
  plot <- gg +
    geom_sf_pattern(data = x,
                    pattern = "stripe",
                    pattern_fill = "grey55",
                    pattern_color = "grey55",
                    fill = NA,
                    color = "grey55",
                    size = 0.1,
                    pattern_size = 0.05) +
    coord_sf(xlim = st_bbox(grid)$xlim, ylim = st_bbox(grid)$ylim) 
}