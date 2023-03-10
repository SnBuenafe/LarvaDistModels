# Description: Plot seasonal species distribution map

plotModel <- function(sf, # seasonal species sf object
                      df # seasonal species data.frame
                      ) {
  
  x <- sf %>%
    restrict_adult(df, .) %>%
    dplyr::filter(adult_cat == 0) %>%
    sf::st_union()
  
  ggmodel <- ggplot() + 
    geom_sf(data = sf, aes(fill = model),
            color = NA, size = 0.1) +
    scale_fill_cmocean("Probability ",
                       name = "ice",
                       direction = -1, 
                       limits = c(0, as.numeric(quantile(sf$model, 0.99))),
                       na.value = NA,
                       oob = scales::squish,
                       guide = guide_colourbar(
                         title.vjust = 0.5,
                         barheight = grid::unit(0.01, "npc"),
                         barwidth = grid::unit(0.25, "npc"),
                         frame.colour = "black")) +
    geom_sf(data = landmass, fill = "black", color = NA, size = 0.01) +
    xlab("Longitude") +
    ylab("Latitude") +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.title = element_blank(),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 18),
          panel.border = element_blank()) +
    geom_sf_pattern(data = x,
                    pattern = "stripe",
                    pattern_fill = "grey55",
                    pattern_color = "grey55",
                    fill = NA,
                    color = "grey55",
                    size = 0.1,
                    pattern_size = 0.05) +
    coord_sf(xlim = st_bbox(grid)$xlim, ylim = st_bbox(grid)$ylim) 
  
  
  return(ggmodel)
}