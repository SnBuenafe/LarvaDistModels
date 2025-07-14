# Description: Plot seasonal species distribution map

plotModel <- function(sf, # seasonal species sf object
                      limits) {
  ggmodel <- ggplot() + 
    geom_sf(data = sf, aes(fill = model),
            color = NA, size = 0.1) +
    scale_fill_cmocean("Probability     ",
                       name = "ice",
                       direction = -1, 
                       limits = limits,
                       na.value = NA,
                       oob = scales::squish,
                       guide = guide_colourbar(
                         title.vjust = 0.5,
                         barheight = grid::unit(0.035, "npc"),
                         barwidth = grid::unit(0.5, "npc"),
                         frame.colour = "black")) +
    geom_sf(data = landmass, fill = "black", color = NA, size = 0.01) +
    xlab("Longitude") +
    ylab("Latitude") +
    change_gglayout()
  
  
  return(ggmodel)
}
