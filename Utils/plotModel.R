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
                         barwidth = grid::unit(0.2, "npc"),
                         frame.colour = "black")) +
    geom_sf(data = landmass, fill = "black", color = NA, size = 0.01) +
    xlab("Longitude") +
    ylab("Latitude") +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.title = element_blank(),
          legend.text = element_text(size = 18, color = "black"),
          legend.title = element_text(size = 25, color = "black"),
          axis.text = element_text(size = 12, color = "black"),
          panel.border = element_rect(linewidth = 2, color = "black"),
          plot.margin = unit(c(0,0.5,0,0.5), "cm")) +
    coord_sf(xlim = st_bbox(grid)$xlim, ylim = st_bbox(grid)$ylim) 
  
  
  return(ggmodel)
}
