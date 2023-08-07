# Description: Plotting PC values across the domain

plotPC1_limits <- function(df, axis, label) {
  gg <- ggplot() + 
    geom_sf(data = df, aes(fill = !!sym(axis), color = !!sym(axis)), size = 0.1) +
    scale_fill_gradientn(name = label,
                         aesthetics = c("fill", "color"),
                         colors = rev(brewer.pal(11, "RdBu")),
                         na.value = "grey64",
                         oob = scales::squish,
                         limits = c(-1, 20),
                         guide = guide_colourbar(
                           title.vjust = 0.5,
                           barheight = grid::unit(0.035, "npc"),
                           barwidth = grid::unit(0.5, "npc"),
                           frame.colour = "black")) +
    geom_sf(data = landmass, fill = "black", color = NA, size = 0.01) +
    change_gglayout()
}

plotPC2_limits <- function(df, axis, label) {
  gg <- ggplot() + 
    geom_sf(data = df, aes(fill = !!sym(axis), color = !!sym(axis)), size = 0.1) +
    scale_fill_gradientn(name = label,
                         aesthetics = c("fill", "color"),
                         colors = rev(brewer.pal(11, "RdBu")),
                         na.value = "grey64",
                         oob = scales::squish,
                         limits = c(-5, 10),
                         guide = guide_colourbar(
                           title.vjust = 0.5,
                           barheight = grid::unit(0.035, "npc"),
                           barwidth = grid::unit(0.5, "npc"),
                           frame.colour = "black")) +
    geom_sf(data = landmass, fill = "black", color = NA, size = 0.01) +
    change_gglayout()
}
