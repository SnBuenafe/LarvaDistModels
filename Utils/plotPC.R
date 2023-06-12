# Description: Plotting PC values across the domain

plotPC <- function(df, axis, label) {
  gg <- ggplot() + 
    geom_sf(data = df, aes(fill = !!sym(axis), color = !!sym(axis)), size = 0.1) +
    scale_fill_gradientn(name = label,
                         aesthetics = c("fill", "color"),
                         colors = rev(brewer.pal(11, "RdBu")),
                         na.value = "grey64",
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
    coord_sf(xlim = st_bbox(grid)$xlim, ylim = st_bbox(grid)$ylim) 
}

plotPC1_limits <- function(df, axis, label) {
  gg <- ggplot() + 
    geom_sf(data = df, aes(fill = !!sym(axis), color = !!sym(axis)), size = 0.1) +
    scale_fill_gradientn(name = label,
                         aesthetics = c("fill", "color"),
                         colors = rev(brewer.pal(11, "RdBu")),
                         na.value = "grey64",
                         oob = scales::squish,
                         limits = c(-0.15, 1.75),
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
          legend.text = element_text(size = 30, color = "black"),
          legend.title = element_text(size = 30, color = "black"),
          axis.text = element_text(size = 25, color = "black"),
          panel.border = element_rect(linewidth = 2, color = "black")) +
    coord_sf(xlim = st_bbox(grid)$xlim, ylim = st_bbox(grid)$ylim) 
}

plotPC2_limits <- function(df, axis, label) {
  gg <- ggplot() + 
    geom_sf(data = df, aes(fill = !!sym(axis), color = !!sym(axis)), size = 0.1) +
    scale_fill_gradientn(name = label,
                         aesthetics = c("fill", "color"),
                         colors = rev(brewer.pal(11, "RdBu")),
                         na.value = "grey64",
                         oob = scales::squish,
                         limits = c(-0.5, 1),
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
          legend.text = element_text(size = 30, color = "black"),
          legend.title = element_text(size = 30, color = "black"),
          axis.text = element_text(size = 25, color = "black"),
          panel.border = element_rect(linewidth = 2, color = "black")) +
    coord_sf(xlim = st_bbox(grid)$xlim, ylim = st_bbox(grid)$ylim) 
}
