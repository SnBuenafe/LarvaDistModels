# DESCRIPTION: Plot heatmaps

plotHotspot <- function(df, season, label) {
  ggplot() + 
    geom_sf(data = df %>% 
              dplyr::filter(!!sym(paste("count", season, sep = "_")) == 0) %>% 
              sf::st_as_sf(crs = cCRS), 
            aes(fill = !!sym(season)), 
            color = NA, 
            size = 0.1) +
    scale_fill_cmocean(name = "thermal",
                       #   alpha = 1,
                       aesthetics = c("fill"),
                       direction = 1,
                       na.value = "grey64",
                       guide = guide_colourbar(
                         title = "# of taxa",
                         title.vjust = 0.5,
                         barheight = grid::unit(0.01, "npc"),
                         barwidth = grid::unit(0.25, "npc"),
                         frame.colour = "black")) +
    geom_sf(data = landmass, fill = "black", color = "black") +
    ggtitle(label) +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.title = element_blank(),
          legend.text = element_text(size = 18, color = "black"),
          legend.title = element_text(size = 25, color = "black"),
          axis.text = element_text(size = 12, color = "black"),
          panel.border = element_rect(linewidth = 2, color = "black"),
          plot.title = element_text(size = 25, color = "black"),
          plot.margin = unit(c(0,0.5,0,0.5), "cm")) +
    coord_sf(xlim = st_bbox(grid)$xlim, ylim = st_bbox(grid)$ylim)
}

plotHotspotSummary <- function(df) {
  ggplot() +
    geom_sf(data = df %>% 
              dplyr::filter(!is.na(hotspot_cat)), aes(fill = hotspot_cat), color = NA, size = 0.1) +
    scale_fill_manual(name = "Priority",
                      aesthetics = "fill",
                      values = c("#062843", "#5C3E9A", "#B36080", "#FAAF64")) +
    geom_sf(data = landmass, fill = "black", color = "black") +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.title = element_blank(),
          legend.text = element_text(size = 18, color = "black"),
          legend.title = element_text(size = 25, color = "black"),
          axis.text = element_text(size = 12, color = "black"),
          panel.border = element_rect(linewidth = 2, color = "black"),
          plot.margin = unit(c(0,0.5,0,0.5), "cm")) +
    coord_sf(xlim = st_bbox(grid)$xlim, ylim = st_bbox(grid)$ylim)
}
