# DESCRIPTION: Plot heatmaps

plotHotspot <- function(df, season) {
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
                       limits = c(0, 14),
                       direction = 1,
                       na.value = "grey64",
                       guide = guide_colourbar(
                         title = "# of taxa",
                         title.vjust = 0.5,
                         barheight = grid::unit(0.035, "npc"),
                         barwidth = grid::unit(0.5, "npc"),
                         frame.colour = "black")) +
    geom_sf(data = landmass, fill = "black", color = "black") +
    change_gglayout()
}

plotHotspotSummary <- function(df) {
  ggplot() +
    geom_sf(data = df %>% 
              dplyr::filter(!is.na(hotspot_cat)), aes(fill = hotspot_cat), color = NA, size = 0.1) +
    scale_fill_manual(name = "Priority rank",
                      aesthetics = "fill",
                      values = c("#FAAF64", "#B36080", "#5C3E9A", "#062843")
                      ) +
    geom_sf(data = landmass, fill = "black", color = "black") +
    change_gglayout() +
    theme(legend.key.width = grid::unit(0.05, "npc"))
}
