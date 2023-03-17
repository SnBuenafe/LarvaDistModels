# Load preliminaries
source("00_Preliminaries.R")
figure_dir <- here::here("Figures")

# Bathymetry
bathy <- gebcoConvert(grid, 10000) # bathymetry data is extrapolated depending on the grid area provided

ggBathy <- ggplot() +
  geom_sf(data = bathy, aes(fill = meanDepth), color = NA, size = 0.2) +
  scale_fill_cmocean(name = "ice",
                     #   alpha = 1,
                     aesthetics = c("fill"),
                     direction = 1,
                     na.value = "grey64",
                     guide = guide_colourbar(
                       title.vjust = 0.5,
                       barheight = grid::unit(0.01, "npc"),
                       barwidth = grid::unit(0.25, "npc"),
                       frame.colour = "black")) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('m')) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 18),
        panel.border = element_blank()) +
  coord_sf(xlim = st_bbox(grid)$xlim, ylim = st_bbox(grid)$ylim)

ggsave(plot = ggBathy, filename = here::here(figure_dir, "global_bathy.png"), width = 15, height = 8, dpi = 300)

# Coastline
dist2coast <- calculateDist2Coast(grid) # distance to coast is calculated depending on the grid area provided

dataCoast <- dist2coast %>% 
  dplyr::as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  dplyr::left_join(., grid) %>% 
  sf::st_as_sf(crs = cCRS)

ggCoast <- ggplot() +
  geom_sf(data = dataCoast, aes(fill = coastDistance/1000), color = NA, size = 0.2) +
  scale_fill_cmocean(name = "deep",
                      #   alpha = 1,
                      aesthetics = c("fill"),
                      direction = -1,
                      na.value = "grey64",
                      guide = guide_colourbar(
                        title.vjust = 0.5,
                        barheight = grid::unit(0.01, "npc"),
                        barwidth = grid::unit(0.25, "npc"),
                        frame.colour = "black")) +
  geom_sf(data = landmass, fill = "white", color = "white") +
  labs(fill = expression('km')) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 18),
        panel.border = element_blank()) +
  coord_sf(xlim = st_bbox(grid)$xlim, ylim = st_bbox(grid)$ylim)

ggsave(plot = ggCoast, filename = here::here(figure_dir, "global_coast.png"), width = 15, height = 8, dpi = 300)
