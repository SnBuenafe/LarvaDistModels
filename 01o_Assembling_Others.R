# DESCRIPTION: Preparing bathymetry and distance to coast layers

# Load preliminaries
source("00_SetupGrid.R")
source("Utils/gebcoConvert.R")
source("Utils/calculateDist2Coast.R")
figure_dir <- here::here("Figures", "predictors")
output_dir <- here::here("Data")

# Load packages
# install.packages("pacman")
pacman::p_load(cmocean)

# Bathymetry
bathy <- gebcoConvert(grid) # bathymetry data is extrapolated depending on the grid area provided
# bathy <- readRDS(here::here(output_dir, "gebco.rds"))

ggBathy <- ggplot() +
  geom_sf(data = bathy, aes(fill = meanDepth/1000), color = NA, size = 0.2) +
  scale_fill_cmocean(name = "ice",
                     #   alpha = 1,
                     aesthetics = c("fill"),
                     direction = 1,
                     limits = c(NA, NA),
                     oob = scales::squish,
                     na.value = "grey64",
                     guide = guide_colourbar(
                       title.vjust = 0.5,
                       barheight = grid::unit(0.01, "npc"),
                       barwidth = grid::unit(0.25, "npc"),
                       frame.colour = "black")) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('Mean depth (km)')) +
  theme_bw() +
  theme(plot.title = element_text(size = 28, color = "black"),
        axis.title = element_blank(),
        legend.text = element_text(size = 22, color = "black"),
        legend.title = element_text(size = 28, color = "black"),
        legend.position = "bottom",
        axis.text = element_text(size = 20, color = "black"),
        panel.border = element_rect(linewidth = 2, color = "black"),
        plot.margin = unit(c(0,0.5,0,0.5), "cm")) +
  coord_sf(xlim = st_bbox(grid)$xlim, ylim = st_bbox(grid)$ylim)

ggsave(plot = ggBathy, filename = here::here(figure_dir, "PredictorPlots_MeanDepth.png"), width = 15, height = 8, dpi = 300)

# Coastline
dist2coast <- calculateDist2Coast(grid) # distance to coast is calculated depending on the grid area provided
# dist2coast <- readRDS(here::here(output_dir, "CoastDistance.rds"))

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
                     limits = c(NA, NA),
                     oob = scales::squish,
                      na.value = "grey64",
                      guide = guide_colourbar(
                        title.vjust = 0.5,
                        barheight = grid::unit(0.01, "npc"),
                        barwidth = grid::unit(0.25, "npc"),
                        frame.colour = "black")) +
  geom_sf(data = landmass, fill = "white", color = "white") +
  labs(fill = expression('Distance to the nearest coastline (km)')) +
  theme_bw() +
  theme(plot.title = element_text(size = 28, color = "black"),
        axis.title = element_blank(),
        legend.text = element_text(size = 22, color = "black"),
        legend.title = element_text(size = 28, color = "black"),
        legend.position = "bottom",
        axis.text = element_text(size = 20, color = "black"),
        panel.border = element_rect(linewidth = 2, color = "black"),
        plot.margin = unit(c(0,0.5,0,0.5), "cm")) +
  coord_sf(xlim = st_bbox(grid)$xlim, ylim = st_bbox(grid)$ylim)

ggsave(plot = ggCoast, filename = here::here(figure_dir, "PredictorPlots_Distance2Coastline.png"), width = 15, height = 8, dpi = 300)
