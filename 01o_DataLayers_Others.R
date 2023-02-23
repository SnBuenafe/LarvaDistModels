# Load preliminaries
source("00_Utils.R")

# Bathymetry
bathy <- gebcoConvert(grid, 2500) # bathymetry data is extrapolated depending on the grid area provided

# Coastline
dist2coast <- calculateDist2Coast(grid) # distance to coast is calculated depending on the grid area provided

#### Plotting ####

# Distance to coast
dataCoast <- readRDS("Data/CoastDistance.rds") %>% 
  crop_predictor()

ggCoast <- ggplot() +
  geom_sf(data = dataCoast, aes(color = coastDistance/1000), size = 0.2) +
  scale_color_cmocean(name = "deep",
                      #   alpha = 1,
                      aesthetics = c("color"),
                      direction = -1,
                      na.value = "grey64",
                      guide = guide_colourbar(
                        title.vjust = 0.5,
                        barheight = grid::unit(0.01, "npc"),
                        barwidth = grid::unit(0.25, "npc"),
                        frame.colour = "black")) +
  geom_sf(data = landmass, fill = "white", color = "white") +
  labs(color = expression('km')) +
  theme_bw() +
  gg_add_text(., "white")

ggsave(plot = ggCoast, filename = "Figures/global_coast.png", width = 15, height = 8, dpi = 300)

# Bathymetry
dataBathy <- readRDS("Data/GEBCO/gebco2500.rds") %>% 
  crop_predictor()

ggBathy <- ggplot() +
  geom_sf(data = dataBathy, aes(fill = meanDepth), color = NA, size = 0.2) +
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
  gg_add_text(., "white")

ggsave(plot = ggBathy, filename = "Figures/global_bathy.png", width = 15, height = 8, dpi = 300)
