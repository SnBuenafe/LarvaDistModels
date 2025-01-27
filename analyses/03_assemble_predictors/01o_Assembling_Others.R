# DESCRIPTION: Preparing bathymetry and distance to coast layers

# Load packages
# install.packages("pacman")
pacman::p_load(cmocean, here)

# Define directories
figure_dir <- here("figures", "supplementary")
output_dir <- here("data_input")
preliminaries_dir <- here("analyses", "02_preliminaries")

# Load preliminaries
source(here(preliminaries_dir, "00_SetupGrid.R"))
source("functions/gebcoConvert.R")
source("functions/calculateDist2Coast.R")
source("functions/change_gglayout.R")


# Bathymetry layer --------------------------------------------------------

# bathy <- gebcoConvert(grid) # bathymetry data is extrapolated depending on the grid area provided
bathy <- readRDS(here(output_dir, "gebco.rds"))

ggBathy <- ggplot() +
  geom_sf(data = bathy, aes(fill = meanDepth/1000), color = NA, size = 0.2) + # change the units from m to km
  scale_fill_cmocean(name = "ice",
                     #   alpha = 1,
                     aesthetics = c("fill"),
                     direction = 1,
                     limits = c(NA, NA),
                     oob = scales::squish,
                     na.value = "grey64",
                     guide = guide_colourbar(
                       title.vjust = 0.5,
                       barheight = grid::unit(0.035, "npc"),
                       barwidth = grid::unit(0.5, "npc"),
                       frame.colour = "black")) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('Mean depth (km)')) +
  change_gglayout()

ggsave(plot = ggBathy, filename = here(figure_dir, "mean_depth.png"), width = 14, height = 5, dpi = 600)


# Coastline layer ---------------------------------------------------------

# dist2coast <- calculateDist2Coast(grid) # distance to coast is calculated depending on the grid area provided
dist2coast <- readRDS(here(output_dir, "coast_distance.rds"))

ggCoast <- ggplot() +
  geom_sf(data = dist2coast, aes(fill = coastDistance/1000), color = NA, size = 0.2) + # converting from m to km
  scale_fill_cmocean(name = "deep",
                      #   alpha = 1,
                      aesthetics = c("fill"),
                      direction = -1,
                     limits = c(NA, NA),
                     oob = scales::squish,
                      na.value = "grey64",
                      guide = guide_colourbar(
                        title.vjust = 0.5,
                        barheight = grid::unit(0.035, "npc"),
                        barwidth = grid::unit(0.4, "npc"),
                        frame.colour = "black")) +
  geom_sf(data = landmass, fill = "white", color = "white") +
  labs(fill = expression('Distance to the nearest coastline (km)')) +
  change_gglayout()

ggsave(plot = ggCoast, filename = here(figure_dir, "distance_to_coast.png"), width = 14, height = 5, dpi = 600)
