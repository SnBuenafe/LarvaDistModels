# DESCRIPTION: Creating seasonal oxygen layers

# Load preliminaries
source("00_Utils.R")

# i. January-March
o2os_rs <- stars::read_ncdf("Data/Climatology/ensemble/o2os_historical_1956_1981_jan-mar_ensemble.nc") %>% 
  terra::rast()
names(o2os_rs) <- paste0("X", seq(1956, 1981, by = 1))
o2os <- rs2sf(o2os_rs) %>% 
  dplyr::rename(o2os = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "o2os") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, o2os_transformed, geometry)
saveRDS(o2os, "Data/Climatology/sf/o2os_historical_jan-mar_interpolated.rds")

# ii. April-June
o2os_rs <- stars::read_ncdf("Data/Climatology/ensemble/o2os_historical_1956_1981_apr-jun_ensemble.nc") %>% 
  terra::rast()
names(o2os_rs) <- paste0("X", seq(1956, 1981, by = 1))
o2os <- rs2sf(o2os_rs) %>% 
  dplyr::rename(o2os = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "o2os") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, o2os_transformed, geometry)
saveRDS(o2os, "Data/Climatology/sf/o2os_historical_apr-jun_interpolated.rds")

# iii. July-September
o2os_rs <- stars::read_ncdf("Data/Climatology/ensemble/o2os_historical_1956_1981_jul-sept_ensemble.nc") %>% 
  terra::rast()
names(o2os_rs) <- paste0("X", seq(1956, 1981, by = 1))
o2os <- rs2sf(o2os_rs) %>% 
  dplyr::rename(o2os = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "o2os") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, o2os_transformed, geometry)
saveRDS(o2os, "Data/Climatology/sf/o2os_historical_jul-sept_interpolated.rds")

# iv. October-December
o2os_rs <- stars::read_ncdf("Data/Climatology/ensemble/o2os_historical_1956_1981_oct-dec_ensemble.nc") %>% 
  terra::rast()
names(o2os_rs) <- paste0("X", seq(1956, 1981, by = 1))
o2os <- rs2sf(o2os_rs) %>% 
  dplyr::rename(o2os = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "o2os") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, o2os_transformed, geometry)
saveRDS(o2os, "Data/Climatology/sf/o2os_historical_oct-dec_interpolated.rds")

#### Plotting ####

# January-March
dataO2 <- readRDS("Data/Climatology/sf/o2os_historical_jan-mar_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

ox1 <- ggplot() +
  geom_sf(data = dataO2, aes(fill = o2os_transformed), color = NA, size = 0.01) +
  scale_fill_cmocean(name = "tempo",
                     #   alpha = 1,
                     aesthetics = c("fill"),
                     direction = -1,
                     na.value = "grey64",
                     guide = guide_colourbar(
                       title.vjust = 0.5,
                       barheight = grid::unit(0.01, "npc"),
                       barwidth = grid::unit(0.25, "npc"),
                       frame.colour = "black")) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('mol m'^"-3"*'')) +
  theme_bw() +
  gg_add_text(., "white")

ggsave(plot = ox1, filename = "Figures/global_historical_oxygen_jan-mar.png", width = 15, height = 8, dpi = 300)

# April-June
dataO2 <- readRDS("Data/Climatology/sf/o2os_historical_apr-jun_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

ox2 <- ggplot() +
  geom_sf(data = dataO2, aes(fill = o2os_transformed), color = NA, size = 0.01) +
  scale_fill_cmocean(name = "tempo",
                     #   alpha = 1,
                     aesthetics = c("fill"),
                     direction = -1,
                     na.value = "grey64",
                     guide = guide_colourbar(
                       title.vjust = 0.5,
                       barheight = grid::unit(0.01, "npc"),
                       barwidth = grid::unit(0.25, "npc"),
                       frame.colour = "black")) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('mol m'^"-3"*'')) +
  theme_bw() +
  gg_add_text(., "white")

ggsave(plot = ox2, filename = "Figures/global_historical_oxygen_apr-jun.png", width = 15, height = 8, dpi = 300)

# July-September
dataO2 <- readRDS("Data/Climatology/sf/o2os_historical_jul-sept_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

ox3 <- ggplot() +
  geom_sf(data = dataO2, aes(fill = o2os_transformed), color = NA, size = 0.01) +
  scale_fill_cmocean(name = "tempo",
                     #   alpha = 1,
                     aesthetics = c("fill"),
                     direction = -1,
                     na.value = "grey64",
                     guide = guide_colourbar(
                       title.vjust = 0.5,
                       barheight = grid::unit(0.01, "npc"),
                       barwidth = grid::unit(0.25, "npc"),
                       frame.colour = "black")) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('mol m'^"-3"*'')) +
  theme_bw() +
  gg_add_text(., "white")

ggsave(plot = ox3, filename = "Figures/global_historical_oxygen_jul-sept.png", width = 15, height = 8, dpi = 300)

# October-December
dataO2 <- readRDS("Data/Climatology/sf/o2os_historical_oct-dec_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

ox4 <- ggplot() +
  geom_sf(data = dataO2, aes(fill = o2os_transformed), color = NA, size = 0.01) +
  scale_fill_cmocean(name = "tempo",
                     #   alpha = 1,
                     aesthetics = c("fill"),
                     direction = -1,
                     na.value = "grey64",
                     guide = guide_colourbar(
                       title.vjust = 0.5,
                       barheight = grid::unit(0.01, "npc"),
                       barwidth = grid::unit(0.25, "npc"),
                       frame.colour = "black")) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('mol m'^"-3"*'')) +
  theme_bw() +
  gg_add_text(., "white")

ggsave(plot = ox4, filename = "Figures/global_historical_oxygen_oct-dec.png", width = 15, height = 8, dpi = 300)

# Full oxygen plot
full_ox <- (ox1 + ox2) / (ox3 + ox4) +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = full_ox, filename = "Figures/global_historical_oxygen_full.png", width = 27, height = 15, dpi = 300)
