# DESCRIPTION: Creating seasonal pH layers

# Load preliminaries
source("00_Utils.R")

# i. January-March
phos_rs <- stars::read_ncdf("Data/Climatology/ensemble/phos_historical_1956_1981_jan-mar_ensemble.nc") %>% 
  terra::rast()
names(phos_rs) <- paste0("X", seq(1956, 1981, by = 1))
phos <- rs2sf(phos_rs) %>% 
  dplyr::rename(phos = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "phos") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, phos_transformed, geometry)
saveRDS(phos, "Data/Climatology/sf/phos_historical_jan-mar_interpolated.rds")

# ii. April-June
phos_rs <- stars::read_ncdf("Data/Climatology/ensemble/phos_historical_1956_1981_apr-jun_ensemble.nc") %>% 
  terra::rast()
names(phos_rs) <- paste0("X", seq(1956, 1981, by = 1))
phos <- rs2sf(phos_rs) %>% 
  dplyr::rename(phos = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "phos") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, phos_transformed, geometry)
saveRDS(phos, "Data/Climatology/sf/phos_historical_apr-jun_interpolated.rds")

# iii. July-September
phos_rs <- stars::read_ncdf("Data/Climatology/ensemble/phos_historical_1956_1981_jul-sept_ensemble.nc") %>% 
  terra::rast()
names(phos_rs) <- paste0("X", seq(1956, 1981, by = 1))
phos <- rs2sf(phos_rs) %>% 
  dplyr::rename(phos = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "phos") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, phos_transformed, geometry)
saveRDS(phos, "Data/Climatology/sf/phos_historical_jul-sept_interpolated.rds")

# iv. October-December
phos_rs <- stars::read_ncdf("Data/Climatology/ensemble/phos_historical_1956_1981_oct-dec_ensemble.nc") %>% 
  terra::rast()
names(phos_rs) <- paste0("X", seq(1956, 1981, by = 1))
phos <- rs2sf(phos_rs) %>% 
  dplyr::rename(phos = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "phos") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, phos_transformed, geometry)
saveRDS(phos, "Data/Climatology/sf/phos_historical_oct-dec_interpolated.rds")

#### Plotting ####

# January-March
dataPH <- readRDS("Data/Climatology/sf/phos_historical_jan-mar_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

ph1 <- ggplot() +
  geom_sf(data = dataPH, aes(fill = phos_transformed), color = NA, size = 0.01) +
  scale_fill_gradientn(colors = brewer.pal(9, "RdPu"),
                       na.value = "grey64",
                       guide = guide_colourbar(
                         title.vjust = 0.5,
                         barheight = grid::unit(0.01, "npc"),
                         barwidth = grid::unit(0.25, "npc"),
                         frame.colour = "black")) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('pH')) +
  theme_bw() +
  gg_add_text()

ggsave(plot = ph1, filename = "Figures/global_historical_ph_jan-mar.png", width = 15, height = 8, dpi = 300)

# April-June
dataPH <- readRDS("Data/Climatology/sf/phos_historical_apr-jun_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

ph2 <- ggplot() +
  geom_sf(data = dataPH, aes(fill = phos_transformed), color = NA, size = 0.01) +
  scale_fill_gradientn(colors = brewer.pal(9, "RdPu"),
                       na.value = "grey64",
                       guide = guide_colourbar(
                         title.vjust = 0.5,
                         barheight = grid::unit(0.01, "npc"),
                         barwidth = grid::unit(0.25, "npc"),
                         frame.colour = "black")) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('pH')) +
  theme_bw() +
  gg_add_text()

ggsave(plot = ph2, filename = "Figures/global_historical_ph_apr-jun.png", width = 15, height = 8, dpi = 300)

# July-September
dataPH <- readRDS("Data/Climatology/sf/phos_historical_jul-sept_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

ph3 <- ggplot() +
  geom_sf(data = dataPH, aes(fill = phos_transformed), color = NA, size = 0.01) +
  scale_fill_gradientn(colors = brewer.pal(9, "RdPu"),
                       na.value = "grey64",
                       guide = guide_colourbar(
                         title.vjust = 0.5,
                         barheight = grid::unit(0.01, "npc"),
                         barwidth = grid::unit(0.25, "npc"),
                         frame.colour = "black")) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('pH')) +
  theme_bw() +
  gg_add_text()

ggsave(plot = ph3, filename = "Figures/global_historical_ph_jul-sept.png", width = 15, height = 8, dpi = 300)

# October-December
dataPH <- readRDS("Data/Climatology/sf/phos_historical_oct-dec_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

ph4 <- ggplot() +
  geom_sf(data = dataPH, aes(fill = phos_transformed), color = NA, size = 0.01) +
  scale_fill_gradientn(colors = brewer.pal(9, "RdPu"),
                       na.value = "grey64",
                       guide = guide_colourbar(
                         title.vjust = 0.5,
                         barheight = grid::unit(0.01, "npc"),
                         barwidth = grid::unit(0.25, "npc"),
                         frame.colour = "black")) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('pH')) +
  theme_bw() +
  gg_add_text()

ggsave(plot = ph4, filename = "Figures/global_historical_ph_oct-dec.png", width = 15, height = 8, dpi = 300)

# Full pH plot
full_ph <- (ph1 + ph2) / (ph3 + ph4) +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = full_ph, filename = "Figures/global_historical_ph_full.png", width = 27, height = 15, dpi = 300)
