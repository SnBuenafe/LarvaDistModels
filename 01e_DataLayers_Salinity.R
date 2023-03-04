# DESCRIPTION: Creating seasonal salinity layers

# Load preliminaries
source("00_Utils.R")

# i. January-March
sos_rs <- stars::read_ncdf("Data/Climatology/ensemble/sos_historical_1956_1981_jan-mar_ensemble.nc") %>% 
  terra::rast()
names(sos_rs) <- paste0("X", seq(1956, 1981, by = 1))
sos <- rs2sf(sos_rs) %>% 
  dplyr::rename(sos = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll_pacific) %>% 
  replaceNN(., grid, "sos") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, sos_transformed, geometry)
saveRDS(sos, "Data/Climatology/sf/sos_historical_jan-mar_interpolated.rds")

# ii. April-June
sos_rs <- stars::read_ncdf("Data/Climatology/ensemble/sos_historical_1956_1981_apr-jun_ensemble.nc") %>% 
  terra::rast()
names(sos_rs) <- paste0("X", seq(1956, 1981, by = 1))
sos <- rs2sf(sos_rs) %>% 
  dplyr::rename(sos = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll_pacific) %>% 
  replaceNN(., grid, "sos") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, sos_transformed, geometry)
saveRDS(sos, "Data/Climatology/sf/sos_historical_apr-jun_interpolated.rds")

# iii. July-September
sos_rs <- stars::read_ncdf("Data/Climatology/ensemble/sos_historical_1956_1981_jul-sept_ensemble.nc") %>% 
  terra::rast()
names(sos_rs) <- paste0("X", seq(1956, 1981, by = 1))
sos <- rs2sf(sos_rs) %>% 
  dplyr::rename(sos = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll_pacific) %>% 
  replaceNN(., grid, "sos") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, sos_transformed, geometry)
saveRDS(sos, "Data/Climatology/sf/sos_historical_jul-sept_interpolated.rds")

# iv. October-December
sos_rs <- stars::read_ncdf("Data/Climatology/ensemble/sos_historical_1956_1981_oct-dec_ensemble.nc") %>% 
  terra::rast()
names(sos_rs) <- paste0("X", seq(1956, 1981, by = 1))
sos <- rs2sf(sos_rs) %>% 
  dplyr::rename(sos = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll_pacific) %>% 
  replaceNN(., grid, "sos") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, sos_transformed, geometry)
saveRDS(sos, "Data/Climatology/sf/sos_historical_oct-dec_interpolated.rds")

#### Plotting ####
# January-March
dataSalinity <- readRDS("Data/Climatology/sf/sos_historical_jan-mar_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

sal1 <- ggplot() +
  geom_sf(data = dataSalinity, aes(fill = sos_transformed), color = NA, size = 0.01) +
  scale_fill_gradientn(colors = rev(brewer.pal(9, "PuBuGn")),
                       na.value = "grey64",
                       guide = guide_colourbar(
                         title.vjust = 0.5,
                         barheight = grid::unit(0.01, "npc"),
                         barwidth = grid::unit(0.25, "npc"),
                         frame.colour = "black")) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('ppt')) +
  theme_bw()  +
  theme(legend.position = "bottom",
        axis.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 18),
        panel.border = element_blank()) +
  coord_sf(xlim = st_bbox(grid)$xlim, ylim = st_bbox(grid)$ylim)

ggsave(plot = sal1, filename = "Figures/global_historical_salinity_jan-mar.png", width = 15, height = 8, dpi = 300)

# April-June
dataSalinity <- readRDS("Data/Climatology/sf/sos_historical_apr-jun_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

sal2 <- ggplot() +
  geom_sf(data = dataSalinity, aes(fill = sos_transformed), color = NA, size = 0.01) +
  scale_fill_gradientn(colors = rev(brewer.pal(9, "PuBuGn")),
                       na.value = "grey64",
                       guide = guide_colourbar(
                         title.vjust = 0.5,
                         barheight = grid::unit(0.01, "npc"),
                         barwidth = grid::unit(0.25, "npc"),
                         frame.colour = "black")) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('ppt')) +
  theme_bw()  +
  theme(legend.position = "bottom",
        axis.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 18),
        panel.border = element_blank()) +
  coord_sf(xlim = st_bbox(grid)$xlim, ylim = st_bbox(grid)$ylim)

ggsave(plot = sal2, filename = "Figures/global_historical_salinity_apr-jun.png", width = 15, height = 8, dpi = 300)

# July-September
dataSalinity <- readRDS("Data/Climatology/sf/sos_historical_jul-sept_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

sal3 <- ggplot() +
  geom_sf(data = dataSalinity, aes(fill = sos_transformed), color = NA, size = 0.01) +
  scale_fill_gradientn(colors = rev(brewer.pal(9, "PuBuGn")),
                       na.value = "grey64",
                       guide = guide_colourbar(
                         title.vjust = 0.5,
                         barheight = grid::unit(0.01, "npc"),
                         barwidth = grid::unit(0.25, "npc"),
                         frame.colour = "black")) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('ppt')) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 18),
        panel.border = element_blank()) +
  coord_sf(xlim = st_bbox(grid)$xlim, ylim = st_bbox(grid)$ylim)

ggsave(plot = sal3, filename = "Figures/global_historical_salinity_jul-sept.png", width = 15, height = 8, dpi = 300)

# October-December
dataSalinity <- readRDS("Data/Climatology/sf/sos_historical_oct-dec_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

sal4 <- ggplot() +
  geom_sf(data = dataSalinity, aes(fill = sos_transformed), color = NA, size = 0.01) +
  scale_fill_gradientn(colors = rev(brewer.pal(9, "PuBuGn")),
                       na.value = "grey64",
                       guide = guide_colourbar(
                         title.vjust = 0.5,
                         barheight = grid::unit(0.01, "npc"),
                         barwidth = grid::unit(0.25, "npc"),
                         frame.colour = "black")) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('ppt')) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 18),
        panel.border = element_blank()) +
  coord_sf(xlim = st_bbox(grid)$xlim, ylim = st_bbox(grid)$ylim)

ggsave(plot = sal4, filename = "Figures/global_historical_salinity_oct-dec.png", width = 15, height = 8, dpi = 300)

# Full salinity plot
full_sal <- (sal1 + sal2) / (sal3 + sal4) +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = full_sal, filename = "Figures/global_historical_salinity_full.png", width = 27, height = 15, dpi = 300)