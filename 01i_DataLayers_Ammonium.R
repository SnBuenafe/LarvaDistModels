# DESCRIPTION: Creating seasonal ammonium layers

# Load preliminaries
source("00_Utils.R")

# i. January-March
nh4os_rs <- stars::read_ncdf("Data/Climatology/ensemble/nh4os_historical_1956_1981_jan-mar_ensemble.nc") %>% 
  terra::rast()
names(nh4os_rs) <- paste0("X", seq(1956, 1981, by = 1))
nh4os <- rs2sf(nh4os_rs) %>% 
  dplyr::rename(nh4os = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll_pacific) %>% 
  replaceNN(., grid, "nh4os") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, nh4os_transformed, geometry)
saveRDS(nh4os, "Data/Climatology/sf/nh4os_historical_jan-mar_interpolated.rds")

# ii. April-June
nh4os_rs <- stars::read_ncdf("Data/Climatology/ensemble/nh4os_historical_1956_1981_apr-jun_ensemble.nc") %>% 
  terra::rast()
names(nh4os_rs) <- paste0("X", seq(1956, 1981, by = 1))
nh4os <- rs2sf(nh4os_rs) %>% 
  dplyr::rename(nh4os = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll_pacific) %>% 
  replaceNN(., grid, "nh4os") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, nh4os_transformed, geometry)
saveRDS(nh4os, "Data/Climatology/sf/nh4os_historical_apr-jun_interpolated.rds")

# iii. July-September
nh4os_rs <- stars::read_ncdf("Data/Climatology/ensemble/nh4os_historical_1956_1981_jul-sept_ensemble.nc") %>% 
  terra::rast()
names(nh4os_rs) <- paste0("X", seq(1956, 1981, by = 1))
nh4os <- rs2sf(nh4os_rs) %>% 
  dplyr::rename(nh4os = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll_pacific) %>% 
  replaceNN(., grid, "nh4os") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, nh4os_transformed, geometry)
saveRDS(nh4os, "Data/Climatology/sf/nh4os_historical_jul-sept_interpolated.rds")

# iv. October-December
nh4os_rs <- stars::read_ncdf("Data/Climatology/ensemble/nh4os_historical_1956_1981_oct-dec_ensemble.nc") %>% 
  terra::rast()
names(nh4os_rs) <- paste0("X", seq(1956, 1981, by = 1))
nh4os <- rs2sf(nh4os_rs) %>% 
  dplyr::rename(nh4os = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll_pacific) %>% 
  replaceNN(., grid, "nh4os") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, nh4os_transformed, geometry)
saveRDS(nh4os, "Data/Climatology/sf/nh4os_historical_oct-dec_interpolated.rds")

#### Plotting ####

# January-March
dataNH4OS <- readRDS("Data/Climatology/sf/nh4os_historical_jan-mar_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

amm1 <- ggplot() +
  geom_sf(data = dataNH4OS, aes(fill = nh4os_transformed), color = NA, size = 0.01) +
  scale_fill_gradientn(colors = brewer.pal(9, "Reds"),
                       na.value = "grey64",
                       oob = scales::squish,
                       guide = guide_colourbar(
                         title.vjust = 0.5,
                         barheight = grid::unit(0.01, "npc"),
                         barwidth = grid::unit(0.25, "npc"),
                         frame.colour = "black")) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('mol m'^"-3"*'')) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 18),
        panel.border = element_blank()) +
  coord_sf(xlim = st_bbox(grid)$xlim, ylim = st_bbox(grid)$ylim)

ggsave(plot = amm1, filename = "Figures/global_historical_ammonium_jan-mar.png", width = 15, height = 8, dpi = 300)

# April-June
dataNH4OS <- readRDS("Data/Climatology/sf/nh4os_historical_apr-jun_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

amm2 <- ggplot() +
  geom_sf(data = dataNH4OS, aes(fill = nh4os_transformed), color = NA, size = 0.01) +
  scale_fill_gradientn(colors = brewer.pal(9, "Reds"),
                       na.value = "grey64",
                       oob = scales::squish,
                       guide = guide_colourbar(
                         title.vjust = 0.5,
                         barheight = grid::unit(0.01, "npc"),
                         barwidth = grid::unit(0.25, "npc"),
                         frame.colour = "black")) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('mol m'^"-3"*'')) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 18),
        panel.border = element_blank()) +
  coord_sf(xlim = st_bbox(grid)$xlim, ylim = st_bbox(grid)$ylim)

ggsave(plot = amm2, filename = "Figures/global_historical_ammonium_apr-jun.png", width = 15, height = 8, dpi = 300)

# July-September
dataNH4OS <- readRDS("Data/Climatology/sf/nh4os_historical_jul-sept_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

amm3 <- ggplot() +
  geom_sf(data = dataNH4OS, aes(fill = nh4os_transformed), color = NA, size = 0.01) +
  scale_fill_gradientn(colors = brewer.pal(9, "Reds"),
                       na.value = "grey64",
                       oob = scales::squish,
                       guide = guide_colourbar(
                         title.vjust = 0.5,
                         barheight = grid::unit(0.01, "npc"),
                         barwidth = grid::unit(0.25, "npc"),
                         frame.colour = "black")) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('mol m'^"-3"*'')) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 18),
        panel.border = element_blank()) +
  coord_sf(xlim = st_bbox(grid)$xlim, ylim = st_bbox(grid)$ylim)

# October-December
dataNH4OS <- readRDS("Data/Climatology/sf/nh4os_historical_oct-dec_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

amm4 <- ggplot() +
  geom_sf(data = dataNH4OS, aes(fill = nh4os_transformed), color = NA, size = 0.01) +
  scale_fill_gradientn(colors = brewer.pal(9, "Reds"),
                       na.value = "grey64",
                       oob = scales::squish,
                       guide = guide_colourbar(
                         title.vjust = 0.5,
                         barheight = grid::unit(0.01, "npc"),
                         barwidth = grid::unit(0.25, "npc"),
                         frame.colour = "black")) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('mol m'^"-3"*'')) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 18),
        panel.border = element_blank()) +
  coord_sf(xlim = st_bbox(grid)$xlim, ylim = st_bbox(grid)$ylim)

ggsave(plot = amm4, filename = "Figures/global_historical_ammonium_oct-dec.png", width = 15, height = 8, dpi = 300)

# Full ammonium plot
full_amm <- (amm1 + amm2) / (amm3 + amm4) +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = full_amm, filename = "Figures/global_historical_ammonium_full.png", width = 27, height = 15, dpi = 300)