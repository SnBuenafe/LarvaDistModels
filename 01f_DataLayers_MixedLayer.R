# DESCRIPTION: Creating seasonal mixed layer thickness layers

# Load preliminaries
source("00_Utils.R")

# i. January-March
mlotst_rs <- stars::read_ncdf("Data/Climatology/ensemble/mlotst_historical_1956_1981_jan-mar_ensemble.nc") %>% 
  terra::rast()
names(mlotst_rs) <- paste0("X", seq(1956, 1981, by = 1))
mlotst <- rs2sf(mlotst_rs) %>% 
  dplyr::rename(mlotst = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll_pacific) %>% 
  replaceNN(., grid, "mlotst") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, mlotst_transformed, geometry)
saveRDS(mlotst, "Data/Climatology/sf/mlotst_historical_jan-mar_interpolated.rds")

# ii. April-June
mlotst_rs <- stars::read_ncdf("Data/Climatology/ensemble/mlotst_historical_1956_1981_apr-jun_ensemble.nc") %>% 
  terra::rast()
names(mlotst_rs) <- paste0("X", seq(1956, 1981, by = 1))
mlotst <- rs2sf(mlotst_rs) %>% 
  dplyr::rename(mlotst = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll_pacific) %>% 
  replaceNN(., grid, "mlotst") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, mlotst_transformed, geometry)
saveRDS(mlotst, "Data/Climatology/sf/mlotst_historical_apr-jun_interpolated.rds")

# iii. July-September
mlotst_rs <- stars::read_ncdf("Data/Climatology/ensemble/mlotst_historical_1956_1981_jul-sept_ensemble.nc") %>% 
  terra::rast()
names(mlotst_rs) <- paste0("X", seq(1956, 1981, by = 1))
mlotst <- rs2sf(mlotst_rs) %>% 
  dplyr::rename(mlotst = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll_pacific) %>% 
  replaceNN(., grid, "mlotst") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, mlotst_transformed, geometry)
saveRDS(mlotst, "Data/Climatology/sf/mlotst_historical_jul-sept_interpolated.rds")

# iv. October-December
mlotst_rs <- stars::read_ncdf("Data/Climatology/ensemble/mlotst_historical_1956_1981_oct-dec_ensemble.nc") %>% 
  terra::rast()
names(mlotst_rs) <- paste0("X", seq(1956, 1981, by = 1))
mlotst <- rs2sf(mlotst_rs) %>% 
  dplyr::rename(mlotst = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll_pacific) %>% 
  replaceNN(., grid, "mlotst") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, mlotst_transformed, geometry)
saveRDS(mlotst, "Data/Climatology/sf/mlotst_historical_oct-dec_interpolated.rds")

#### Plotting ####

# January-March
dataMixed <- readRDS("Data/Climatology/sf/mlotst_historical_jan-mar_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

mix1 <- ggplot() +
  geom_sf(data = dataMixed, aes(fill = mlotst_transformed), color = NA, size = 0.01) +
  scale_fill_gradientn(colors = rev(brewer.pal(9, "GnBu")),
                       na.value = "grey64",
                       oob = scales::squish,
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

ggsave(plot = mix1, filename = "Figures/global_historical_mixed_layer_jan-mar.png", width = 15, height = 8, dpi = 300)

# April-June
dataMixed <- readRDS("Data/Climatology/sf/mlotst_historical_apr-jun_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

mix2 <- ggplot() +
  geom_sf(data = dataMixed, aes(fill = mlotst_transformed), color = NA, size = 0.01) +
  scale_fill_gradientn(colors = rev(brewer.pal(9, "GnBu")),
                       na.value = "grey64",
                       oob = scales::squish,
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

ggsave(plot = mix2, filename = "Figures/global_historical_mixed_layer_apr-jun.png", width = 15, height = 8, dpi = 300)

# July-September
dataMixed <- readRDS("Data/Climatology/sf/mlotst_historical_jul-sept_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

mix3 <- ggplot() +
  geom_sf(data = dataMixed, aes(fill = mlotst_transformed), color = NA, size = 0.01) +
  scale_fill_gradientn(colors = rev(brewer.pal(9, "GnBu")),
                       na.value = "grey64",
                       oob = scales::squish,
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

ggsave(plot = mix3, filename = "Figures/global_historical_mixed_layer_jul-sept.png", width = 15, height = 8, dpi = 300)

# October-December
dataMixed <- readRDS("Data/Climatology/sf/mlotst_historical_oct-dec_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

mix4 <- ggplot() +
  geom_sf(data = dataMixed, aes(fill = mlotst_transformed), color = NA, size = 0.01) +
  scale_fill_gradientn(colors = rev(brewer.pal(9, "GnBu")),
                       na.value = "grey64",
                       oob = scales::squish,
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

ggsave(plot = mix4, filename = "Figures/global_historical_mixed_layer_oct-dec.png", width = 15, height = 8, dpi = 300)

# Full mixed layer depth plot
full_mix <- (mix1 + mix2) / (mix3 + mix4) +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = full_mix, filename = "Figures/global_historical_mixed_layer_full.png", width = 27, height = 15, dpi = 300)
