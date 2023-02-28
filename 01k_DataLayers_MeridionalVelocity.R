# DESCRIPTION: Creating seasonal meridional velocity layers

# Load preliminaries
source("00_Utils.R")

# i. January-March
vo_rs <- stars::read_ncdf("Data/Climatology/ensemble/vo_historical_1956_1981_jan-mar_ensemble.nc") %>% 
  terra::rast()
names(vo_rs) <- paste0("X", seq(1956, 1981, by = 1))
vo <- rs2sf(vo_rs) %>% 
  dplyr::rename(vo = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "vo") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, vo_transformed, geometry)
saveRDS(vo, "Data/Climatology/sf/vo_historical_jan-mar_interpolated.rds")

# ii. April-June
vo_rs <- stars::read_ncdf("Data/Climatology/ensemble/vo_historical_1956_1981_apr-jun_ensemble.nc") %>% 
  terra::rast()
names(vo_rs) <- paste0("X", seq(1956, 1981, by = 1))
vo <- rs2sf(vo_rs) %>% 
  dplyr::rename(vo = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "vo") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, vo_transformed, geometry)
saveRDS(vo, "Data/Climatology/sf/vo_historical_apr-jun_interpolated.rds")

# iii. July-September
vo_rs <- stars::read_ncdf("Data/Climatology/ensemble/vo_historical_1956_1981_jul-sept_ensemble.nc") %>% 
  terra::rast()
names(vo_rs) <- paste0("X", seq(1956, 1981, by = 1))
vo <- rs2sf(vo_rs) %>% 
  dplyr::rename(vo = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "vo") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, vo_transformed, geometry)
saveRDS(vo, "Data/Climatology/sf/vo_historical_jul-sept_interpolated.rds")

# iv. October-December
vo_rs <- stars::read_ncdf("Data/Climatology/ensemble/vo_historical_1956_1981_oct-dec_ensemble.nc") %>% 
  terra::rast()
names(vo_rs) <- paste0("X", seq(1956, 1981, by = 1))
vo <- rs2sf(vo_rs) %>% 
  dplyr::rename(vo = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "vo") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, vo_transformed, geometry)
saveRDS(vo, "Data/Climatology/sf/vo_historical_oct-dec_interpolated.rds")

#### Plot: Meridional velocity ####

# January-March
datavo <- readRDS("Data/Climatology/sf/vo_historical_jan-mar_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

vo1 <- ggplot() +
  geom_sf(data = datavo, aes(fill = vo_transformed), color = NA, size = 0.01) +
  scale_fill_gradientn(colors = rev(brewer.pal(9, "Blues")),
                       na.value = "grey64",
                       oob = scales::squish,
                       guide = guide_colourbar(
                         title.vjust = 0.5,
                         barheight = grid::unit(0.01, "npc"),
                         barwidth = grid::unit(0.25, "npc"),
                         frame.colour = "black")) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('m s'^"-1"*'')) +
  theme_bw() +
  gg_add_text(., "white")

ggsave(plot = vo1, filename = "Figures/global_historical_vo_jan-mar.png", width = 15, height = 8, dpi = 300)

# April-June
datavo <- readRDS("Data/Climatology/sf/vo_historical_apr-jun_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

vo2 <- ggplot() +
  geom_sf(data = datavo, aes(fill = vo_transformed), color = NA, size = 0.01) +
  scale_fill_gradientn(colors = rev(brewer.pal(9, "Blues")),
                       na.value = "grey64",
                       oob = scales::squish,
                       guide = guide_colourbar(
                         title.vjust = 0.5,
                         barheight = grid::unit(0.01, "npc"),
                         barwidth = grid::unit(0.25, "npc"),
                         frame.colour = "black")) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('m s'^"-1"*'')) +
  theme_bw() +
  gg_add_text(., "white")

ggsave(plot = vo2, filename = "Figures/global_historical_vo_apr-jun.png", width = 15, height = 8, dpi = 300)

# July-September
datavo <- readRDS("Data/Climatology/sf/vo_historical_jul-sept_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

vo3 <- ggplot() +
  geom_sf(data = datavo, aes(fill = vo_transformed), color = NA, size = 0.01) +
  scale_fill_gradientn(colors = rev(brewer.pal(9, "Blues")),
                       na.value = "grey64",
                       oob = scales::squish,
                       guide = guide_colourbar(
                         title.vjust = 0.5,
                         barheight = grid::unit(0.01, "npc"),
                         barwidth = grid::unit(0.25, "npc"),
                         frame.colour = "black")) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('m s'^"-1"*'')) +
  theme_bw() +
  gg_add_text(., "white")

ggsave(plot = vo3, filename = "Figures/global_historical_vo_jul-sept.png", width = 15, height = 8, dpi = 300)

# October-December
datavo <- readRDS("Data/Climatology/sf/vo_historical_oct-dec_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

vo4 <- ggplot() +
  geom_sf(data = datavo, aes(fill = vo_transformed), color = NA, size = 0.01) +
  scale_fill_gradientn(colors = rev(brewer.pal(9, "Blues")),
                       na.value = "grey64",
                       oob = scales::squish,
                       guide = guide_colourbar(
                         title.vjust = 0.5,
                         barheight = grid::unit(0.01, "npc"),
                         barwidth = grid::unit(0.25, "npc"),
                         frame.colour = "black")) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('m s'^"-1"*'')) +
  theme_bw() +
  gg_add_text(., "white")

ggsave(plot = vo4, filename = "Figures/global_historical_vo_oct-dec.png", width = 15, height = 8, dpi = 300)

# Full zonal velocity plots
full_vo <- (vo1 + vo2) / (vo3 + vo4) +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = full_vo, filename = "Figures/global_historical_vo_full.png", width = 27, height = 15, dpi = 300)
