# DESCRIPTION: Creating seasonal zonal velocity layers

# Load preliminaries
source("00_Utils.R")

# i. January-March
uo_rs <- stars::read_ncdf("Data/Climatology/ensemble/uo_historical_1956_1981_jan-mar_ensemble.nc") %>% 
  terra::rast()
names(uo_rs) <- paste0("X", seq(1956, 1981, by = 1))
uo <- rs2sf(uo_rs) %>% 
  dplyr::rename(uo = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "uo") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, uo_transformed, geometry)
saveRDS(uo, "Data/Climatology/sf/uo_historical_jan-mar_interpolated.rds")

# ii. April-June
uo_rs <- stars::read_ncdf("Data/Climatology/ensemble/uo_historical_1956_1981_apr-jun_ensemble.nc") %>% 
  terra::rast()
names(uo_rs) <- paste0("X", seq(1956, 1981, by = 1))
uo <- rs2sf(uo_rs) %>% 
  dplyr::rename(uo = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "uo") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, uo_transformed, geometry)
saveRDS(uo, "Data/Climatology/sf/uo_historical_apr-jun_interpolated.rds")

# iii. July-September
uo_rs <- stars::read_ncdf("Data/Climatology/ensemble/uo_historical_1956_1981_jul-sept_ensemble.nc") %>% 
  terra::rast()
names(uo_rs) <- paste0("X", seq(1956, 1981, by = 1))
uo <- rs2sf(uo_rs) %>% 
  dplyr::rename(uo = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "uo") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, uo_transformed, geometry)
saveRDS(uo, "Data/Climatology/sf/uo_historical_jul-sept_interpolated.rds")

# iv. October-December
uo_rs <- stars::read_ncdf("Data/Climatology/ensemble/uo_historical_1956_1981_oct-dec_ensemble.nc") %>% 
  terra::rast()
names(uo_rs) <- paste0("X", seq(1956, 1981, by = 1))
uo <- rs2sf(uo_rs) %>% 
  dplyr::rename(uo = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "uo") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, uo_transformed, geometry)
saveRDS(uo, "Data/Climatology/sf/uo_historical_oct-dec_interpolated.rds")

#### Plot: Zonal velocity ####

# January-March
dataUO <- readRDS("Data/Climatology/sf/uo_historical_jan-mar_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

uo1 <- ggplot() +
  geom_sf(data = dataUO, aes(fill = uo_transformed), color = NA, size = 0.01) +
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

ggsave(plot = uo1, filename = "Figures/global_historical_uo_jan-mar.png", width = 15, height = 8, dpi = 300)

# April-June
dataUO <- readRDS("Data/Climatology/sf/uo_historical_apr-jun_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

uo2 <- ggplot() +
  geom_sf(data = dataUO, aes(fill = uo_transformed), color = NA, size = 0.01) +
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

ggsave(plot = uo2, filename = "Figures/global_historical_uo_apr-jun.png", width = 15, height = 8, dpi = 300)

# July-September
dataUO <- readRDS("Data/Climatology/sf/uo_historical_jul-sept_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

uo3 <- ggplot() +
  geom_sf(data = dataUO, aes(fill = uo_transformed), color = NA, size = 0.01) +
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

ggsave(plot = uo3, filename = "Figures/global_historical_uo_jul-sept.png", width = 15, height = 8, dpi = 300)

# October-December
dataUO <- readRDS("Data/Climatology/sf/uo_historical_oct-dec_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

uo4 <- ggplot() +
  geom_sf(data = dataUO, aes(fill = uo_transformed), color = NA, size = 0.01) +
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

ggsave(plot = uo4, filename = "Figures/global_historical_uo_oct-dec.png", width = 15, height = 8, dpi = 300)

# Full zonal velocity plots
full_uo <- (uo1 + uo2) / (uo3 + uo4) +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = full_uo, filename = "Figures/global_historical_uo_full.png", width = 27, height = 15, dpi = 300)
