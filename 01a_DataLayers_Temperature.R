# DESCRIPTION: Creating seasonal temperature layers

# Load preliminaries
source("00_Utils.R")

# i. January-March
tos_rs <- stars::read_ncdf("Data/Climatology/ensemble/tos_historical_1956_1981_jan-mar_ensemble.nc") %>% 
  terra::rast()
names(tos_rs) <- paste0("X", seq(1956, 1981, by = 1))  
tos <- rs2sf(tos_rs) %>% 
  dplyr::rename(tos = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "tos") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, tos_transformed, geometry)  
saveRDS(tos, "Data/Climatology/sf/tos_historical_jan-mar_interpolated.rds")

# ii. April-June
tos_rs <- stars::read_ncdf("Data/Climatology/ensemble/tos_historical_1956_1981_apr-jun_ensemble.nc") %>% 
  terra::rast()
names(tos_rs) <- paste0("X", seq(1956, 1981, by = 1))  
tos <- rs2sf(tos_rs) %>% 
  dplyr::rename(tos = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "tos") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, tos_transformed, geometry)  
saveRDS(tos, "Data/Climatology/sf/tos_historical_apr-jun_interpolated.rds")  

# iii. July-September
tos_rs <- stars::read_ncdf("Data/Climatology/ensemble/tos_historical_1956_1981_jul-sept_ensemble.nc") %>% 
  terra::rast()
names(tos_rs) <- paste0("X", seq(1956, 1981, by = 1))  
tos <- rs2sf(tos_rs) %>% 
  dplyr::rename(tos = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "tos") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, tos_transformed, geometry)  
saveRDS(tos, "Data/Climatology/sf/tos_historical_jul-sept_interpolated.rds")

# iv. October-December
tos_rs <- stars::read_ncdf("Data/Climatology/ensemble/tos_historical_1956_1981_oct-dec_ensemble.nc") %>% 
  terra::rast()
names(tos_rs) <- paste0("X", seq(1956, 1981, by = 1))  
tos <- rs2sf(tos_rs) %>% 
  dplyr::rename(tos = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "tos") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, tos_transformed, geometry)  
saveRDS(tos, "Data/Climatology/sf/tos_historical_oct-dec_interpolated.rds")

#### Plotting ####

# January-March
dataTmp <- readRDS("Data/Climatology/sf/tos_historical_jan-mar_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

tmp1 <- ggplot() +
  geom_sf(data = dataTmp, aes(fill = tos_transformed), color = NA, size = 0.01) +
  scale_fill_cmocean(name = "deep",
                     #   alpha = 1,
                     aesthetics = c("fill"),
                     direction = -1,
                     na.value = "grey64",
                     guide = guide_colorbar(
                       title.vjust = 0.5,
                       barheight = grid::unit(0.01, "npc"),
                       barwidth = grid::unit(0.25, "npc"),
                       frame.colour = "black")) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression(''^"o"*'C')) +
  theme_bw() +
  gg_add_text()

ggsave(plot = tmp1, filename = "Figures/global_historical_temp_jan-mar.png", width = 15, height = 8, dpi = 300)

# April-June
dataTmp <- readRDS("Data/Climatology/sf/tos_historical_apr-jun_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

tmp2 <- ggplot() +
  geom_sf(data = dataTmp, aes(fill = tos_transformed), color = NA, size = 0.01) +
  scale_fill_cmocean(name = "deep",
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
  labs(fill = expression(''^"o"*'C')) +
  theme_bw() +
  gg_add_text()

ggsave(plot = tmp2, filename = "Figures/global_historical_temp_apr-jun.png", width = 15, height = 8, dpi = 300)

# July-September
dataTmp <- readRDS("Data/Climatology/sf/tos_historical_jul-sept_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

tmp3 <- ggplot() +
  geom_sf(data = dataTmp, aes(fill = tos_transformed), color = NA, size = 0.01) +
  scale_fill_cmocean(name = "deep",
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
  labs(fill = expression(''^"o"*'C')) +
  theme_bw() +
  gg_add_text()

ggsave(plot = tmp3, filename = "Figures/global_historical_temp_jul-sept.png", width = 15, height = 8, dpi = 300)

# October-December
dataTmp <- readRDS("Data/Climatology/sf/tos_historical_oct-dec_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

tmp4 <- ggplot() +
  geom_sf(data = dataTmp, aes(fill = tos_transformed), color = NA, size = 0.01) +
  scale_fill_cmocean(name = "deep",
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
  labs(fill = expression(''^"o"*'C')) +
  theme_bw() +
  gg_add_text()

ggsave(plot = tmp4, filename = "Figures/global_historical_temp_oct-dec.png", width = 15, height = 8, dpi = 300)

# Full temperature plot
full_tmp <- (tmp1 + tmp2) / (tmp3 + tmp4) +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = full_tmp, filename = "Figures/global_historical_temp_full.png", width = 27, height = 15, dpi = 300)
