# DESCRIPTION: Creating seasonal chlorophyll layers

# Load preliminaries
source("00_Utils.R")

# i. January-March
chlos_rs <- stars::read_ncdf("Data/Climatology/ensemble/chlos_historical_1956_1981_jan-mar_ensemble.nc") %>% 
  terra::rast()
names(chlos_rs) <- paste0("X", seq(1956, 1981, by = 1))
chlos <- rs2sf(chlos_rs) %>% 
  dplyr::rename(chlos = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "chlos") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, chlos_transformed, geometry)
saveRDS(chlos, "Data/Climatology/sf/chlos_historical_jan-mar_interpolated.rds")

# ii. April-June
chlos_rs <- stars::read_ncdf("Data/Climatology/ensemble/chlos_historical_1956_1981_apr-jun_ensemble.nc") %>% 
  terra::rast()
names(chlos_rs) <- paste0("X", seq(1956, 1981, by = 1))
chlos <- rs2sf(chlos_rs) %>% 
  dplyr::rename(chlos = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "chlos") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, chlos_transformed, geometry)
saveRDS(chlos, "Data/Climatology/sf/chlos_historical_apr-jun_interpolated.rds")

# iii. July-September
chlos_rs <- stars::read_ncdf("Data/Climatology/ensemble/chlos_historical_1956_1981_jul-sept_ensemble.nc") %>% 
  terra::rast()
names(chlos_rs) <- paste0("X", seq(1956, 1981, by = 1))
chlos <- rs2sf(chlos_rs) %>% 
  dplyr::rename(chlos = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "chlos") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, chlos_transformed, geometry)
saveRDS(chlos, "Data/Climatology/sf/chlos_historical_jul-sept_interpolated.rds")

# iv. October-December
chlos_rs <- stars::read_ncdf("Data/Climatology/ensemble/chlos_historical_1956_1981_oct-dec_ensemble.nc") %>% 
  terra::rast()
names(chlos_rs) <- paste0("X", seq(1956, 1981, by = 1))
chlos <- rs2sf(chlos_rs) %>% 
  dplyr::rename(chlos = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "chlos") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, chlos_transformed, geometry)
saveRDS(chlos, "Data/Climatology/sf/chlos_historical_oct-dec_interpolated.rds")

### Plotting ####

# January-March
dataChlorophyll <- readRDS("Data/Climatology/sf/chlos_historical_jan-mar_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

chl1 <- ggplot() +
  geom_sf(data = dataChlorophyll, aes(fill = chlos_transformed), color = NA, size = 0.01) +
  scale_fill_gradientn(colors = brewer.pal(9, "YlGn"),
                       na.value = "grey64",
                       limits = c(as.numeric(quantile(dataChlorophyll$chlos_transformed, 0.05)), 
                                  max(dataChlorophyll$chlos_transformed)),
                       oob = scales::squish,
                       guide = guide_colourbar(
                         title.vjust = 0.5,
                         barheight = grid::unit(0.01, "npc"),
                         barwidth = grid::unit(0.25, "npc"),
                         frame.colour = "black")) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('kg m'^"-3"*'')) +
  theme_bw() +
  gg_add_text()

ggsave(plot = chl1, filename = "Figures/global_historical_chlorophyll_jan-mar.png", width = 15, height = 8, dpi = 300)

# April-June
dataChlorophyll <- readRDS("Data/Climatology/sf/chlos_historical_apr-jun_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

chl2 <- ggplot() +
  geom_sf(data = dataChlorophyll, aes(fill = chlos_transformed), color = NA, size = 0.01) +
  scale_fill_gradientn(colors = brewer.pal(9, "YlGn"),
                       na.value = "grey64",
                       limits = c(as.numeric(quantile(dataChlorophyll$chlos_transformed, 0.05)), 
                                  max(dataChlorophyll$chlos_transformed)),
                       oob = scales::squish,
                       guide = guide_colourbar(
                         title.vjust = 0.5,
                         barheight = grid::unit(0.01, "npc"),
                         barwidth = grid::unit(0.25, "npc"),
                         frame.colour = "black")) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('kg m'^"-3"*'')) +
  theme_bw() +
  gg_add_text()

ggsave(plot = chl2, filename = "Figures/global_historical_chlorophyll_apr-jun.png", width = 15, height = 8, dpi = 300)

# July-September
dataChlorophyll <- readRDS("Data/Climatology/sf/chlos_historical_jul-sept_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

chl3 <- ggplot() +
  geom_sf(data = dataChlorophyll, aes(fill = chlos_transformed), color = NA, size = 0.01) +
  scale_fill_gradientn(colors = brewer.pal(9, "YlGn"),
                       na.value = "grey64",
                       limits = c(as.numeric(quantile(dataChlorophyll$chlos_transformed, 0.05)), 
                                  max(dataChlorophyll$chlos_transformed)),
                       oob = scales::squish,
                       guide = guide_colourbar(
                         title.vjust = 0.5,
                         barheight = grid::unit(0.01, "npc"),
                         barwidth = grid::unit(0.25, "npc"),
                         frame.colour = "black")) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('kg m'^"-3"*'')) +
  theme_bw() +
  gg_add_text()

ggsave(plot = chl3, filename = "Figures/global_historical_chlorophyll_jul-sept.png", width = 15, height = 8, dpi = 300)

# October-December
dataChlorophyll <- readRDS("Data/Climatology/sf/chlos_historical_oct-dec_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

chl4 <- ggplot() +
  geom_sf(data = dataChlorophyll, aes(fill = chlos_transformed), color = NA, size = 0.01) +
  scale_fill_gradientn(colors = brewer.pal(9, "YlGn"),
                       na.value = "grey64",
                       limits = c(as.numeric(quantile(dataChlorophyll$chlos_transformed, 0.05)), 
                                  max(dataChlorophyll$chlos_transformed)),
                       oob = scales::squish,
                       guide = guide_colourbar(
                         title.vjust = 0.5,
                         barheight = grid::unit(0.01, "npc"),
                         barwidth = grid::unit(0.25, "npc"),
                         frame.colour = "black")) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('kg m'^"-3"*'')) +
  theme_bw() +
  gg_add_text()

ggsave(plot = chl4, filename = "Figures/global_historical_chlorophyll_oct-dec.png", width = 15, height = 8, dpi = 300)

# Full chlorophyll plot
full_chl <- (chl1 + chl2) / (chl3 + chl4) +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = full_chl, filename = "Figures/global_historical_chlorophyll_full.png", width = 27, height = 15, dpi = 300)

