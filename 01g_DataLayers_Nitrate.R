# DESCRIPTION: Creating seasonal nitrate layers

# Load preliminaries
source("00_Utils.R")

# i. January-March
no3os_rs <- stars::read_ncdf("Data/Climatology/ensemble/no3os_historical_1956_1981_jan-mar_ensemble.nc") %>% 
  terra::rast()
names(no3os_rs) <- paste0("X", seq(1956, 1981, by = 1))
no3os <- rs2sf(no3os_rs) %>% 
  dplyr::rename(no3os = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "no3os") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, no3os_transformed, geometry)
saveRDS(no3os, "Data/Climatology/sf/no3os_historical_jan-mar_interpolated.rds")

# ii. April-June
no3os_rs <- stars::read_ncdf("Data/Climatology/ensemble/no3os_historical_1956_1981_apr-jun_ensemble.nc") %>% 
  terra::rast()
names(no3os_rs) <- paste0("X", seq(1956, 1981, by = 1))
no3os <- rs2sf(no3os_rs) %>% 
  dplyr::rename(no3os = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "no3os") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, no3os_transformed, geometry)
saveRDS(no3os, "Data/Climatology/sf/no3os_historical_apr-jun_interpolated.rds")

# iii. July-September
no3os_rs <- stars::read_ncdf("Data/Climatology/ensemble/no3os_historical_1956_1981_jul-sept_ensemble.nc") %>% 
  terra::rast()
names(no3os_rs) <- paste0("X", seq(1956, 1981, by = 1))
no3os <- rs2sf(no3os_rs) %>% 
  dplyr::rename(no3os = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "no3os") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, no3os_transformed, geometry)
saveRDS(no3os, "Data/Climatology/sf/no3os_historical_jul-sept_interpolated.rds")

# iv. October-December
no3os_rs <- stars::read_ncdf("Data/Climatology/ensemble/no3os_historical_1956_1981_oct-dec_ensemble.nc") %>% 
  terra::rast()
names(no3os_rs) <- paste0("X", seq(1956, 1981, by = 1))
no3os <- rs2sf(no3os_rs) %>% 
  dplyr::rename(no3os = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "no3os") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, no3os_transformed, geometry)
saveRDS(no3os, "Data/Climatology/sf/no3os_historical_oct-dec_interpolated.rds")

#### Plotting ####

# January-March
dataNO3 <- readRDS("Data/Climatology/sf/no3os_historical_jan-mar_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

nit1 <- ggplot() +
  geom_sf(data = dataNO3, aes(fill = no3os_transformed), color = NA, size = 0.01) +
  scale_fill_gradientn(colors = brewer.pal(9, "YlOrBr"),
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
  gg_add_text()

ggsave(plot = nit1, filename = "Figures/global_historical_nitrate_jan-mar.png", width = 15, height = 8, dpi = 300)

# April-June
dataNO3 <- readRDS("Data/Climatology/sf/no3os_historical_apr-jun_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

nit2 <- ggplot() +
  geom_sf(data = dataNO3, aes(fill = no3os_transformed), color = NA, size = 0.01) +
  scale_fill_gradientn(colors = brewer.pal(9, "YlOrBr"),
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
  gg_add_text()

ggsave(plot = nit2, filename = "Figures/global_historical_nitrate_apr-jun.png", width = 15, height = 8, dpi = 300)

# July-September
dataNO3 <- readRDS("Data/Climatology/sf/no3os_historical_jul-sept_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

nit3 <- ggplot() +
  geom_sf(data = dataNO3, aes(fill = no3os_transformed), color = NA, size = 0.01) +
  scale_fill_gradientn(colors = brewer.pal(9, "YlOrBr"),
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
  gg_add_text()

ggsave(plot = nit3, filename = "Figures/global_historical_nitrate_jul-sept.png", width = 15, height = 8, dpi = 300)

# October-December
dataNO3 <- readRDS("Data/Climatology/sf/no3os_historical_oct-dec_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

nit4 <- ggplot() +
  geom_sf(data = dataNO3, aes(fill = no3os_transformed), color = NA, size = 0.01) +
  scale_fill_gradientn(colors = brewer.pal(9, "YlOrBr"),
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
  gg_add_text()

ggsave(plot = nit4, filename = "Figures/global_historical_nitrate_oct-dec.png", width = 15, height = 8, dpi = 300)

# Full nitrate plot
full_nit <- (nit1 + nit2) / (nit3 + nit4) +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = full_nit, filename = "Figures/global_historical_nitrate_full.png", width = 27, height = 15, dpi = 300)
