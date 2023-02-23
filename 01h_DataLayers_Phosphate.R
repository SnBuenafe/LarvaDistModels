# DESCRIPTION: Creating seasonal phosphate layers

# Load preliminaries
source("00_Utils.R")

# i. January-March
po4os_rs <- stars::read_ncdf("Data/Climatology/ensemble/po4os_historical_1956_1981_jan-mar_ensemble.nc") %>% 
  terra::rast()
names(po4os_rs) <- paste0("X", seq(1956, 1981, by = 1))
po4os <- rs2sf(po4os_rs) %>% 
  dplyr::rename(po4os = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "po4os") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, po4os_transformed, geometry)
saveRDS(po4os, "Data/Climatology/sf/po4os_historical_jan-mar_interpolated.rds")

# ii. April-June
po4os_rs <- stars::read_ncdf("Data/Climatology/ensemble/po4os_historical_1956_1981_apr-jun_ensemble.nc") %>% 
  terra::rast()
names(po4os_rs) <- paste0("X", seq(1956, 1981, by = 1))
po4os <- rs2sf(po4os_rs) %>% 
  dplyr::rename(po4os = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "po4os") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, po4os_transformed, geometry)
saveRDS(po4os, "Data/Climatology/sf/po4os_historical_apr-jun_interpolated.rds")

# iii. July-September
po4os_rs <- stars::read_ncdf("Data/Climatology/ensemble/po4os_historical_1956_1981_jul-sept_ensemble.nc") %>% 
  terra::rast()
names(po4os_rs) <- paste0("X", seq(1956, 1981, by = 1))
po4os <- rs2sf(po4os_rs) %>% 
  dplyr::rename(po4os = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "po4os") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, po4os_transformed, geometry)
saveRDS(po4os, "Data/Climatology/sf/po4os_historical_jul-sept_interpolated.rds")

# iv. October-December
po4os_rs <- stars::read_ncdf("Data/Climatology/ensemble/po4os_historical_1956_1981_oct-dec_ensemble.nc") %>% 
  terra::rast()
names(po4os_rs) <- paste0("X", seq(1956, 1981, by = 1))
po4os <- rs2sf(po4os_rs) %>% 
  dplyr::rename(po4os = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "po4os") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, po4os_transformed, geometry)
saveRDS(po4os, "Data/Climatology/sf/po4os_historical_oct-dec_interpolated.rds")

#### Plotting ####

# January-March
dataPO4OS <- readRDS("Data/Climatology/sf/po4os_historical_jan-mar_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

phos1 <- ggplot() +
  geom_sf(data = dataPO4OS, aes(fill = po4os_transformed), color = NA, size = 0.01) +
  scale_fill_gradientn(colors = brewer.pal(9, "BuPu"),
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

ggsave(plot = phos1, filename = "Figures/global_historical_phosphate_jan-mar.png", width = 15, height = 8, dpi = 300)

# April-June
dataPO4OS <- readRDS("Data/Climatology/sf/po4os_historical_apr-jun_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

phos2 <- ggplot() +
  geom_sf(data = dataPO4OS, aes(fill = po4os_transformed), color = NA, size = 0.01) +
  scale_fill_gradientn(colors = brewer.pal(9, "BuPu"),
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

ggsave(plot = phos2, filename = "Figures/global_historical_phosphate_apr-jun.png", width = 15, height = 8, dpi = 300)

# July-September
dataPO4OS <- readRDS("Data/Climatology/sf/po4os_historical_jul-sept_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

phos3 <- ggplot() +
  geom_sf(data = dataPO4OS, aes(fill = po4os_transformed), color = NA, size = 0.01) +
  scale_fill_gradientn(colors = brewer.pal(9, "BuPu"),
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

ggsave(plot = phos3, filename = "Figures/global_historical_phosphate_jul-sept.png", width = 15, height = 8, dpi = 300)

# October-December
dataPO4OS <- readRDS("Data/Climatology/sf/po4os_historical_oct-dec_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

phos4 <- ggplot() +
  geom_sf(data = dataPO4OS, aes(fill = po4os_transformed), color = NA, size = 0.01) +
  scale_fill_gradientn(colors = brewer.pal(9, "BuPu"),
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

ggsave(plot = phos4, filename = "Figures/global_historical_phosphate_oct-dec.png", width = 15, height = 8, dpi = 300)

# Full phosphate plot
full_phos <- (phos1 + phos2) / (phos3 + phos4) +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = full_phos, filename = "Figures/global_historical_phosphate_full.png", width = 27, height = 15, dpi = 300)
