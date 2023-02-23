# DESCRIPTION: Calculate salinity fronts

# Load preliminaries
source("00_Utils.R")

# i. January-March
rs <- raster::stack("Data/Climatology/ensemble/sos_historical_1956_1981_jan-mar_ensemble.nc")

# Clean up names
names(rs) <- names(rs) %>% 
  substr(., 1, 5)

# Calculate thermal spatial gradient
grad_rs <- spatGrad(rs, th = 0.0001, projected = FALSE) %>% 
  terra::rast() 

grad <- spat2sf(grad_rs) %>% 
  dplyr::rename(salinity_front = Grad,
                angle = Ang) %>% 
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "salinity_front") %>%
  replaceNN(., grid, "angle") %>% 
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, salinity_front_transformed, angle_transformed, geometry)

saveRDS(grad, "Data/Climatology/sf/salinity_front_historical_jan-mar_interpolated.rds")

# ii. April-June
rs <- raster::stack("Data/Climatology/ensemble/sos_historical_1956_1981_apr-jun_ensemble.nc")

# Clean up names
names(rs) <- names(rs) %>% 
  substr(., 1, 5)

# Calculate thermal spatial gradient
grad_rs <- spatGrad(rs, th = 0.0001, projected = FALSE) %>% # Calculate thermal spatial gradient
  terra::rast() 

grad <- spat2sf(grad_rs) %>% 
  dplyr::rename(salinity_front = Grad,
                angle = Ang) %>% 
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "salinity_front") %>%
  replaceNN(., grid, "angle") %>% 
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, salinity_front_transformed, angle_transformed, geometry)

saveRDS(grad, "Data/Climatology/sf/salinity_front_historical_apr-jun_interpolated.rds")

# iii. July-September
rs <- raster::stack("Data/Climatology/ensemble/sos_historical_1956_1981_jul-sept_ensemble.nc")

# Clean up names
names(rs) <- names(rs) %>% 
  substr(., 1, 5)

# Calculate thermal spatial gradient
grad_rs <- spatGrad(rs, th = 0.0001, projected = FALSE) %>% # Calculate thermal spatial gradient
  terra::rast() 

grad <- spat2sf(grad_rs) %>% 
  dplyr::rename(salinity_front = Grad,
                angle = Ang) %>% 
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "salinity_front") %>%
  replaceNN(., grid, "angle") %>% 
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, salinity_front_transformed, angle_transformed, geometry)

saveRDS(grad, "Data/Climatology/sf/salinity_front_historical_jul-sept_interpolated.rds")

# iv. October-December
rs <- raster::stack("Data/Climatology/ensemble/sos_historical_1956_1981_oct-dec_ensemble.nc")

# Clean up names
names(rs) <- names(rs) %>% 
  substr(., 1, 5)

# Calculate thermal spatial gradient
grad_rs <- spatGrad(rs, th = 0.0001, projected = FALSE) %>% # Calculate thermal spatial gradient
  terra::rast() 

grad <- spat2sf(grad_rs) %>% 
  dplyr::rename(salinity_front = Grad,
                angle = Ang) %>% 
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "salinity_front") %>%
  replaceNN(., grid, "angle") %>% 
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, salinity_front_transformed, angle_transformed, geometry)

saveRDS(grad, "Data/Climatology/sf/salinity_front_historical_oct-dec_interpolated.rds")

#### Plotting ####

# January-March
dataSalinity <- readRDS("Data/Climatology/sf/salinity_front_historical_jan-mar_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

sf1 <- ggplot() +
  geom_sf(data = dataSalinity, aes(fill = salinity_front_transformed), color = NA, size = 0.01) +
  scale_fill_gradientn(colors = rev(brewer.pal(9, "PuBuGn")),
                       na.value = "grey64",
                       guide = guide_colourbar(
                         title.vjust = 0.5,
                         barheight = grid::unit(0.01, "npc"),
                         barwidth = grid::unit(0.25, "npc"),
                         frame.colour = "black")) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('ppt km'^"-1")) +
  theme_bw() +
  gg_add_text(., "white")

ggsave(plot = sf1, filename = "Figures/global_historical_salinity_front_jan-mar.png", width = 15, height = 8, dpi = 300)

# April-June
dataSalinity <- readRDS("Data/Climatology/sf/salinity_front_historical_apr-jun_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

sf2 <- ggplot() +
  geom_sf(data = dataSalinity, aes(fill = salinity_front_transformed), color = NA, size = 0.01) +
  scale_fill_gradientn(colors = rev(brewer.pal(9, "PuBuGn")),
                       na.value = "grey64",
                       guide = guide_colourbar(
                         title.vjust = 0.5,
                         barheight = grid::unit(0.01, "npc"),
                         barwidth = grid::unit(0.25, "npc"),
                         frame.colour = "black")) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('ppt km'^"-1")) +
  theme_bw() +
  gg_add_text(., "white")

ggsave(plot = sf2, filename = "Figures/global_historical_salinity_front_apr-jun.png", width = 15, height = 8, dpi = 300)

# July-September
dataSalinity <- readRDS("Data/Climatology/sf/salinity_front_historical_jul-sept_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

sf3 <- ggplot() +
  geom_sf(data = dataSalinity, aes(fill = salinity_front_transformed), color = NA, size = 0.01) +
  scale_fill_gradientn(colors = rev(brewer.pal(9, "PuBuGn")),
                       na.value = "grey64",
                       guide = guide_colourbar(
                         title.vjust = 0.5,
                         barheight = grid::unit(0.01, "npc"),
                         barwidth = grid::unit(0.25, "npc"),
                         frame.colour = "black")) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('ppt km'^"-1")) +
  theme_bw() +
  gg_add_text(., "white")

ggsave(plot = sf3, filename = "Figures/global_historical_salinity_front_jul-sept.png", width = 15, height = 8, dpi = 300)

# October-December
dataSalinity <- readRDS("Data/Climatology/sf/salinity_front_historical_oct-dec_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

sf4 <- ggplot() +
  geom_sf(data = dataSalinity, aes(fill = salinity_front_transformed), color = NA, size = 0.01) +
  scale_fill_gradientn(colors = brewer.pal(9, "PuBuGn"),
                       na.value = "grey64",
                       guide = guide_colourbar(
                         title.vjust = 0.5,
                         barheight = grid::unit(0.01, "npc"),
                         barwidth = grid::unit(0.25, "npc"),
                         frame.colour = "black")) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('ppt km'^"-1")) +
  theme_bw() +
  gg_add_text(., "white")

ggsave(plot = sf4, filename = "Figures/global_historical_salinity_front_oct-dec.png", width = 15, height = 8, dpi = 300)

# Full thermal front plots
full_sf <- (sf1 + sf2) / (sf3 + sf4) +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = full_sf, filename = "Figures/global_historical_salinity_front_full.png", width = 27, height = 15, dpi = 300)
