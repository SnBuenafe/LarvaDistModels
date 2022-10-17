source("00_Utils.R")

# ----- Temperature -----
dataTmp <- readRDS("Data/Climatology/sf/tos_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry")
  
ggTemp <- ggplot() +
  geom_sf(data = dataTmp, aes(fill = tos_transformed), color = NA, size = 0.01) +
  scale_fill_cmocean(name = "deep",
                  #   alpha = 1,
                     aesthetics = c("fill"),
                     direction = -1,
                     na.value = "grey64") +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('Mean temperature (1952-1981) ('^"o"*'C)')) +
  theme_bw()
ggsave(plot = ggTemp, filename = "Figures/global_temp.png", width = 15, height = 8, dpi = 300)

# ----- Oxygen -----
dataO2 <- readRDS("Data/Climatology/sf/o2os_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

ggO2 <- ggplot() +
  geom_sf(data = dataO2, aes(fill = o2os_transformed), color = NA, size = 0.01) +
  scale_fill_cmocean(name = "tempo",
                     #   alpha = 1,
                     aesthetics = c("fill"),
                     direction = 1,
                     na.value = "grey64") +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('Mean O'[2]*' levels (1952-1981) (mol m'^"-3"*')')) +
  theme_bw()
ggsave(plot = ggO2, filename = "Figures/global_oxygen.png", width = 15, height = 8, dpi = 300)

# ---- Chlorophyll ----
dataChl <- readRDS("Data/Climatology/sf/chlos_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

ggChl <- ggplot() +
  geom_sf(data = dataChl, aes(fill = chlos_transformed), color = NA, size = 0.01) +
  scale_fill_cmocean(name = "algae",
                     #   alpha = 1,
                     aesthetics = c("fill"),
                     direction = 1,
                     na.value = "grey64") +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('Mean surface chlorophyll levels (1952-1981) (kg m'^"-3"*')')) +
  theme_bw()
ggsave(plot = ggChl, filename = "Figures/global_chlorophyll.png", width = 15, height = 8, dpi = 300)

# ---- pH ----
dataPH <- readRDS("Data/Climatology/sf/phos_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

ggPH <- ggplot() +
  geom_sf(data = dataPH, aes(fill = phos_transformed), color = NA, size = 0.01) +
  scale_fill_cmocean(name = "dense",
                     #   alpha = 1,
                     aesthetics = c("fill"),
                     direction = -1,
                     na.value = "grey64") +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('Mean surface pH levels (1952-1981)')) +
  theme_bw()
ggsave(plot = ggPH, filename = "Figures/global_ph.png", width = 15, height = 8, dpi = 300)

# ----- Distance to coast -----
dataCoast <- readRDS("Data/CoastDistance.rds")

ggCoast <- ggplot() +
  geom_sf(data = dataCoast, aes(color = coastDistance), size = 0.2) +
  scale_color_cmocean(name = "deep",
                     #   alpha = 1,
                     aesthetics = c("color"),
                     direction = -1,
                     na.value = "grey64") +
  geom_sf(data = landmass, fill = "white", color = "white") +
  labs(fill = expression('Distance to the nearest coast (m)')) +
  theme_bw()
ggsave(plot = ggCoast, filename = "Figures/global_coast.png", width = 15, height = 8, dpi = 300)

# ---- Bathymetry ----
dataBathy <- readRDS("Data/GEBCO/gebco2500.rds")

ggBathy <- ggplot() +
  geom_sf(data = dataBathy, aes(fill = meanDepth), color = NA, size = 0.2) +
  scale_fill_cmocean(name = "ice",
                      #   alpha = 1,
                      aesthetics = c("fill"),
                      direction = 1,
                      na.value = "grey64") +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('Depth (m)')) +
  theme_bw()
ggsave(plot = ggBathy, filename = "Figures/global_bathy.png", width = 15, height = 8, dpi = 300)
