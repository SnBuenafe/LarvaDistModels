source("00_Utils.R")

# ----- Temperature -----
# Historical
dataTmp <- readRDS("Data/Climatology/sf/tos_historical_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry")
  
tmp_historical <- ggplot() +
  geom_sf(data = dataTmp, aes(fill = tos_transformed), color = NA, size = 0.01) +
  scale_fill_cmocean(name = "deep",
                     #   alpha = 1,
                     aesthetics = c("fill"),
                     direction = -1,
                     na.value = "grey64",
                     limits = c(-5, 35)) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('Mean temperature (1956-1981) ('^"o"*'C)')) +
  theme_bw()

ggsave(plot = tmp_historical, filename = "Figures/global_present_temp.png", width = 15, height = 8, dpi = 300)

# Present
dataTmp <- readRDS("Data/Climatology/sf/tos_present_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

tmp_present <- ggplot() +
  geom_sf(data = dataTmp, aes(fill = tos_transformed), color = NA, size = 0.01) +
  scale_fill_cmocean(name = "deep",
                     #   alpha = 1,
                     aesthetics = c("fill"),
                     direction = -1,
                     na.value = "grey64",
                     limits = c(-5, 35)) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('Mean temperature (2017-2026) ('^"o"*'C)')) +
  theme_bw()

ggsave(plot = tmp_present, filename = "Figures/global_present_temp.png", width = 15, height = 8, dpi = 300)

# Mid century
dataTmp <- readRDS("Data/Climatology/sf/tos_midCentury_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

tmp_mid <- ggplot() +
  geom_sf(data = dataTmp, aes(fill = tos_transformed), color = NA, size = 0.01) +
  scale_fill_cmocean(name = "deep",
                     #   alpha = 1,
                     aesthetics = c("fill"),
                     direction = -1,
                     na.value = "grey64",
                     limits = c(-5, 35)) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('Mean temperature (2046-2055) ('^"o"*'C)')) +
  theme_bw()

ggsave(plot = tmp_mid, filename = "Figures/global_midCentury_temp.png", width = 15, height = 8, dpi = 300)

# End of the century
dataTmp <- readRDS("Data/Climatology/sf/tos_endCentury_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

tmp_end <- ggplot() +
  geom_sf(data = dataTmp, aes(fill = tos_transformed), color = NA, size = 0.01) +
  scale_fill_cmocean(name = "deep",
                     #   alpha = 1,
                     aesthetics = c("fill"),
                     direction = -1,
                     na.value = "grey64",
                     limits = c(-5, 35)) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('Mean temperature (2091-2100) ('^"o"*'C)')) +
  theme_bw()

ggsave(plot = tmp_end, filename = "Figures/global_endCentury_temp.png", width = 15, height = 8, dpi = 300)

tempAll <- (tmp_historical + tmp_present) / (tmp_mid + tmp_end)
ggsave(plot = tempAll, filename = "Figures/global_temperature.png", width = 18, height = 10, dpi = 300)

# ----- Oxygen -----
# Historical
dataO2 <- readRDS("Data/Climatology/sf/o2os_historical_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

O2_historical <- ggplot() +
  geom_sf(data = dataO2, aes(fill = o2os_transformed), color = NA, size = 0.01) +
  scale_fill_cmocean(name = "tempo",
                     #   alpha = 1,
                     aesthetics = c("fill"),
                     direction = -1,
                     na.value = "grey64",
                     limits = c(0.1, 0.5)
                    ) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('Mean O'[2]*' levels (1952-1981) (mol m'^"-3"*')')) +
  theme_bw()

ggsave(plot = O2_historical, filename = "Figures/global_historical_oxygen.png", width = 15, height = 8, dpi = 300)

# Present
dataO2 <- readRDS("Data/Climatology/sf/o2os_present_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

O2_present <- ggplot() +
  geom_sf(data = dataO2, aes(fill = o2os_transformed), color = NA, size = 0.01) +
  scale_fill_cmocean(name = "tempo",
                     #   alpha = 1,
                     aesthetics = c("fill"),
                     direction = -1,
                     na.value = "grey64",
                     limits = c(0.1, 0.5)) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('Mean O'[2]*' levels (2017-2026) (mol m'^"-3"*')')) +
  theme_bw()

ggsave(plot = O2_present, filename = "Figures/global_present_oxygen.png", width = 15, height = 8, dpi = 300)

# Mid century
dataO2 <- readRDS("Data/Climatology/sf/o2os_midCentury_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

O2_mid <- ggplot() +
  geom_sf(data = dataO2, aes(fill = o2os_transformed), color = NA, size = 0.01) +
  scale_fill_cmocean(name = "tempo",
                     #   alpha = 1,
                     aesthetics = c("fill"),
                     direction = -1,
                     na.value = "grey64",
                     limits = c(0.1, 0.5)) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('Mean O'[2]*' levels (2046-2055) (mol m'^"-3"*')')) +
  theme_bw()

ggsave(plot = O2_mid, filename = "Figures/global_midCentury_oxygen.png", width = 15, height = 8, dpi = 300)

# End of the century
dataO2 <- readRDS("Data/Climatology/sf/o2os_endCentury_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

O2_end <- ggplot() +
  geom_sf(data = dataO2, aes(fill = o2os_transformed), color = NA, size = 0.01) +
  scale_fill_cmocean(name = "tempo",
                     #   alpha = 1,
                     aesthetics = c("fill"),
                     direction = -1,
                     na.value = "grey64",
                     limits = c(0.1, 0.5)) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('Mean O'[2]*' levels (2091-2100) (mol m'^"-3"*')')) +
  theme_bw()

ggsave(plot = O2_end, filename = "Figures/global_endCentury_oxygen.png", width = 15, height = 8, dpi = 300)

O2All <- (O2_historical + O2_present) / (O2_mid + O2_end)
ggsave(plot = O2All, filename = "Figures/global_oxygen.png", width = 18, height = 10, dpi = 300)

# ---- Chlorophyll ----
# Historical
dataChlorophyll <- readRDS("Data/Climatology/sf/chlos_historical_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

chlos_historical <- ggplot() +
  geom_sf(data = dataChlorophyll, aes(fill = chlos_transformed), color = NA, size = 0.01) +
  scale_fill_cmocean(name = "algae",
                     #   alpha = 1,
                     aesthetics = c("fill"),
                     direction = 1,
                     na.value = "grey64",
                     limits = c(6e-30, 2e-6)
  ) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('Mean surface chlorophyll levels (1956-1981) (kg m'^"-3"*')')) +
  theme_bw()

ggsave(plot = chlos_historical, filename = "Figures/global_historical_chlorophyll.png", width = 15, height = 8, dpi = 300)

# Present
dataChlorophyll <- readRDS("Data/Climatology/sf/chlos_present_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

chlos_present <- ggplot() +
  geom_sf(data = dataChlorophyll, aes(fill = chlos_transformed), color = NA, size = 0.01) +
  scale_fill_cmocean(name = "algae",
                     #   alpha = 1,
                     aesthetics = c("fill"),
                     direction = 1,
                     na.value = "grey64",
                     limits = c(6e-30, 2e-6)) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('Mean surface chlorophyll levels (2017-2026) (kg m'^"-3"*')')) +
  theme_bw()

ggsave(plot = chlos_present, filename = "Figures/global_present_chlorophyll.png", width = 15, height = 8, dpi = 300)

# Mid century
dataChlorophyll <- readRDS("Data/Climatology/sf/chlos_midCentury_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

chlos_mid <- ggplot() +
  geom_sf(data = dataChlorophyll, aes(fill = chlos_transformed), color = NA, size = 0.01) +
  scale_fill_cmocean(name = "algae",
                     #   alpha = 1,
                     aesthetics = c("fill"),
                     direction = 1,
                     na.value = "grey64",
                     limits = c(6e-30, 2e-6)
                     ) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('Mean surface chlorophyll levels (2046-2055) (kg m'^"-3"*')')) +
  theme_bw()

ggsave(plot = chlos_mid, filename = "Figures/global_midCentury_chlorophyll.png", width = 15, height = 8, dpi = 300)

# End of the century
dataChlorophyll <- readRDS("Data/Climatology/sf/chlos_endCentury_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

chlos_end <- ggplot() +
  geom_sf(data = dataChlorophyll, aes(fill = chlos_transformed), color = NA, size = 0.01) +
  scale_fill_cmocean(name = "algae",
                     #   alpha = 1,
                     aesthetics = c("fill"),
                     direction = 1,
                     na.value = "grey64",
                     limits = c(6e-30, 2e-6)
                     ) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('Mean surface chlorophyll levels (2091-2100) (kg m'^"-3"*')')) +
  theme_bw()

ggsave(plot = chlos_end, filename = "Figures/global_endCentury_chlorophyll.png", width = 15, height = 8, dpi = 300)

ChlorophyllAll <- (chlos_historical + chlos_present) / (chlos_mid + chlos_end)
ggsave(plot = ChlorophyllAll, filename = "Figures/global_chlorophyll.png", width = 18, height = 10, dpi = 300)

# ---- pH ----
# Historical
dataPH <- readRDS("Data/Climatology/sf/phos_historical_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

PH_historical <- ggplot() +
  geom_sf(data = dataPH, aes(fill = phos_transformed), color = NA, size = 0.01) +
  scale_fill_cmocean(name = "dense",
                     #   alpha = 1,
                     aesthetics = c("fill"),
                     direction = -1,
                     na.value = "grey64",
                     limits = c(7, 8.5)) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('Mean surface pH levels (1956-1981)')) +
  theme_bw()
ggsave(plot = PH_historical, filename = "Figures/global_historical_ph.png", width = 15, height = 8, dpi = 300)

# Present
dataPH <- readRDS("Data/Climatology/sf/phos_present_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

PH_present <- ggplot() +
  geom_sf(data = dataPH, aes(fill = phos_transformed), color = NA, size = 0.01) +
  scale_fill_cmocean(name = "dense",
                     #   alpha = 1,
                     aesthetics = c("fill"),
                     direction = -1,
                     na.value = "grey64",
                     limits = c(7, 8.5)) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('Mean surface pH levels (2017-2026)')) +
  theme_bw()
ggsave(plot = PH_present, filename = "Figures/global_present_ph.png", width = 15, height = 8, dpi = 300)

# Mid-century
dataPH <- readRDS("Data/Climatology/sf/phos_midCentury_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

PH_midCentury <- ggplot() +
  geom_sf(data = dataPH, aes(fill = phos_transformed), color = NA, size = 0.01) +
  scale_fill_cmocean(name = "dense",
                     #   alpha = 1,
                     aesthetics = c("fill"),
                     direction = -1,
                     na.value = "grey64",
                     limits = c(7, 8.5)) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('Mean surface pH levels (2046-2055)')) +
  theme_bw()
ggsave(plot = PH_midCentury, filename = "Figures/global_midCentury_ph.png", width = 15, height = 8, dpi = 300)

# End of the century
dataPH <- readRDS("Data/Climatology/sf/phos_endCentury_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

PH_endCentury <- ggplot() +
  geom_sf(data = dataPH, aes(fill = phos_transformed), color = NA, size = 0.01) +
  scale_fill_cmocean(name = "dense",
                     #   alpha = 1,
                     aesthetics = c("fill"),
                     direction = -1,
                     na.value = "grey64",
                     limits = c(7, 8.5)) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('Mean surface pH levels (2091-2100)')) +
  theme_bw()
ggsave(plot = PH_endCentury, filename = "Figures/global_endCentury_ph.png", width = 15, height = 8, dpi = 300)

PHAll <- (PH_historical + PH_present) / (PH_midCentury + PH_endCentury)
ggsave(plot = PHAll, filename = "Figures/global_ph.png", width = 18, height = 10, dpi = 300)

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
