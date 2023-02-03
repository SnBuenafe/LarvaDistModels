source("00_Utils.R")

# ----- Temperature -----
#### January-March ####
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

#### April-June ####
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

#### July-September ####
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

##### October-December ####
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

#### Full temperature plot ####
full_tmp <- (tmp1 + tmp2) / (tmp3 + tmp4) +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = full_tmp, filename = "Figures/global_historical_temp_full.png", width = 27, height = 15, dpi = 300)

# ----- Oxygen -----
##### January-March ##### 
dataO2 <- readRDS("Data/Climatology/sf/o2os_historical_jan-mar_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

ox1 <- ggplot() +
  geom_sf(data = dataO2, aes(fill = o2os_transformed), color = NA, size = 0.01) +
  scale_fill_cmocean(name = "tempo",
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
  labs(fill = expression('mol m'^"-3"*'')) +
  theme_bw() +
  gg_add_text(., "white")

ggsave(plot = ox1, filename = "Figures/global_historical_oxygen_jan-mar.png", width = 15, height = 8, dpi = 300)

#### April-June #### 
dataO2 <- readRDS("Data/Climatology/sf/o2os_historical_apr-jun_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

ox2 <- ggplot() +
  geom_sf(data = dataO2, aes(fill = o2os_transformed), color = NA, size = 0.01) +
  scale_fill_cmocean(name = "tempo",
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
  labs(fill = expression('mol m'^"-3"*'')) +
  theme_bw() +
  gg_add_text(., "white")

ggsave(plot = ox2, filename = "Figures/global_historical_oxygen_apr-jun.png", width = 15, height = 8, dpi = 300)

####  July-September #### 
dataO2 <- readRDS("Data/Climatology/sf/o2os_historical_jul-sept_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

ox3 <- ggplot() +
  geom_sf(data = dataO2, aes(fill = o2os_transformed), color = NA, size = 0.01) +
  scale_fill_cmocean(name = "tempo",
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
  labs(fill = expression('mol m'^"-3"*'')) +
  theme_bw() +
  gg_add_text(., "white")

ggsave(plot = ox3, filename = "Figures/global_historical_oxygen_jul-sept.png", width = 15, height = 8, dpi = 300)

####  October-December #### 
dataO2 <- readRDS("Data/Climatology/sf/o2os_historical_oct-dec_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

ox4 <- ggplot() +
  geom_sf(data = dataO2, aes(fill = o2os_transformed), color = NA, size = 0.01) +
  scale_fill_cmocean(name = "tempo",
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
  labs(fill = expression('mol m'^"-3"*'')) +
  theme_bw() +
  gg_add_text(., "white")

ggsave(plot = ox4, filename = "Figures/global_historical_oxygen_oct-dec.png", width = 15, height = 8, dpi = 300)

#### Full oxygen plot ####
full_ox <- (ox1 + ox2) / (ox3 + ox4) +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = full_ox, filename = "Figures/global_historical_oxygen_full.png", width = 27, height = 15, dpi = 300)

# ---- Chlorophyll ----
#### January-March ####
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

#### April-June ####
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

#### July-September ####
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

#### October-December ####
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

#### Full chlorophyll plot ####
full_chl <- (chl1 + chl2) / (chl3 + chl4) +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = full_chl, filename = "Figures/global_historical_chlorophyll_full.png", width = 27, height = 15, dpi = 300)


# ---- pH ----
#### January-March#### 
dataPH <- readRDS("Data/Climatology/sf/phos_historical_jan-mar_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

ph1 <- ggplot() +
  geom_sf(data = dataPH, aes(fill = phos_transformed), color = NA, size = 0.01) +
  scale_fill_gradientn(colors = brewer.pal(9, "RdPu"),
                       na.value = "grey64",
                       guide = guide_colourbar(
                         title.vjust = 0.5,
                         barheight = grid::unit(0.01, "npc"),
                         barwidth = grid::unit(0.25, "npc"),
                         frame.colour = "black")) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('pH')) +
  theme_bw() +
  gg_add_text()

ggsave(plot = ph1, filename = "Figures/global_historical_ph_jan-mar.png", width = 15, height = 8, dpi = 300)

#### April-June #### 
dataPH <- readRDS("Data/Climatology/sf/phos_historical_apr-jun_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

ph2 <- ggplot() +
  geom_sf(data = dataPH, aes(fill = phos_transformed), color = NA, size = 0.01) +
  scale_fill_gradientn(colors = brewer.pal(9, "RdPu"),
                       na.value = "grey64",
                       guide = guide_colourbar(
                         title.vjust = 0.5,
                         barheight = grid::unit(0.01, "npc"),
                         barwidth = grid::unit(0.25, "npc"),
                         frame.colour = "black")) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('pH')) +
  theme_bw() +
  gg_add_text()

ggsave(plot = ph2, filename = "Figures/global_historical_ph_apr-jun.png", width = 15, height = 8, dpi = 300)

#### July-September #### 
dataPH <- readRDS("Data/Climatology/sf/phos_historical_jul-sept_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

ph3 <- ggplot() +
  geom_sf(data = dataPH, aes(fill = phos_transformed), color = NA, size = 0.01) +
  scale_fill_gradientn(colors = brewer.pal(9, "RdPu"),
                       na.value = "grey64",
                       guide = guide_colourbar(
                         title.vjust = 0.5,
                         barheight = grid::unit(0.01, "npc"),
                         barwidth = grid::unit(0.25, "npc"),
                         frame.colour = "black")) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('pH')) +
  theme_bw() +
  gg_add_text()

ggsave(plot = ph3, filename = "Figures/global_historical_ph_jul-sept.png", width = 15, height = 8, dpi = 300)

#### October-December #### 
dataPH <- readRDS("Data/Climatology/sf/phos_historical_oct-dec_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

ph4 <- ggplot() +
  geom_sf(data = dataPH, aes(fill = phos_transformed), color = NA, size = 0.01) +
  scale_fill_gradientn(colors = brewer.pal(9, "RdPu"),
                       na.value = "grey64",
                       guide = guide_colourbar(
                         title.vjust = 0.5,
                         barheight = grid::unit(0.01, "npc"),
                         barwidth = grid::unit(0.25, "npc"),
                         frame.colour = "black")) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('pH')) +
  theme_bw() +
  gg_add_text()

ggsave(plot = ph4, filename = "Figures/global_historical_ph_oct-dec.png", width = 15, height = 8, dpi = 300)

#### Full pH plot ####
full_ph <- (ph1 + ph2) / (ph3 + ph4) +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = full_ph, filename = "Figures/global_historical_ph_full.png", width = 27, height = 15, dpi = 300)

# ----- Distance to coast -----
dataCoast <- readRDS("Data/CoastDistance.rds") %>% 
  crop_predictor()

ggCoast <- ggplot() +
  geom_sf(data = dataCoast, aes(color = coastDistance/1000), size = 0.2) +
  scale_color_cmocean(name = "deep",
                     #   alpha = 1,
                     aesthetics = c("color"),
                     direction = -1,
                     na.value = "grey64",
                     guide = guide_colourbar(
                       title.vjust = 0.5,
                       barheight = grid::unit(0.01, "npc"),
                       barwidth = grid::unit(0.25, "npc"),
                       frame.colour = "black")) +
  geom_sf(data = landmass, fill = "white", color = "white") +
  labs(color = expression('km')) +
  theme_bw() +
  gg_add_text(., "white")

ggsave(plot = ggCoast, filename = "Figures/global_coast.png", width = 15, height = 8, dpi = 300)

# ---- Bathymetry ----
dataBathy <- readRDS("Data/GEBCO/gebco2500.rds") %>% 
  crop_predictor()

ggBathy <- ggplot() +
  geom_sf(data = dataBathy, aes(fill = meanDepth), color = NA, size = 0.2) +
  scale_fill_cmocean(name = "ice",
                     #   alpha = 1,
                     aesthetics = c("fill"),
                     direction = 1,
                     na.value = "grey64",
                     guide = guide_colourbar(
                       title.vjust = 0.5,
                       barheight = grid::unit(0.01, "npc"),
                       barwidth = grid::unit(0.25, "npc"),
                       frame.colour = "black")) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('m')) +
  theme_bw() +
  gg_add_text(., "white")

ggsave(plot = ggBathy, filename = "Figures/global_bathy.png", width = 15, height = 8, dpi = 300)

# ----- Salinity -----
#### January-March ####
dataSalinity <- readRDS("Data/Climatology/sf/sos_historical_jan-mar_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

sal1 <- ggplot() +
  geom_sf(data = dataSalinity, aes(fill = sos_transformed), color = NA, size = 0.01) +
  scale_fill_gradientn(colors = rev(brewer.pal(9, "PuBuGn")),
                       na.value = "grey64",
                       guide = guide_colourbar(
                         title.vjust = 0.5,
                         barheight = grid::unit(0.01, "npc"),
                         barwidth = grid::unit(0.25, "npc"),
                         frame.colour = "black")) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('ppt')) +
  theme_bw() +
  gg_add_text()

ggsave(plot = sal1, filename = "Figures/global_historical_salinity_jan-mar.png", width = 15, height = 8, dpi = 300)

#### April-June ####
dataSalinity <- readRDS("Data/Climatology/sf/sos_historical_apr-jun_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

sal2 <- ggplot() +
  geom_sf(data = dataSalinity, aes(fill = sos_transformed), color = NA, size = 0.01) +
  scale_fill_gradientn(colors = rev(brewer.pal(9, "PuBuGn")),
                       na.value = "grey64",
                       guide = guide_colourbar(
                         title.vjust = 0.5,
                         barheight = grid::unit(0.01, "npc"),
                         barwidth = grid::unit(0.25, "npc"),
                         frame.colour = "black")) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('ppt')) +
  theme_bw() +
  gg_add_text()

ggsave(plot = sal2, filename = "Figures/global_historical_salinity_apr-jun.png", width = 15, height = 8, dpi = 300)

#### July-September ####
dataSalinity <- readRDS("Data/Climatology/sf/sos_historical_jul-sept_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

sal3 <- ggplot() +
  geom_sf(data = dataSalinity, aes(fill = sos_transformed), color = NA, size = 0.01) +
  scale_fill_gradientn(colors = rev(brewer.pal(9, "PuBuGn")),
                       na.value = "grey64",
                       guide = guide_colourbar(
                         title.vjust = 0.5,
                         barheight = grid::unit(0.01, "npc"),
                         barwidth = grid::unit(0.25, "npc"),
                         frame.colour = "black")) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('ppt')) +
  theme_bw() +
  gg_add_text()

ggsave(plot = sal3, filename = "Figures/global_historical_salinity_jul-sept.png", width = 15, height = 8, dpi = 300)

#### October-December ####
dataSalinity <- readRDS("Data/Climatology/sf/sos_historical_oct-dec_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

sal4 <- ggplot() +
  geom_sf(data = dataSalinity, aes(fill = sos_transformed), color = NA, size = 0.01) +
  scale_fill_gradientn(colors = rev(brewer.pal(9, "PuBuGn")),
                       na.value = "grey64",
                       guide = guide_colourbar(
                         title.vjust = 0.5,
                         barheight = grid::unit(0.01, "npc"),
                         barwidth = grid::unit(0.25, "npc"),
                         frame.colour = "black")) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('ppt')) +
  theme_bw() +
  gg_add_text()

ggsave(plot = sal4, filename = "Figures/global_historical_salinity_oct-dec.png", width = 15, height = 8, dpi = 300)

#### Full salinity plot ####
full_sal <- (sal1 + sal2) / (sal3 + sal4) +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = full_sal, filename = "Figures/global_historical_salinity_full.png", width = 27, height = 15, dpi = 300)

# ----- Mixed layer thickness -----
#### January-March ####
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
  gg_add_text(., "white")

ggsave(plot = mix1, filename = "Figures/global_historical_mixed_layer_jan-mar.png", width = 15, height = 8, dpi = 300)

#### April-June ####
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
  gg_add_text(., "white")

ggsave(plot = mix2, filename = "Figures/global_historical_mixed_layer_apr-jun.png", width = 15, height = 8, dpi = 300)

#### July-September ####
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
  gg_add_text(., "white")

ggsave(plot = mix3, filename = "Figures/global_historical_mixed_layer_jul-sept.png", width = 15, height = 8, dpi = 300)

#### October-December ####
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
  gg_add_text(., "white")

ggsave(plot = mix4, filename = "Figures/global_historical_mixed_layer_oct-dec.png", width = 15, height = 8, dpi = 300)

#### Full mixed layer depth plot ####
full_mix <- (mix1 + mix2) / (mix3 + mix4) +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = full_mix, filename = "Figures/global_historical_mixed_layer_full.png", width = 27, height = 15, dpi = 300)

# ----- Nitrate -----
#### January-March ####
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

#### April-June ####
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

#### July-September ####
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

#### October-December ####
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

#### Full nitrate plot ####
full_nit <- (nit1 + nit2) / (nit3 + nit4) +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = full_nit, filename = "Figures/global_historical_nitrate_full.png", width = 27, height = 15, dpi = 300)

# ----- Phosphate -----
#### January-March ####
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

#### April-June ####
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

#### July-September ####
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

#### October-December ####
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

#### Full phosphate plot ####
full_phos <- (phos1 + phos2) / (phos3 + phos4) +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = full_phos, filename = "Figures/global_historical_phosphate_full.png", width = 27, height = 15, dpi = 300)

# ----- Ammonium -----
#### January-March ####
dataNH4OS <- readRDS("Data/Climatology/sf/nh4os_historical_jan-mar_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

amm1 <- ggplot() +
  geom_sf(data = dataNH4OS, aes(fill = nh4os_transformed), color = NA, size = 0.01) +
  scale_fill_gradientn(colors = brewer.pal(9, "Reds"),
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

ggsave(plot = amm1, filename = "Figures/global_historical_ammonium_jan-mar.png", width = 15, height = 8, dpi = 300)

#### April-June ####
dataNH4OS <- readRDS("Data/Climatology/sf/nh4os_historical_apr-jun_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

amm2 <- ggplot() +
  geom_sf(data = dataNH4OS, aes(fill = nh4os_transformed), color = NA, size = 0.01) +
  scale_fill_gradientn(colors = brewer.pal(9, "Reds"),
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

ggsave(plot = amm2, filename = "Figures/global_historical_ammonium_apr-jun.png", width = 15, height = 8, dpi = 300)

#### July-September ####
dataNH4OS <- readRDS("Data/Climatology/sf/nh4os_historical_jul-sept_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

amm3 <- ggplot() +
  geom_sf(data = dataNH4OS, aes(fill = nh4os_transformed), color = NA, size = 0.01) +
  scale_fill_gradientn(colors = brewer.pal(9, "Reds"),
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

#### October-December ####
dataNH4OS <- readRDS("Data/Climatology/sf/nh4os_historical_oct-dec_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

amm4 <- ggplot() +
  geom_sf(data = dataNH4OS, aes(fill = nh4os_transformed), color = NA, size = 0.01) +
  scale_fill_gradientn(colors = brewer.pal(9, "Reds"),
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

ggsave(plot = amm4, filename = "Figures/global_historical_ammonium_oct-dec.png", width = 15, height = 8, dpi = 300)

#### Full ammonium plot ####
full_amm <- (amm1 + amm2) / (amm3 + amm4) +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = full_amm, filename = "Figures/global_historical_ammonium_full.png", width = 27, height = 15, dpi = 300)