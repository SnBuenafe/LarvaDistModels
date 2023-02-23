# DESCRIPTION: Calculate eddy kinetic energy (in m^2/s^2) and current velocity (in m/s)

# Load preliminaries
source("00_Utils.R")

# i. January-March
uo <- readRDS("Data/Climatology/sf/uo_historical_jan-mar_interpolated.rds")
vo <- readRDS("Data/Climatology/sf/vo_historical_jan-mar_interpolated.rds")

comb <- dplyr::left_join(uo, vo) %>% 
  dplyr::select(cellID, uo_transformed, vo_transformed, geometry) %>%  # arrange columns
  dplyr::mutate(vel = sqrt(uo_transformed^2 + vo_transformed^2), # calculate current velocity
                eke = 0.5*(vel)) %>% # calculate EKE
  dplyr::select(cellID, vel, eke, geometry)

saveRDS(comb, "Data/Climatology/sf/mesoscale_features_historical_jan-mar_interpolated.rds")

# ii. April-June
uo <- readRDS("Data/Climatology/sf/uo_historical_apr-jun_interpolated.rds")
vo <- readRDS("Data/Climatology/sf/vo_historical_apr-jun_interpolated.rds")

comb <- dplyr::left_join(uo, vo) %>% 
  dplyr::select(cellID, uo_transformed, vo_transformed, geometry) %>%  # arrange columns
  dplyr::mutate(vel = sqrt(uo_transformed^2 + vo_transformed^2), # calculate current velocity
                eke = 0.5*(vel)) %>% # calculate EKE
  dplyr::select(cellID, vel, eke, geometry)

saveRDS(comb, "Data/Climatology/sf/mesoscale_features_historical_apr-jun_interpolated.rds")

# iii. July-September
uo <- readRDS("Data/Climatology/sf/uo_historical_jul-sept_interpolated.rds")
vo <- readRDS("Data/Climatology/sf/vo_historical_jul-sept_interpolated.rds")

comb <- dplyr::left_join(uo, vo) %>% 
  dplyr::select(cellID, uo_transformed, vo_transformed, geometry) %>%  # arrange columns
  dplyr::mutate(vel = sqrt(uo_transformed^2 + vo_transformed^2), # calculate current velocity
                eke = 0.5*(vel)) %>% # calculate EKE
  dplyr::select(cellID, vel, eke, geometry)

saveRDS(comb, "Data/Climatology/sf/mesoscale_features_historical_jul-sept_interpolated.rds")

# iv. October-December
uo <- readRDS("Data/Climatology/sf/uo_historical_oct-dec_interpolated.rds")
vo <- readRDS("Data/Climatology/sf/vo_historical_oct-dec_interpolated.rds")

comb <- dplyr::left_join(uo, vo) %>% 
  dplyr::select(cellID, uo_transformed, vo_transformed, geometry) %>%  # arrange columns
  dplyr::mutate(vel = sqrt(uo_transformed^2 + vo_transformed^2), # calculate current velocity
                eke = 0.5*(vel)) %>% # calculate EKE
  dplyr::select(cellID, vel, eke, geometry)

saveRDS(comb, "Data/Climatology/sf/mesoscale_features_historical_oct-dec_interpolated.rds")

#### Plot: EKE & Currents ####

# January-March
dataMeso <- readRDS("Data/Climatology/sf/mesoscale_features_historical_jan-mar_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

eke1 <- ggplot() +
  geom_sf(data = dataMeso, aes(fill = eke), color = NA, size = 0.01) +
  scale_fill_gradientn(colors = rev(brewer.pal(9, "Blues")),
                       na.value = "grey64",
                       oob = scales::squish,
                       guide = guide_colourbar(
                         title.vjust = 0.5,
                         barheight = grid::unit(0.01, "npc"),
                         barwidth = grid::unit(0.25, "npc"),
                         frame.colour = "black")) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('m'^"2"*'s'^"-2"*'')) +
  theme_bw() +
  gg_add_text(., "white")

ggsave(plot = eke1, filename = "Figures/global_historical_eke_jan-mar.png", width = 15, height = 8, dpi = 300)

vel1 <- ggplot() +
  geom_sf(data = dataMeso, aes(fill = vel), color = NA, size = 0.01) +
  scale_fill_cmocean(name = "matter",
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
  labs(fill = expression('m'*' s'^"-1"*'')) +
  theme_bw() +
  gg_add_text(., "white")

ggsave(plot = vel1, filename = "Figures/global_historical_vel_jan-mar.png", width = 15, height = 8, dpi = 300)

# April-June
dataMeso <- readRDS("Data/Climatology/sf/mesoscale_features_historical_apr-jun_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

eke2 <- ggplot() +
  geom_sf(data = dataMeso, aes(fill = eke), color = NA, size = 0.01) +
  scale_fill_gradientn(colors = rev(brewer.pal(9, "Blues")),
                       na.value = "grey64",
                       oob = scales::squish,
                       guide = guide_colourbar(
                         title.vjust = 0.5,
                         barheight = grid::unit(0.01, "npc"),
                         barwidth = grid::unit(0.25, "npc"),
                         frame.colour = "black")) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('m'^"2"*'s'^"-2"*'')) +
  theme_bw() +
  gg_add_text(., "white")

ggsave(plot = eke2, filename = "Figures/global_historical_eke_apr-jun.png", width = 15, height = 8, dpi = 300)

vel2 <- ggplot() +
  geom_sf(data = dataMeso, aes(fill = vel), color = NA, size = 0.01) +
  scale_fill_cmocean(name = "matter",
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
  labs(fill = expression('m'*' s'^"-1"*'')) +
  theme_bw() +
  gg_add_text(., "white")

ggsave(plot = vel2, filename = "Figures/global_historical_vel_apr-jun.png", width = 15, height = 8, dpi = 300)

# July-September
dataMeso <- readRDS("Data/Climatology/sf/mesoscale_features_historical_jul-sept_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

eke3 <- ggplot() +
  geom_sf(data = dataMeso, aes(fill = eke), color = NA, size = 0.01) +
  scale_fill_gradientn(colors = rev(brewer.pal(9, "Blues")),
                       na.value = "grey64",
                       oob = scales::squish,
                       guide = guide_colourbar(
                         title.vjust = 0.5,
                         barheight = grid::unit(0.01, "npc"),
                         barwidth = grid::unit(0.25, "npc"),
                         frame.colour = "black")) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('m'^"2"*'s'^"-2"*'')) +
  theme_bw() +
  gg_add_text(., "white")

ggsave(plot = eke3, filename = "Figures/global_historical_eke_jul-sept.png", width = 15, height = 8, dpi = 300)

vel3 <- ggplot() +
  geom_sf(data = dataMeso, aes(fill = vel), color = NA, size = 0.01) +
  scale_fill_cmocean(name = "matter",
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
  labs(fill = expression('m'*' s'^"-1"*'')) +
  theme_bw() +
  gg_add_text(., "white")

ggsave(plot = vel3, filename = "Figures/global_historical_vel_jul-sept.png", width = 15, height = 8, dpi = 300)

# October-December
dataMeso <- readRDS("Data/Climatology/sf/mesoscale_features_historical_oct-dec_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor()

eke4 <- ggplot() +
  geom_sf(data = dataMeso, aes(fill = eke), color = NA, size = 0.01) +
  scale_fill_gradientn(colors = rev(brewer.pal(9, "Blues")),
                       na.value = "grey64",
                       oob = scales::squish,
                       guide = guide_colourbar(
                         title.vjust = 0.5,
                         barheight = grid::unit(0.01, "npc"),
                         barwidth = grid::unit(0.25, "npc"),
                         frame.colour = "black")) +
  geom_sf(data = landmass, fill = "black", color = "black") +
  labs(fill = expression('m'^"2"*'s'^"-2"*'')) +
  theme_bw() +
  gg_add_text(., "white")

ggsave(plot = eke4, filename = "Figures/global_historical_eke_oct-dec.png", width = 15, height = 8, dpi = 300)

vel4 <- ggplot() +
  geom_sf(data = dataMeso, aes(fill = vel), color = NA, size = 0.01) +
  scale_fill_cmocean(name = "matter",
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
  labs(fill = expression('m'*' s'^"-1"*'')) +
  theme_bw() +
  gg_add_text(., "white")

ggsave(plot = vel4, filename = "Figures/global_historical_vel_oct-dec.png", width = 15, height = 8, dpi = 300)

# Full mesoscale features plots
full_eke <- (eke1 + eke2) / (eke3 + eke4) +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = full_eke, filename = "Figures/global_historical_eke_full.png", width = 27, height = 15, dpi = 300)

full_vel <- (vel1 + vel2) / (vel3 + vel4) +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = full_vel, filename = "Figures/global_historical_vel_full.png", width = 27, height = 15, dpi = 300)
