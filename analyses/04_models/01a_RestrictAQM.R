source("analyses/02_preliminaries/00_Preliminaries.R")

library(terra)
library(sf)
library(tidyverse)

# Tibble to rename layers since Aquamaps and BRTS use different names
spp <- tibble::tribble(
  ~Abbrev, ~Species,
  "slt", "Allothunnus_fallai", 
  "fri", "Auxis_rochei", 
  "fri", "Auxis_thazard", 
  "sau", "Cololabis_adocetus",
  "sau", "Cololabis_saira",
  "sail", "Istiophorus_platypterus", 
  "strm", "Kajikia_albida",
  "strm", "Kajikia_audax",
  "skp", "Katsuwonus_pelamis",
  "blum", "Makaira_nigricans",
  "sau", "Scomberesox_saurus",
  "lesc", "Scombrolabrax_heterolepis",
  "shos", "Tetrapturus_angustirostris",
  "alb", "Thunnus_alalunga",
  "yft", "Thunnus_albacares",
  "bet", "Thunnus_atlanticus",
  "sbft", "Thunnus_maccoyii", 
  "bet", "Thunnus_obesus",
  "bft", "Thunnus_orientalis",
  "bft", "Thunnus_thynnus",
  "swo", "Xiphias_gladius")



aq <- readRDS("data_input/AquaMaps_sf.rds") %>% 
  # sf::st_as_sf() %>% 
  dplyr::select(tidyselect::all_of(c(spp$Species, "geometry"))) %>% 
  # If there are matches wtih multiple model species we need to merge the columns.
  # I'm taking the highest probablity, rather than the mean or the sum.
  rowwise() %>%
  dplyr::mutate(Auxis_rochei = max(Auxis_rochei, Auxis_thazard, 0, na.rm = TRUE)) %>% 
  dplyr::select(-Auxis_thazard) %>% 
  dplyr::mutate(Cololabis_adocetus = max(Cololabis_adocetus,
                                         Cololabis_saira,
                                         Scomberesox_saurus, 0, na.rm = TRUE)) %>%
  dplyr::select(-c(Cololabis_saira, Scomberesox_saurus)) %>%
  dplyr::mutate(Kajikia_albida = max(Kajikia_audax, Kajikia_albida, Kajikia_audax, 0, na.rm = TRUE)) %>% 
  dplyr::select(-Kajikia_audax) %>% 
  dplyr::mutate(Thunnus_orientalis = max(Thunnus_orientalis, Thunnus_thynnus, 0, na.rm = TRUE)) %>% 
  dplyr::select(-Thunnus_thynnus) %>% 
  dplyr::mutate(Thunnus_atlanticus = max(Thunnus_atlanticus, Thunnus_obesus, 0, na.rm = TRUE)) %>% 
  dplyr::select(-Thunnus_obesus) %>% 
  dplyr::ungroup() %>% 
  # dplyr::mutate(dplyr::across(
  #   -dplyr::any_of(c("cellID", "geometry")), # Apply to all columns except geometry and cellID
  #   ~ dplyr::if_else(. < 0.5, NA, .))) %>% 
  # mutate(across(!geometry, ~ if_else(.x >= median(.x, na.rm = TRUE), .x, NA))) %>%
  mutate(across(!geometry, ~ if_else(.x >= 0.01, .x, NA))) %>%
  # mutate(across(!geometry, ~ if_else(.x > 0, .x, NA))) %>%
  dplyr::mutate(Area_km2 = as.numeric(units::set_units(st_area(.), "km2")))

aq <- aq %>% 
  rename(deframe(spp %>% dplyr::filter(Species %in% colnames(aq)))) %>% 
  pivot_longer(cols = tidyselect::all_of(spp$Abbrev), values_to = "Probability", names_to = "Species", values_drop_na = TRUE) %>% 
  dplyr::mutate(Binary = 1)


cCRS <- "EPSG:4326"

polygon <- sf::st_polygon(x = list(rbind(
  c(-0.0001, 90),
  c(0, 90),
  c(0, -90),
  c(-0.0001, -90),
  c(-0.0001, 90)))) %>%
  sf::st_sfc() %>%
  sf::st_set_crs(cCRS)


aq2 <- aq %>%
  sf::st_transform(cCRS) %>% # The input needs to be unprojected.
  sf::st_make_valid() %>% # Just in case....
  sf::st_difference(polygon) %>%
  sf::st_make_valid() %>%
  sf::st_shift_longitude()



# Get range limits for adults from aquamaps -------------------------------


range_limits <- aq2 %>% 
  sf::st_centroid() %>% 
  mutate(
    Longitude = st_coordinates(.)[, 1],
    Latitude = st_coordinates(.)[, 2],
    Hemisphere = if_else(Longitude <0, "W", "E")
  ) %>% 
  group_by(Species, Hemisphere) %>% 
  summarise(MinLon = min(Longitude), # Need to do it in 2 lots to deal with dateline
            MaxLon = max(Longitude),
            MinLat = min(Latitude),
            MaxLat = max(Latitude)) %>% 
  ungroup() %>% 
  group_by(Species) %>% 
  summarise(MinLon = max(MinLon),
            MaxLon = min(MaxLon),
            MinLat = first(MinLat),
            MaxLat = first(MaxLat)) %>% 
  mutate(MinLat = (round(MinLat*2))/2,
         MaxLat = (round(MaxLat*2))/2) %>%  # round to nearest 0.5 because Lat comes out 0.499
  sf::st_drop_geometry()

write_rds(range_limits, here::here("data_output", "RangeLimits.rds"))
