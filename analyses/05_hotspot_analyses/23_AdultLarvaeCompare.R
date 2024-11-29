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
  mutate(across(!geometry, ~ if_else(.x >= 0.01, .x, NA))) %>%
  dplyr::mutate(Area_km2 = as.numeric(units::set_units(st_area(.), "km2")))

aq <- aq %>% 
  rename(deframe(spp %>% dplyr::filter(Species %in% colnames(aq)))) %>% 
  pivot_longer(cols = tidyselect::all_of(spp$Abbrev), values_to = "Probability", names_to = "Species", values_drop_na = TRUE)


# Create a dataframe of the area of each species and calculate proportion
# 
# summary_df <- function(name, dat_m, dat_a){
#   
#   out <- tibble(Species = name,
#                 Adult_km2 = dat_a %>% filter(Species %in% name) %>% pull(Area_km2) %>% sum(),
#                 Larval_km2 = dat_m %>% filter(Species %in% name) %>% pull(Area_km2) %>% sum(),
#                 LarvalProp = (round((Larval_km2 / Adult_km2)*100))/100)
#   
# }
# 
# out <- purrr::map_df(unique(spp$Abbrev), summary_df, dat_a = aq, dat_m = m)
# 
# write.csv(out, file = "Output/LarvalAreaProportion.csv")
# 


# Now do some plots

## Make it binary

aq <- aq %>% 
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



# Modify world dataset to remove overlapping portions with world's polygons
landmass <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  sf::st_transform(cCRS) %>% # The input needs to be unprojected.
  sf::st_make_valid() %>% # Just in case....
  sf::st_difference(polygon) %>%
  sf::st_make_valid() %>%
  sf::st_shift_longitude()


colours <- c("Adult" = "blue", "Larvae" = "red", "Nishikawa" = "black")

plot_AdultLarval <- function(sp, season, aq2, range_limits, landmass){
  
  m <- terra::rast(file.path(rast_dir,paste0("ModelOutputs_", season, ".tif"))) %>% 
    terra::as.polygons(trunc = FALSE, dissolve = FALSE, na.rm = TRUE, na.all = TRUE, round = FALSE) %>%
    sf::st_as_sf() %>% 
    # Set all values below the median of the column to NA
    # mutate(across(!geometry, ~ if_else(.x >= median(.x, na.rm = TRUE), .x, NA))) %>%
    # mutate(across(!geometry, ~ if_else(.x > 0, .x, NA))) %>%
    dplyr::mutate(Area_km2 = as.numeric(units::set_units(st_area(.), "km2"))) %>% 
    tidyr::pivot_longer(cols = tidyselect::all_of(unique(spp$Abbrev)), values_to = "Probability", names_to = "Species", values_drop_na = TRUE)
  
  
  # Modify world dataset to remove overlapping portions with world's polygons
  m2 <- m %>%
    sf::st_transform(cCRS) %>% # The input needs to be unprojected.
    sf::st_make_valid() %>% # Just in case....
    sf::st_difference(polygon) %>%
    sf::st_make_valid() %>%
    sf::st_shift_longitude()
  
  files <- list.files(file.path("data_input", "fish"), pattern = season, full.names = TRUE)
  
  obs <- purrr::map(files, readRDS) %>% 
    bind_rows() %>% 
    filter(abundance > 0) %>% 
    sf::st_drop_geometry() %>%
    dplyr::rename(Species = species) %>% 
    group_by(Species, latitude, longitude) %>% 
    summarise(abundance = 1) %>% 
    sf::st_as_sf(coords = c("longitude", "latitude")) %>% 
    mutate(Species = case_match(
      Species,
      "albacore" ~ "alb",
      "bigeye-tuna" ~ "bet",
      "black-marlin" ~ NA_character_,
      "blue-marlin" ~  "blum",
      "bluefin-tuna" ~ "bft",
      "bonitos" ~ NA_character_,              
      "frigate-tuna" ~ "fri",
      "little-tuna" ~ NA_character_,
      "longfin-escolar" ~ "lesc",
      "sailfish" ~ "sail",
      "sauries" ~ "sau",
      "shortbill-spearfish" ~ "shos",
      "skipjack-tuna" ~ "skp",
      "slender-tuna" ~ "slt",
      "southern-bluefin-tuna" ~ "sbft",
      "striped-marlin" ~ "strm",
      "swordfish" ~ "swo", 
      "yellowfin-tuna" ~ "yft")) %>% 
    drop_na("Species") %>% 
    sf::st_set_crs(cCRS) %>% 
    sf::st_make_valid() %>% # Just in case....
    sf::st_difference(polygon) %>%
    sf::st_make_valid() %>%
    sf::st_shift_longitude()
  
  
  ggplot() + 
    geom_sf(data = landmass, fill = "grey60") +
    geom_sf(data = aq2 %>% filter(Species == sp), aes(fill = "Adult"), colour = "blue", linewidth = 0.00001, alpha = 0.5, show.legend = TRUE) +
    geom_sf(data = m2 %>% filter(Species == sp), aes(fill = "Larvae"), colour = "red", linewidth = 0.00001, alpha = 0.5, show.legend = TRUE) +
    geom_sf(data = obs %>% filter(Species == sp), aes(fill = "Nishikawa"), linewidth = 0.00001, size = 0.0000001, alpha = 1, shape = 20) +
    theme_bw(base_size = 18) +
    theme(legend.title = element_blank(), axis.title = element_blank()) +
    scale_fill_manual(values = colours, 
                      aesthetics = c("fill"),
                      guide = ggplot2::guide_legend(
                        override.aes = list(colour = NA, 
                                            linewidth = 0.000001),
                        direction = "horizontal"
                      )
    ) +
    scale_x_continuous(expand = c(0,0), limits = c(40, 290)) +
    scale_y_continuous(expand = c(0,0), limits = c(-40, 40)) +
    geom_label(data = data.frame(x = 55, y = 30, label = sp), aes(x = x, y = y, label = label), size = 12)
  
}

plot_AdultLarval(spp$Abbrev[1], seasons[1], aq2, range_limits, landmass)


seasons = c("jan-mar", "apr-jun", "jul-sep", "oct-dec")

gg1 <- purrr::map(sort(unique(spp$Abbrev)), plot_AdultLarval, seasons[1], aq2, range_limits, landmass)
gg2 <- purrr::map(sort(unique(spp$Abbrev)), plot_AdultLarval, seasons[2], aq2, range_limits, landmass)
gg3 <- purrr::map(sort(unique(spp$Abbrev)), plot_AdultLarval, seasons[3], aq2, range_limits, landmass)
gg4 <- purrr::map(sort(unique(spp$Abbrev)), plot_AdultLarval, seasons[4], aq2, range_limits, landmass)


ggsave(file.path("Figures", "AdultLarval", "AdultLarvalMaps1.pdf"), 
       patchwork::wrap_plots(gg1, ncol = 4, guides = "collect") + patchwork::guide_area(), 
       width = 21*4, height = 11*4, units = "cm")
ggsave(file.path("Figures", "AdultLarval", "AdultLarvalMaps2.pdf"), 
       patchwork::wrap_plots(gg2, ncol = 4, guides = "collect") + patchwork::guide_area(),
       width = 21*4, height = 11*4, units = "cm")
ggsave(file.path("Figures", "AdultLarval", "AdultLarvalMaps3.pdf"), 
       patchwork::wrap_plots(gg3, ncol = 4, guides = "collect") + patchwork::guide_area(),
       width = 21*4, height = 11*4, units = "cm")
ggsave(file.path("Figures", "AdultLarval", "AdultLarvalMaps4.pdf"), 
       patchwork::wrap_plots(gg4, ncol = 4, guides = "collect") + patchwork::guide_area(),
       width = 21*4, height = 11*4, units = "cm")


# These plots don't work as intended because they seem to ignore the `byrow` argument

# p <- list(patchwork::wrap_plots(gg1, ncol = 1, guides = "collect") + patchwork::guide_area(),
#      patchwork::wrap_plots(gg2, ncol = 1, guides = "collect") + patchwork::guide_area(),
#      patchwork::wrap_plots(gg3, ncol = 1, guides = "collect") + patchwork::guide_area(),
#      patchwork::wrap_plots(gg4, ncol = 1, guides = "collect") + patchwork::guide_area())

# gg <- c(gg1, gg2, gg3, gg4)

# p <- patchwork::wrap_plots(gg, nrow = 16, ncol = 4, byrow = FALSE, guides = "collect") + patchwork::guide_area()

# ggsave(file.path("Figures", "AdultLarval", "AdultLarvalMaps_All.pdf"), p, width = 11*4, height = 21*4, units = "cm")

