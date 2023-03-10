# DESCRIPTION: Creating seasonal salinity layers

# Load preliminaries
source("00_Preliminaries.R")
input_dir <- here::here("Data", "Climatology", "ensemble")
output_dir <- here::here("Data", "Climatology", "sf")
figure_dir <- here::here("Figures")

# Function to prepare sos layer
create_layer <- function(rs) {
  names(rs) <- paste0("X", seq(1956, 1981, by = 1))  
  
  sos <- rs2sf(rs) %>% 
    dplyr::rename(sos = mean) %>% # using the mean of the models
    sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
    dplyr::as_tibble() %>% 
    dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
    sf::st_as_sf(crs = cCRS) %>% 
    replaceNN(., grid, "sos") %>%
    dplyr::as_tibble() %>% 
    dplyr::select(cellID, sos_transformed, geometry)  
  
}

# Function to prepare plots
create_plot <- function(ggsos) {
  dataSalinity <- ggsos %>% 
    sf::st_as_sf(sf_column_name = "geometry")
  
  gg <- ggplot() +
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
    theme(legend.position = "bottom",
          axis.title = element_blank(),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 18),
          panel.border = element_blank()) +
    coord_sf(xlim = st_bbox(grid)$xlim, ylim = st_bbox(grid)$ylim)
}

# i. January-March
sos_rs <- stars::read_ncdf(here::here(input_dir, "sos_historical_1956_1981_jan-mar_ensemble.nc")) %>% 
  terra::rast()
sos <- create_layer(sos_rs)
saveRDS(sos, here::here(output_dir, "sos_historical_jan-mar_interpolated.rds"))

sal1 <- create_plot(sos)

# ii. April-June
sos_rs <- stars::read_ncdf(here::here(input_dir, "sos_historical_1956_1981_apr-jun_ensemble.nc")) %>% 
  terra::rast()
sos <- create_layer(sos_rs)
saveRDS(sos, here::here(output_dir, "sos_historical_apr-jun_interpolated.rds"))

sal2 <- create_plot(sos)

# iii. July-September
sos_rs <- stars::read_ncdf(here::here(input_dir, "sos_historical_1956_1981_jul-sept_ensemble.nc")) %>% 
  terra::rast()
sos <- create_layer(sos_rs)
saveRDS(sos, here::here(output_dir, "sos_historical_jul-sept_interpolated.rds"))

sal3 <- create_plot(sos)

# iv. October-December
sos_rs <- stars::read_ncdf(here::here(input_dir, "sos_historical_1956_1981_oct-dec_ensemble.nc")) %>% 
  terra::rast()
sos <- create_layer(sos_rs)
saveRDS(sos, here::here(output_dir, "sos_historical_oct-dec_interpolated.rds"))

sal4 <- create_plot(sos)

# Full salinity plot
full_sal <- (sal1 + sal2) / (sal3 + sal4) +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = full_sal, filename = here::here(figure_dir, "global_historical_salinity_full.png"), width = 27, height = 15, dpi = 300)