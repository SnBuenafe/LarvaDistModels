# DESCRIPTION: Creating seasonal zonal velocity layers

# Load preliminaries
source("00_Preliminaries.R")
input_dir <- here::here("Data", "Climatology", "ensemble")
output_dir <- here::here("Data", "Climatology", "sf")
figure_dir <- here::here("Figures")

# Function to prepare uo layer
create_layer <- function(rs) {
  names(rs) <- paste0("X", seq(1956, 1981, by = 1))  
  
  uo <- rs2sf(rs) %>% 
    dplyr::rename(uo = mean) %>% # using the mean of the models
    sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
    dplyr::as_tibble() %>% 
    dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
    sf::st_as_sf(crs = cCRS) %>% 
    replaceNN(., grid, "uo") %>%
    dplyr::as_tibble() %>% 
    dplyr::select(cellID, uo_transformed, geometry)  
  
}

# Function to prepare plots
create_plot <- function(gguo) {
  dataUO <- gguo %>% 
    sf::st_as_sf(sf_column_name = "geometry")
  
  gg <- ggplot() +
    geom_sf(data = dataUO, aes(fill = uo_transformed), color = NA, size = 0.01) +
    scale_fill_gradientn(colors = rev(brewer.pal(9, "Blues")),
                         na.value = "grey64",
                         oob = scales::squish,
                         guide = guide_colourbar(
                           title.vjust = 0.5,
                           barheight = grid::unit(0.01, "npc"),
                           barwidth = grid::unit(0.25, "npc"),
                           frame.colour = "black")) +
    geom_sf(data = landmass, fill = "black", color = "black") +
    labs(fill = expression('m s'^"-1"*'')) +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.title = element_blank(),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 18),
          panel.border = element_blank()) +
    coord_sf(xlim = st_bbox(grid)$xlim, ylim = st_bbox(grid)$ylim)
}

# i. January-March
uo_rs <- stars::read_ncdf(here::here(input_dir, "uo_historical_1956_1981_jan-mar_ensemble.nc")) %>% 
  terra::rast()
uo <- create_layer(uo_rs)
saveRDS(uo, here::here(output_dir, "uo_historical_jan-mar_interpolated.rds"))

uo1 <- create_plot(uo)

# ii. April-June
uo_rs <- stars::read_ncdf(here::here(input_dir, "uo_historical_1956_1981_apr-jun_ensemble.nc")) %>% 
  terra::rast()
uo <- create_layer(uo_rs)
saveRDS(uo, here::here(output_dir, "uo_historical_apr-jun_interpolated.rds"))

uo2 <- create_plot(uo)

# iii. July-September
uo_rs <- stars::read_ncdf(here::here(input_dir, "uo_historical_1956_1981_jul-sept_ensemble.nc")) %>% 
  terra::rast()
uo <- create_layer(uo_rs)
saveRDS(uo, here::here(output_dir, "uo_historical_jul-sept_interpolated.rds"))

uo3 <- create_plot(uo)

# iv. October-December
uo_rs <- stars::read_ncdf(here::here(input_dir, "uo_historical_1956_1981_oct-dec_ensemble.nc")) %>% 
  terra::rast()
uo <- create_layer(uo_rs)
saveRDS(uo, here::here(output_dir, "uo_historical_oct-dec_interpolated.rds"))

uo4 <- create_plot(uo)

# Full zonal velocity plots
full_uo <- (uo1 + uo2) / (uo3 + uo4) +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = full_uo, filename = here::here(figure_dir, "global_historical_uo_full.png"), width = 27, height = 15, dpi = 300)
