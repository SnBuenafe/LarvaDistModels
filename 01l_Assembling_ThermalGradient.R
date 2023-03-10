# DESCRIPTION: Calculate broad-scale thermal gradients

# Load preliminaries
source("00_Preliminaries.R")
input_dir <- here::here("Data", "Climatology", "ensemble")
output_dir <- here::here("Data", "Climatology", "sf")
figure_dir <- here::here("Figures")

# Function to prepare thermal gradient layer
create_layer <- function(rs) {
  names(rs) <- names(rs) %>% 
    substr(., 1, 5)
  
  # Calculate thermal spatial gradient
  grad_rs <- VoCC::spatGrad(rs, th = 0.0001, projected = FALSE) %>% 
    terra::rast() 
  
  grad <- spat2sf(grad_rs) %>% 
    dplyr::rename(thermal_front = Grad,
                  angle = Ang) %>% 
    sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
    dplyr::as_tibble() %>% 
    dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
    sf::st_as_sf(crs = moll_pacific) %>% 
    replaceNN(., grid, "thermal_front") %>%
    replaceNN(., grid, "angle") %>% 
    dplyr::as_tibble() %>% 
    dplyr::select(cellID, thermal_front_transformed, angle_transformed, geometry)
  
}

# Function to prepare plots
create_plot <- function(ggthermal) {
  dataThermal <- ggthermal %>% 
    sf::st_as_sf(sf_column_name = "geometry")
  
  gg <- ggplot() +
    geom_sf(data = dataThermal, aes(fill = thermal_front_transformed), color = NA, size = 0.01) +
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
    labs(fill = expression(''^"o"*'C km'^"-1")) +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.title = element_blank(),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 18),
          panel.border = element_blank()) +
    coord_sf(xlim = st_bbox(grid)$xlim, ylim = st_bbox(grid)$ylim)
}


# i. January-March
rs <- raster::stack(here::here(input_dir, "tos_historical_1956_1981_jan-mar_ensemble.nc"))
grad <- create_layer(rs)
saveRDS(grad, here::here(output_dir, "thermal_front_historical_jan-mar_interpolated.rds"))

tf1 <- create_plot(grad)

# ii. April-June
rs <- raster::stack(here::here(input_dir, "tos_historical_1956_1981_apr-jun_ensemble.nc"))
grad <- create_layer(rs)
saveRDS(grad, here::here(output_dir, "thermal_front_historical_apr-jun_interpolated.rds"))

tf2 <- create_plot(grad)

# iii. July-September
rs <- raster::stack(here::here(input_dir, "tos_historical_1956_1981_jul-sept_ensemble.nc"))
grad <- create_layer(rs)
saveRDS(grad, here::here(output_dir, "thermal_front_historical_jul-sept_interpolated.rds"))

tf3 <- create_plot(grad)

# iv. October-December
rs <- raster::stack(here::here(input_dir, "tos_historical_1956_1981_oct-dec_ensemble.nc"))
grad <- create_layer(rs)
saveRDS(grad, here::here(output_dir, "thermal_front_historical_oct-dec_interpolated.rds"))

tf4 <- create_plot(grad)

# Full thermal front plots
full_tf <- (tf1 + tf2) / (tf3 + tf4) +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = full_tf, filename = here::here(figure_dir, "global_historical_thermal_front_full.png"), width = 27, height = 15, dpi = 300)