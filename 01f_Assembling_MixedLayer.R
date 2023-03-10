# DESCRIPTION: Creating seasonal mixed layer thickness layers

# Load preliminaries
source("00_Preliminaries.R")
input_dir <- here::here("Data", "Climatology", "ensemble")
output_dir <- here::here("Data", "Climatology", "sf")
figure_dir <- here::here("Figures")

# Function to prepare mlotst layer
create_layer <- function(rs) {
  names(rs) <- paste0("X", seq(1956, 1981, by = 1))  
  
  mlotst <- rs2sf(rs) %>% 
    dplyr::rename(mlotst = mean) %>% # using the mean of the models
    sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
    dplyr::as_tibble() %>% 
    dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
    sf::st_as_sf(crs = cCRS) %>% 
    replaceNN(., grid, "mlotst") %>%
    dplyr::as_tibble() %>% 
    dplyr::select(cellID, mlotst_transformed, geometry)  
  
}

# Function to prepare plots
create_plot <- function(ggmlotst) {
  dataMixed <- ggmlotst %>% 
    sf::st_as_sf(sf_column_name = "geometry")
  
  gg <- ggplot() +
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
    theme(legend.position = "bottom",
          axis.title = element_blank(),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 18),
          panel.border = element_blank()) +
    coord_sf(xlim = st_bbox(grid)$xlim, ylim = st_bbox(grid)$ylim)
}

# i. January-March
mlotst_rs <- stars::read_ncdf(here::here(input_dir, "mlotst_historical_1956_1981_jan-mar_ensemble.nc")) %>% 
  terra::rast()
mlotst <- create_layer(mlotst_rs)
saveRDS(mlotst, here::here(output_dir, "mlotst_historical_jan-mar_interpolated.rds"))

mix1 <- create_plot(mlotst)

# ii. April-June
mlotst_rs <- stars::read_ncdf(here::here(input_dir, "mlotst_historical_1956_1981_apr-jun_ensemble.nc")) %>% 
  terra::rast()
mlotst <- create_layer(mlotst_rs)
saveRDS(mlotst, here::here(output_dir, "mlotst_historical_apr-jun_interpolated.rds"))

mix2 <- create_plot(mlotst)

# iii. July-September
mlotst_rs <- stars::read_ncdf(here::here(input_dir, "mlotst_historical_1956_1981_jul-sept_ensemble.nc")) %>% 
  terra::rast()
mlotst <- create_layer(mlotst_rs)
saveRDS(mlotst, here::here(output_dir, "mlotst_historical_jul-sept_interpolated.rds"))

mix3 <- create_plot(mlotst)

# iv. October-December
mlotst_rs <- stars::read_ncdf(here::here(input_dir, "mlotst_historical_1956_1981_oct-dec_ensemble.nc")) %>% 
  terra::rast()
mlotst <- create_layer(mlotst_rs)
saveRDS(mlotst, here::here(output_dir, "mlotst_historical_oct-dec_interpolated.rds"))

mix4 <- create_plot(mlotst)

# Full mixed layer depth plot
full_mix <- (mix1 + mix2) / (mix3 + mix4) +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = full_mix, filename = "Figures/global_historical_mixed_layer_full.png", width = 27, height = 15, dpi = 300)
