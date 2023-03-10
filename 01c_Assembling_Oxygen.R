# DESCRIPTION: Creating seasonal oxygen layers

# Load preliminaries
source("00_Preliminaries.R")
input_dir <- here::here("Data", "Climatology", "ensemble")
output_dir <- here::here("Data", "Climatology", "sf")
figure_dir <- here::here("Figures")

# Function to prepare o2os layer
create_layer <- function(rs) {
  names(rs) <- paste0("X", seq(1956, 1981, by = 1))  
  
  o2os <- rs2sf(rs) %>% 
    dplyr::rename(o2os = mean) %>% # using the mean of the models
    sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
    dplyr::as_tibble() %>% 
    dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
    sf::st_as_sf(crs = cCRS) %>% 
    replaceNN(., grid, "o2os") %>%
    dplyr::as_tibble() %>% 
    dplyr::select(cellID, o2os_transformed, geometry)  
  
}

# Function to prepare plots
create_plot <- function(ggo2os) {
  dataO2 <- ggo2os %>% 
    sf::st_as_sf(sf_column_name = "geometry")
  
  gg <- ggplot() +
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
    theme(legend.position = "bottom",
          axis.title = element_blank(),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 18),
          panel.border = element_blank()) +
    coord_sf(xlim = st_bbox(grid)$xlim, ylim = st_bbox(grid)$ylim)
}

#### Create layers ####

# i. January-March
o2os_rs <- stars::read_ncdf(here::here(input_dir, "o2os_historical_1956_1981_jan-mar_ensemble.nc")) %>% 
  terra::rast()
o2os <- create_layer(o2os_rs)
saveRDS(o2os, here::here(output_dir, "o2os_historical_jan-mar_interpolated.rds"))

ox1 <- create_plot(o2os)

# ii. April-June
o2os_rs <- stars::read_ncdf(here::here(input_dir, "o2os_historical_1956_1981_apr-jun_ensemble.nc")) %>% 
  terra::rast()
o2os <- create_layer(o2os_rs)
saveRDS(o2os, here::here(output_dir, "o2os_historical_apr-jun_interpolated.rds"))

ox2 <- create_plot(o2os)

# iii. July-September
o2os_rs <- stars::read_ncdf(here::here(input_dir, "o2os_historical_1956_1981_jul-sept_ensemble.nc")) %>% 
  terra::rast()
o2os <- create_layer(o2os_rs)
saveRDS(o2os, here::here(output_dir, "o2os_historical_jul-sept_interpolated.rds"))

ox3 <- create_plot(o2os)

# iv. October-December
o2os_rs <- stars::read_ncdf(here::here(input_dir, "o2os_historical_1956_1981_oct-dec_ensemble.nc")) %>% 
  terra::rast()
o2os <- create_layer(o2os_rs)
saveRDS(o2os, here::here(output_dir, "o2os_historical_oct-dec_interpolated.rds"))

ox4 <- create_plot(o2os)

# Full oxygen plot
full_ox <- (ox1 + ox2) / (ox3 + ox4) +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = full_ox, filename = here::here(figure_dir, "global_historical_oxygen_full.png"), width = 27, height = 15, dpi = 300)
