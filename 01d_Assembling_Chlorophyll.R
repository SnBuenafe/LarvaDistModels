# DESCRIPTION: Creating seasonal chlorophyll layers

# Load preliminaries
source("00_Preliminaries.R")
input_dir <- here::here("Data", "Climatology", "ensemble")
output_dir <- here::here("Data", "Climatology", "sf")
figure_dir <- here::here("Figures")

# Function to prepare chlos layer
create_layer <- function(rs) {
  names(rs) <- paste0("X", seq(1956, 1981, by = 1))  
  
  chlos <- rs2sf(rs) %>% 
    dplyr::rename(chlos = mean) %>% # using the mean of the models
    sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
    dplyr::as_tibble() %>% 
    dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
    sf::st_as_sf(crs = cCRS) %>% 
    replaceNN(., grid, "chlos") %>%
    dplyr::as_tibble() %>% 
    dplyr::select(cellID, chlos_transformed, geometry)  
  
}

# Function to prepare plots
create_plot <- function(ggchlos) {
  dataChlorophyll <- ggchlos %>% 
    sf::st_as_sf(sf_column_name = "geometry")
  
  gg <- ggplot() +
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
    theme(legend.position = "bottom",
          axis.title = element_blank(),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 18),
          panel.border = element_blank()) +
    coord_sf(xlim = st_bbox(grid)$xlim, ylim = st_bbox(grid)$ylim)
}

# i. January-March
chlos_rs <- stars::read_ncdf(here::here(input_dir, "chlos_historical_1956_1981_jan-mar_ensemble.nc")) %>% 
  terra::rast()
chlos <- create_layer(chlos_rs)
saveRDS(chlos, here::here(output_dir, "chlos_historical_jan-mar_interpolated.rds"))

chl1 <- create_plot(chlos)

# ii. April-June
chlos_rs <- stars::read_ncdf(here::here(input_dir, "chlos_historical_1956_1981_apr-jun_ensemble.nc")) %>% 
  terra::rast()
chlos <- create_layer(chlos_rs)
saveRDS(chlos, here::here(output_dir, "chlos_historical_apr-jun_interpolated.rds"))

chl2 <- create_plot(chlos)

# iii. July-September
chlos_rs <- stars::read_ncdf(here::here(input_dir, "chlos_historical_1956_1981_jul-sept_ensemble.nc")) %>% 
  terra::rast()
chlos <- create_layer(chlos_rs)
saveRDS(chlos, here::here(output_dir, "chlos_historical_jul-sept_interpolated.rds"))

chl3 <- create_plot(chlos)

# iv. October-December
chlos_rs <- stars::read_ncdf(here::here(input_dir, "chlos_historical_1956_1981_oct-dec_ensemble.nc")) %>% 
  terra::rast()
chlos <- create_layer(chlos_rs)
saveRDS(chlos, here::here(output_dir, "chlos_historical_oct-dec_interpolated.rds"))

chl4 <- create_plot(chlos)

# Full chlorophyll plot
full_chl <- (chl1 + chl2) / (chl3 + chl4) +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = full_chl, filename = here::here(figure_dir, "global_historical_chlorophyll_full.png"), width = 27, height = 15, dpi = 300)

