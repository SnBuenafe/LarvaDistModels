# DESCRIPTION: Creating seasonal nitrate layers

# Load preliminaries
source("00_Preliminaries.R")
input_dir <- here::here("Data", "Climatology", "ensemble")
output_dir <- here::here("Data", "Climatology", "sf")
figure_dir <- here::here("Figures")

# Function to prepare mlotst layer
create_layer <- function(rs) {
  names(rs) <- paste0("X", seq(1956, 1981, by = 1))  
  
  no3os <- rs2sf(rs) %>% 
    dplyr::rename(no3os = mean) %>% # using the mean of the models
    sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
    dplyr::as_tibble() %>% 
    dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
    sf::st_as_sf(crs = cCRS) %>% 
    replaceNN(., grid, "no3os") %>%
    dplyr::as_tibble() %>% 
    dplyr::select(cellID, no3os_transformed, geometry)  
  
}

# Function to prepare plots
create_plot <- function(ggno3os) {
  dataNO3 <- ggno3os %>% 
    sf::st_as_sf(sf_column_name = "geometry")
  
  gg <- ggplot() +
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
    theme(legend.position = "bottom",
          axis.title = element_blank(),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 18),
          panel.border = element_blank()) +
    coord_sf(xlim = st_bbox(grid)$xlim, ylim = st_bbox(grid)$ylim)
}

# i. January-March
no3os_rs <- stars::read_ncdf(here::here(input_dir, "no3os_historical_1956_1981_jan-mar_ensemble.nc")) %>% 
  terra::rast()
no3os <- create_layer(no3os_rs)
saveRDS(no3os, here::here(output_dir, "no3os_historical_jan-mar_interpolated.rds"))

nit1 <- create_plot(no3os)

# ii. April-June
no3os_rs <- stars::read_ncdf(here::here(input_dir, "no3os_historical_1956_1981_apr-jun_ensemble.nc")) %>% 
  terra::rast()
no3os <- create_layer(no3os_rs)
saveRDS(no3os, here::here(output_dir, "no3os_historical_apr-jun_interpolated.rds"))

nit2 <- create_plot(no3os)

# iii. July-September
no3os_rs <- stars::read_ncdf(here::here(input_dir, "no3os_historical_1956_1981_jul-sept_ensemble.nc")) %>% 
  terra::rast()
no3os <- create_layer(no3os_rs)
saveRDS(no3os, here::here(output_dir, "no3os_historical_jul-sept_interpolated.rds"))

nit3 <- create_plot(no3os)

# iv. October-December
no3os_rs <- stars::read_ncdf(here::here(input_dir, "no3os_historical_1956_1981_oct-dec_ensemble.nc")) %>% 
  terra::rast()
no3os <- create_layer(no3os_rs)
saveRDS(no3os, here::here(output_dir, "no3os_historical_oct-dec_interpolated.rds"))

nit4 <- create_plot(no3os)

# Full nitrate plot
full_nit <- (nit1 + nit2) / (nit3 + nit4) +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = full_nit, filename = here::here(figure_dir, "global_historical_nitrate_full.png"), width = 27, height = 15, dpi = 300)
