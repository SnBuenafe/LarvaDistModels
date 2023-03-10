# DESCRIPTION: Creating seasonal pH layers

# Load preliminaries
source("00_Preliminaries.R")
input_dir <- here::here("Data", "Climatology", "ensemble")
output_dir <- here::here("Data", "Climatology", "sf")
figure_dir <- here::here("Figures")

# Function to prepare phos layer
create_layer <- function(rs) {
  names(rs) <- paste0("X", seq(1956, 1981, by = 1))  
  
  phos <- rs2sf(rs) %>% 
    dplyr::rename(phos = mean) %>% # using the mean of the models
    sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
    dplyr::as_tibble() %>% 
    dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
    sf::st_as_sf(crs = cCRS) %>% 
    replaceNN(., grid, "phos") %>%
    dplyr::as_tibble() %>% 
    dplyr::select(cellID, phos_transformed, geometry)  
  
}

# Function to prepare plots
create_plot <- function(ggphos) {
  dataPH <- ggphos %>% 
    sf::st_as_sf(sf_column_name = "geometry")
  
  gg <- ggplot() +
    geom_sf(data = dataPH, aes(fill = phos_transformed), color = NA, size = 0.01) +
    scale_fill_gradientn(colors = brewer.pal(9, "RdPu"),
                         na.value = "grey64",
                         guide = guide_colourbar(
                           title.vjust = 0.5,
                           barheight = grid::unit(0.01, "npc"),
                           barwidth = grid::unit(0.25, "npc"),
                           frame.colour = "black")) +
    geom_sf(data = landmass, fill = "black", color = "black") +
    labs(fill = expression('pH')) +
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
phos_rs <- stars::read_ncdf(here::here(input_dir, "phos_historical_1956_1981_jan-mar_ensemble.nc")) %>% 
  terra::rast()
phos <- create_layer(phos_rs)
saveRDS(phos, here::here(output_dir, "phos_historical_jan-mar_interpolated.rds"))

ph1 <- create_plot(phos)

# ii. April-June
phos_rs <- stars::read_ncdf(here::here(input_dir, "phos_historical_1956_1981_apr-jun_ensemble.nc")) %>% 
  terra::rast()
phos <- create_layer(phos_rs)
saveRDS(phos, here::here(output_dir, "phos_historical_apr-jun_interpolated.rds"))

ph2 <- create_plot(phos)

# iii. July-September
phos_rs <- stars::read_ncdf(here::here(input_dir, "phos_historical_1956_1981_jul-sept_ensemble.nc")) %>% 
  terra::rast()
phos <- create_layer(phos_rs)
saveRDS(phos, here::here(output_dir, "phos_historical_jul-sept_interpolated.rds"))

ph3 <- create_plot(phos)

# iv. October-December
phos_rs <- stars::read_ncdf(here::here(input_dir, "phos_historical_1956_1981_oct-dec_ensemble.nc")) %>% 
  terra::rast()
phos <- create_layer(phos_rs)
saveRDS(phos, here::here(output_dir, "phos_historical_oct-dec_interpolated.rds"))

ph4 <- create_plot(phos)

# Full pH plot
full_ph <- (ph1 + ph2) / (ph3 + ph4) +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = full_ph, filename = here::here(figure_dir, "global_historical_ph_full.png"), width = 27, height = 15, dpi = 300)
