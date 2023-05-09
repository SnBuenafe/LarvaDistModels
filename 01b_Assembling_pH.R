# DESCRIPTION: Creating seasonal pH layers

# Load preliminaries
source("00_PreparePredictors.R")
label <- "phos_historical"

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
season <- "jan-mar"
phos_rs <- stars::read_ncdf(here::here(input_dir, 
                                      paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
phos <- create_layer(phos_rs)
saveRDS(phos, here::here(output_dir, 
                        paste(label, season, "interpolated.rds", sep = "_"))) # save object
# phos <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

ph1 <- create_plot(phos)

# ii. April-June
season <- "apr-jun"
phos_rs <- stars::read_ncdf(here::here(input_dir, 
                                      paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
phos <- create_layer(phos_rs)
saveRDS(phos, here::here(output_dir, 
                        paste(label, season, "interpolated.rds", sep = "_"))) # save object
# phos <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

ph2 <- create_plot(phos)

# iii. July-September
season <- "jul-sept"
phos_rs <- stars::read_ncdf(here::here(input_dir, 
                                      paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
phos <- create_layer(phos_rs)
saveRDS(phos, here::here(output_dir, 
                        paste(label, season, "interpolated.rds", sep = "_"))) # save object
# phos <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

ph3 <- create_plot(phos)

# iv. October-December
season <- "oct-dec"
phos_rs <- stars::read_ncdf(here::here(input_dir, 
                                      paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
phos <- create_layer(phos_rs)
saveRDS(phos, here::here(output_dir, 
                        paste(label, season, "interpolated.rds", sep = "_"))) # save object
# phos <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

ph4 <- create_plot(phos)

# Full pH plot
full_ph <- (ph1 + ph2) / (ph3 + ph4) +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = full_ph, filename = here::here(figure_dir, "global_historical_ph_full.png"), width = 27, height = 15, dpi = 300)
