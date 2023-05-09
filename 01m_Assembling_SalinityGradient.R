# DESCRIPTION: Calculate broad-scale salinity gradients

# Load preliminaries
source("00_PreparePredictors.R")
pacman::p_load(raster, VoCC)
input <- "sos_historical"
label <- "salinity_front_historical"

# Function to prepare thermal gradient layer
create_layer <- function(rs) {
  names(rs) <- names(rs) %>% 
    substr(., 1, 5)
  
  # Calculate thermal spatial gradient
  grad_rs <- VoCC::spatGrad(rs, th = 0.0001, projected = FALSE) %>% 
    terra::rast() 
  
  grad <- spat2sf(grad_rs) %>% 
    dplyr::rename(salinity_front = Grad,
                  angle = Ang) %>% 
    sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
    dplyr::as_tibble() %>% 
    dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
    sf::st_as_sf(crs = moll_pacific) %>% 
    replaceNN(., grid, "salinity_front") %>%
    replaceNN(., grid, "angle") %>% 
    dplyr::as_tibble() %>% 
    dplyr::select(cellID, salinity_front_transformed, angle_transformed, geometry)
  
}

# Function to prepare plots
create_plot <- function(ggsalinity) {
  dataSalinity <- ggsalinity %>% 
    sf::st_as_sf(sf_column_name = "geometry")
  
  gg <- ggplot() +
    geom_sf(data = dataSalinity, aes(fill = salinity_front_transformed), color = NA, size = 0.01) +
    scale_fill_gradientn(colors = rev(brewer.pal(9, "PuBuGn")),
                         na.value = "grey64",
                         guide = guide_colourbar(
                           title.vjust = 0.5,
                           barheight = grid::unit(0.01, "npc"),
                           barwidth = grid::unit(0.25, "npc"),
                           frame.colour = "black")) +
    geom_sf(data = landmass, fill = "black", color = "black") +
    labs(fill = expression('ppt km'^"-1")) +
    theme_bw()  +
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
rs <- raster::stack(here::here(input_dir, 
                               paste(input, "1956", "1981", season, "ensemble.nc", sep = "_")))
grad <- create_layer(rs)
saveRDS(grad, here::here(output_dir, 
                         paste(label, season, "interpolated.rds", sep = "_"))) # save object
# grad <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

sf1 <- create_plot(grad)

# ii. April-June
season <- "apr-jun"
rs <- raster::stack(here::here(input_dir, 
                               paste(input, "1956", "1981", season, "ensemble.nc", sep = "_")))
grad <- create_layer(rs)
saveRDS(grad, here::here(output_dir, 
                         paste(label, season, "interpolated.rds", sep = "_"))) # save object
# grad <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

sf2 <- create_plot(grad)

# iii. July-September
season <- "jul-sept"
rs <- raster::stack(here::here(input_dir, 
                               paste(input, "1956", "1981", season, "ensemble.nc", sep = "_")))
grad <- create_layer(rs)
saveRDS(grad, here::here(output_dir, 
                         paste(label, season, "interpolated.rds", sep = "_"))) # save object
# grad <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

sf3 <- create_plot(grad)

# iv. October-December
season <- "oct-dec"
rs <- raster::stack(here::here(input_dir, 
                               paste(input, "1956", "1981", season, "ensemble.nc", sep = "_")))
grad <- create_layer(rs)
saveRDS(grad, here::here(output_dir, 
                         paste(label, season, "interpolated.rds", sep = "_"))) # save object
# grad <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

sf4 <- create_plot(grad)

# Full thermal front plots
full_sf <- (sf1 + sf2) / (sf3 + sf4) +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = full_sf, filename = here::here(figure_dir, "global_historical_salinity_front_full.png"), width = 27, height = 15, dpi = 300)
