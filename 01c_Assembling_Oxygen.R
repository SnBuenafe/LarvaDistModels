# DESCRIPTION: Creating seasonal oxygen layers

# Load preliminaries
source("00_PreparePredictors.R")
label <- "o2os_historical"
figure_dir <- here::here(figure_dir, "predictors")

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
create_plot <- function(ggo2os, season) {
  dataO2 <- ggo2os %>% 
    sf::st_as_sf(sf_column_name = "geometry")
  
  gg <- ggplot() +
    geom_sf(data = dataO2, aes(fill = o2os_transformed), color = NA, size = 0.01) +
    scale_fill_cmocean(name = "tempo",
                       #   alpha = 1,
                       aesthetics = c("fill"),
                       direction = -1,
                       limits = c(0.18, 0.36),
                       na.value = "grey64",
                       guide = guide_colourbar(
                         title.vjust = 0.5,
                         barheight = grid::unit(0.01, "npc"),
                         barwidth = grid::unit(0.25, "npc"),
                         frame.colour = "black")) +
    geom_sf(data = landmass, fill = "black", color = "black") +
    ggtitle(season) +
    labs(fill = expression('Oxygen concentration (mol m'^"-3"*')')) +
    theme_bw() +
    theme(plot.title = element_text(size = 28, color = "black"),
          axis.title = element_blank(),
          legend.text = element_text(size = 22, color = "black"),
          legend.title = element_text(size = 28, color = "black"),
          axis.text = element_text(size = 20, color = "black"),
          panel.border = element_rect(linewidth = 2, color = "black"),
          plot.margin = unit(c(0,0.5,0,0.5), "cm")) +
    coord_sf(xlim = st_bbox(grid)$xlim, ylim = st_bbox(grid)$ylim)
}

#### Create layers ####
# i. January-March
season <- "jan-mar"
o2os_rs <- stars::read_ncdf(here::here(input_dir, 
                                       paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
o2os <- create_layer(o2os_rs)
saveRDS(o2os, here::here(output_dir, 
                         paste(label, season, "interpolated.rds", sep = "_"))) # save object
# o2os <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

ox1 <- create_plot(o2os, "January-March")

# ii. April-June
season <- "apr-jun"
o2os_rs <- stars::read_ncdf(here::here(input_dir, 
                                       paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
o2os <- create_layer(o2os_rs)
saveRDS(o2os, here::here(output_dir, 
                         paste(label, season, "interpolated.rds", sep = "_"))) # save object
# o2os <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

ox2 <- create_plot(o2os, "April-June")

# iii. July-September
season <- "jul-sept"
o2os_rs <- stars::read_ncdf(here::here(input_dir, 
                                       paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
o2os <- create_layer(o2os_rs)
saveRDS(o2os, here::here(output_dir, 
                         paste(label, season, "interpolated.rds", sep = "_"))) # save object
# o2os <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

ox3 <- create_plot(o2os, "July-September")

# iv. October-December
season <- "oct-dec"
o2os_rs <- stars::read_ncdf(here::here(input_dir, 
                                       paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
o2os <- create_layer(o2os_rs)
saveRDS(o2os, here::here(output_dir, 
                         paste(label, season, "interpolated.rds", sep = "_"))) # save object
# o2os <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

ox4 <- create_plot(o2os, "October-December")

# Full oxygen plot
full_ox <- (ox1 + ox2) / (ox3 + ox4) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") &
  theme(legend.position = "bottom",
        plot.tag = element_text(size = 30))

ggsave(plot = full_ox, filename = here::here(figure_dir, "PredictorLayers_o2os.png"), width = 27, height = 15, dpi = 300)
