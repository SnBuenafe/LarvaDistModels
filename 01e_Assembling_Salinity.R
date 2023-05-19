# DESCRIPTION: Creating seasonal salinity layers

# Load preliminaries
source("00_PreparePredictors.R")
label <- "sos_historical"
figure_dir <- here::here(figure_dir, "predictors")

# Function to prepare sos layer
create_layer <- function(rs) {
  names(rs) <- paste0("X", seq(1956, 1981, by = 1))  
  
  sos <- rs2sf(rs) %>% 
    dplyr::rename(sos = mean) %>% # using the mean of the models
    sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
    dplyr::as_tibble() %>% 
    dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
    sf::st_as_sf(crs = cCRS) %>% 
    replaceNN(., grid, "sos") %>%
    dplyr::as_tibble() %>% 
    dplyr::select(cellID, sos_transformed, geometry)  
  
}

# Function to prepare plots
create_plot <- function(ggsos, season) {
  dataSalinity <- ggsos %>% 
    sf::st_as_sf(sf_column_name = "geometry")
  
  gg <- ggplot() +
    geom_sf(data = dataSalinity, aes(fill = sos_transformed), color = NA, size = 0.01) +
    scale_fill_gradientn(colors = rev(brewer.pal(9, "PuBuGn")),
                         na.value = "grey64",
                         limits = c(29.5, 36.5),
                         guide = guide_colourbar(
                           title.vjust = 0.5,
                           barheight = grid::unit(0.01, "npc"),
                           barwidth = grid::unit(0.25, "npc"),
                           frame.colour = "black")) +
    geom_sf(data = landmass, fill = "black", color = "black") +
    ggtitle(season) +
    labs(fill = expression('Salinity (ppt)')) +
    theme_bw() +
    theme(plot.title = element_text(size = 28, color = "black"),
          legend.position = "bottom",
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
sos_rs <- stars::read_ncdf(here::here(input_dir, 
                                        paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
sos <- create_layer(sos_rs)
saveRDS(sos, here::here(output_dir, 
                          paste(label, season, "interpolated.rds", sep = "_"))) # save object
# sos <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

sal1 <- create_plot(sos, "January-March")

# ii. April-June
season <- "apr-jun"
sos_rs <- stars::read_ncdf(here::here(input_dir, 
                                        paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
sos <- create_layer(sos_rs)
saveRDS(sos, here::here(output_dir, 
                          paste(label, season, "interpolated.rds", sep = "_"))) # save object
# sos <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

sal2 <- create_plot(sos, "April-June")

# iii. July-September
season <- "jul-sept"
sos_rs <- stars::read_ncdf(here::here(input_dir, 
                                        paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
sos <- create_layer(sos_rs)
saveRDS(sos, here::here(output_dir, 
                          paste(label, season, "interpolated.rds", sep = "_"))) # save object
# sos <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

sal3 <- create_plot(sos, "July-September")

# iv. October-December
season <- "oct-dec"
sos_rs <- stars::read_ncdf(here::here(input_dir, 
                                        paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
sos <- create_layer(sos_rs)
saveRDS(sos, here::here(output_dir, 
                          paste(label, season, "interpolated.rds", sep = "_"))) # save object
# sos <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

sal4 <- create_plot(sos, "October-December")

# Full salinity plot
full_sal <- (sal1 + sal2) / (sal3 + sal4) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") &
  theme(plot.tag = element_text(size = 30),
        legend.position = "bottom")

ggsave(plot = full_sal, filename = here::here(figure_dir, "PredictorPlots_sos.png"), width = 27, height = 15, dpi = 300)
