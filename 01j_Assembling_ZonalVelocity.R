# DESCRIPTION: Creating seasonal zonal velocity layers

# Load preliminaries
source("00_PreparePredictors.R")
label <- "uo_historical"

# Function to prepare uo layer
create_layer <- function(rs) {
  names(rs) <- paste0("X", seq(1956, 1981, by = 1))  
  
  uo <- rs2sf(rs) %>% 
    dplyr::rename(uo = mean) %>% # using the mean of the models
    sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
    dplyr::as_tibble() %>% 
    dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
    sf::st_as_sf(crs = cCRS) %>% 
    replaceNN(., grid, "uo") %>%
    dplyr::as_tibble() %>% 
    dplyr::select(cellID, uo_transformed, geometry)  
  
}

# Function to prepare plots
create_plot <- function(gguo) {
  dataUO <- gguo %>% 
    sf::st_as_sf(sf_column_name = "geometry")
  
  gg <- ggplot() +
    geom_sf(data = dataUO, aes(fill = uo_transformed), color = NA, size = 0.01) +
    scale_fill_gradientn(colors = rev(brewer.pal(9, "Blues")),
                         na.value = "grey64",
                         oob = scales::squish,
                         guide = guide_colourbar(
                           title.vjust = 0.5,
                           barheight = grid::unit(0.01, "npc"),
                           barwidth = grid::unit(0.25, "npc"),
                           frame.colour = "black")) +
    geom_sf(data = landmass, fill = "black", color = "black") +
    labs(fill = expression('m s'^"-1"*'')) +
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
uo_rs <- stars::read_ncdf(here::here(input_dir, 
                                        paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
uo <- create_layer(uo_rs)
saveRDS(uo, here::here(output_dir, 
                          paste(label, season, "interpolated.rds", sep = "_"))) # save object
# uo <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

uo1 <- create_plot(uo)

# ii. April-June
season <- "apr-jun"
uo_rs <- stars::read_ncdf(here::here(input_dir, 
                                        paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
uo <- create_layer(uo_rs)
saveRDS(uo, here::here(output_dir, 
                          paste(label, season, "interpolated.rds", sep = "_"))) # save object
# uo <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

uo2 <- create_plot(uo)

# iii. July-September
season <- "jul-sept"
uo_rs <- stars::read_ncdf(here::here(input_dir, 
                                        paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
uo <- create_layer(uo_rs)
saveRDS(uo, here::here(output_dir, 
                          paste(label, season, "interpolated.rds", sep = "_"))) # save object
# uo <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

uo3 <- create_plot(uo)

# iv. October-December
season <- "oct-dec"
uo_rs <- stars::read_ncdf(here::here(input_dir, 
                                        paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
uo <- create_layer(uo_rs)
saveRDS(uo, here::here(output_dir, 
                          paste(label, season, "interpolated.rds", sep = "_"))) # save object
# uo <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

uo4 <- create_plot(uo)

# Full zonal velocity plots
full_uo <- (uo1 + uo2) / (uo3 + uo4) +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = full_uo, filename = here::here(figure_dir, "global_historical_uo_full.png"), width = 27, height = 15, dpi = 300)
