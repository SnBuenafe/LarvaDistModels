# DESCRIPTION: Creating seasonal meridional velocity layers

# Load preliminaries
source("00_PreparePredictors.R")
label <- "vo_historical"
figure_dir <- here::here(figure_dir, "predictors")

# Function to prepare vo layer
create_layer <- function(rs) {
  names(rs) <- paste0("X", seq(1956, 1981, by = 1))  
  
  vo <- rs2sf(rs) %>% 
    dplyr::rename(vo = mean) %>% # using the mean of the models
    sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
    dplyr::as_tibble() %>% 
    dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
    sf::st_as_sf(crs = cCRS) %>% 
    replaceNN(., grid, "vo") %>%
    dplyr::as_tibble() %>% 
    dplyr::select(cellID, vo_transformed, geometry)  
  
}

# Function to prepare plots
create_plot <- function(ggvo) {
  datavo <- ggvo %>% 
    sf::st_as_sf(sf_column_name = "geometry")
  
  gg <- ggplot() +
    geom_sf(data = datavo, aes(fill = vo_transformed), color = NA, size = 0.01) +
    scale_fill_gradientn(colors = rev(brewer.pal(9, "Blues")),
                         na.value = "grey64",
                         limits = c(-0.5, 0.5),
                         oob = scales::squish,
                         guide = guide_colourbar(
                           title.vjust = 0.5,
                           barheight = grid::unit(0.035, "npc"),
                           barwidth = grid::unit(0.5, "npc"),
                           frame.colour = "black")) +
    geom_sf(data = landmass, fill = "black", color = "black") +
    labs(fill = expression('Meridional velocity (m s'^"-1"*')  ')) +
    change_gglayout()
}

#### Create seasonal layers ####
# i. January-March
season <- "jan-mar"
vo_rs <- stars::read_ncdf(here::here(input_dir, 
                                     paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
vo <- create_layer(vo_rs)
saveRDS(vo, here::here(output_dir, 
                       paste(label, season, "interpolated.rds", sep = "_"))) # save object
# vo <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

vo <- create_plot(vo)
ggsave(plot = vo, filename = here::here(figure_dir, paste0(label, "_", season, ".png")), width = 14, height = 5, dpi = 600)

# ii. April-June
season <- "apr-jun"
vo_rs <- stars::read_ncdf(here::here(input_dir, 
                                     paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
vo <- create_layer(vo_rs)
saveRDS(vo, here::here(output_dir, 
                       paste(label, season, "interpolated.rds", sep = "_"))) # save object
# vo <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

vo <- create_plot(vo)
ggsave(plot = vo, filename = here::here(figure_dir, paste0(label, "_", season, ".png")), width = 14, height = 5, dpi = 600)

# iii. July-September
season <- "jul-sept"
vo_rs <- stars::read_ncdf(here::here(input_dir, 
                                     paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
vo <- create_layer(vo_rs)
saveRDS(vo, here::here(output_dir, 
                       paste(label, season, "interpolated.rds", sep = "_"))) # save object
# vo <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

vo <- create_plot(vo)
ggsave(plot = vo, filename = here::here(figure_dir, paste0(label, "_", season, ".png")), width = 14, height = 5, dpi = 600)

# iv. October-December
season <- "oct-dec"
vo_rs <- stars::read_ncdf(here::here(input_dir, 
                                     paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
vo <- create_layer(vo_rs)
saveRDS(vo, here::here(output_dir, 
                       paste(label, season, "interpolated.rds", sep = "_"))) # save object
# vo <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

vo <- create_plot(vo)
ggsave(plot = vo, filename = here::here(figure_dir, paste0(label, "_", season, ".png")), width = 14, height = 5, dpi = 600)
