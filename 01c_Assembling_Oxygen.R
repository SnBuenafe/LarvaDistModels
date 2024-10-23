# DESCRIPTION: Creating seasonal oxygen layers

# Load preliminaries
source("00_PreparePredictors.R")
old_label <- paste("o2os", "Omon", "ensemble", "omip2", "r1i1p1f1", "seasonal", "19560101-19811231", sep = "_")
new_label <- "o2os_omip2"
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
create_plot <- function(ggo2os) {
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
                         barheight = grid::unit(0.035, "npc"),
                         barwidth = grid::unit(0.5, "npc"),
                         frame.colour = "black")) +
    geom_sf(data = landmass, fill = "black", color = "black") +
    labs(fill = expression('Oxygen concentration (mol m'^"-3"*')')) +
    change_gglayout()
}

#### Create seasonal layers ####
# i. January-March
season <- "jan-mar"
o2os_rs <- stars::read_ncdf(here::here(input_dir, 
                                       paste(old_label, paste0(season, ".nc"), sep = "_"))) %>% 
  terra::rast()
o2os <- create_layer(o2os_rs)
saveRDS(o2os, here::here(output_dir, 
                         paste(new_label, season, "interpolated.rds", sep = "_"))) # save object
# o2os <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

ox <- create_plot(o2os)
ggsave(plot = ox, filename = here::here(figure_dir, paste0(new_label, "_", season, ".png")), width = 14, height = 5, dpi = 600)

# ii. April-June
season <- "apr-jun"
o2os_rs <- stars::read_ncdf(here::here(input_dir, 
                                       paste(old_label, paste0(season, ".nc"), sep = "_"))) %>% 
  terra::rast()
o2os <- create_layer(o2os_rs)
saveRDS(o2os, here::here(output_dir, 
                         paste(new_label, season, "interpolated.rds", sep = "_"))) # save object
# o2os <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

ox <- create_plot(o2os)
ggsave(plot = ox, filename = here::here(figure_dir, paste0(new_label, "_", season, ".png")), width = 14, height = 5, dpi = 600)

# iii. July-September
season <- "jul-sep"
o2os_rs <- stars::read_ncdf(here::here(input_dir, 
                                       paste(old_label, paste0(season, ".nc"), sep = "_"))) %>% 
  terra::rast()
o2os <- create_layer(o2os_rs)
saveRDS(o2os, here::here(output_dir, 
                         paste(new_label, season, "interpolated.rds", sep = "_"))) # save object
# o2os <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

ox <- create_plot(o2os)
ggsave(plot = ox, filename = here::here(figure_dir, paste0(new_label, "_", season, ".png")), width = 14, height = 5, dpi = 600)

# iv. October-December
season <- "oct-dec"
o2os_rs <- stars::read_ncdf(here::here(input_dir, 
                                       paste(old_label, paste0(season, ".nc"), sep = "_"))) %>% 
  terra::rast()
o2os <- create_layer(o2os_rs)
saveRDS(o2os, here::here(output_dir, 
                         paste(new_label, season, "interpolated.rds", sep = "_"))) # save object
# o2os <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

ox <- create_plot(o2os)
ggsave(plot = ox, filename = here::here(figure_dir, paste0(new_label, "_", season, ".png")), width = 14, height = 5, dpi = 600)

