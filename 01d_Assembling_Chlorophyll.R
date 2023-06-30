# DESCRIPTION: Creating seasonal chlorophyll layers

# Load preliminaries
source("00_PreparePredictors.R")
label <- "chlos_historical"
figure_dir <- here::here(figure_dir, "predictors")

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
    dplyr::select(cellID, chlos_transformed, geometry) %>% 
    dplyr::mutate(chlos_transformed = chlos_transformed*1000000) # We transform chlorophyll from kg m-3 to mg m-3 so we multiply by 10^6
  
}

# Function to prepare plots
create_plot <- function(ggchlos, season) {
  dataChlorophyll <- ggchlos %>% 
    sf::st_as_sf(sf_column_name = "geometry")
  
  gg <- ggplot() +
    geom_sf(data = dataChlorophyll, aes(fill = chlos_transformed), color = NA, size = 0.01) +
    scale_fill_gradientn(colors = brewer.pal(9, "YlGn"),
                         na.value = "grey64",
                         limits = c(0.02, 0.45),
                         oob = scales::squish,
                         guide = guide_colourbar(
                           title.vjust = 0.5,
                           barheight = grid::unit(0.035, "npc"),
                           barwidth = grid::unit(0.3, "npc"),
                           frame.colour = "black")) +
    geom_sf(data = landmass, fill = "black", color = "black") +
    labs(fill = expression('Chlorophyll concentration (mg m'^"-3"*')')) +
    change_gglayout()
}

#### Create seasonal layers ####
# i. January-March
season <- "jan-mar"
chlos_rs <- stars::read_ncdf(here::here(input_dir, 
                                       paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
chlos <- create_layer(chlos_rs)
saveRDS(chlos, here::here(output_dir, 
                         paste(label, season, "interpolated.rds", sep = "_"))) # save object
# chlos <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

chl <- create_plot(chlos)
ggsave(plot = chl, filename = here::here(figure_dir, paste0(label, "_", season, ".png")), width = 14, height = 5, dpi = 600)

# ii. April-June
season <- "apr-jun"
chlos_rs <- stars::read_ncdf(here::here(input_dir, 
                                       paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
chlos <- create_layer(chlos_rs)
saveRDS(chlos, here::here(output_dir, 
                         paste(label, season, "interpolated.rds", sep = "_"))) # save object
# chlos <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

chl <- create_plot(chlos)
ggsave(plot = chl, filename = here::here(figure_dir, paste0(label, "_", season, ".png")), width = 14, height = 5, dpi = 600)

# iii. July-September
season <- "jul-sept"
chlos_rs <- stars::read_ncdf(here::here(input_dir, 
                                       paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
chlos <- create_layer(chlos_rs)
saveRDS(chlos, here::here(output_dir, 
                         paste(label, season, "interpolated.rds", sep = "_"))) # save object
# chlos <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

chl <- create_plot(chlos)
ggsave(plot = chl, filename = here::here(figure_dir, paste0(label, "_", season, ".png")), width = 14, height = 5, dpi = 600)

# iv. October-December
season <- "oct-dec"
chlos_rs <- stars::read_ncdf(here::here(input_dir, 
                                       paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
chlos <- create_layer(chlos_rs)
saveRDS(chlos, here::here(output_dir, 
                         paste(label, season, "interpolated.rds", sep = "_"))) # save object
# chlos <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

chl <- create_plot(chlos)
ggsave(plot = chl, filename = here::here(figure_dir, paste0(label, "_", season, ".png")), width = 14, height = 5, dpi = 600)
