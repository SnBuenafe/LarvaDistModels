# DESCRIPTION: Creating seasonal salinity layers

# Load preliminaries
source("00_PreparePredictors.R")
old_label <- paste("sos", "Omon", "ensemble", "omip2", "r1i1p1f1", "seasonal", "19560101-19811231", sep = "_")
new_label <- "sos_omip2"
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
create_plot <- function(ggsos) {
  dataSalinity <- ggsos %>% 
    sf::st_as_sf(sf_column_name = "geometry")
  
  gg <- ggplot() +
    geom_sf(data = dataSalinity, aes(fill = sos_transformed), color = NA, size = 0.01) +
    scale_fill_gradientn(colors = rev(brewer.pal(9, "PuBuGn")),
                         na.value = "grey64",
                         limits = c(0, 50),
                         guide = guide_colourbar(
                           title.vjust = 0.5,
                           barheight = grid::unit(0.035, "npc"),
                           barwidth = grid::unit(0.5, "npc"),
                           frame.colour = "black")) +
    geom_sf(data = landmass, fill = "black", color = "black") +
    labs(fill = expression('Salinity (ppt)')) +
    change_gglayout()
}

#### Create seasonal layers ####
# i. January-March
season <- "jan-mar"
sos_rs <- stars::read_ncdf(here::here(input_dir, 
                                      paste(old_label, paste0(season, ".nc"), sep = "_"))) %>% 
  terra::rast()
sos <- create_layer(sos_rs)
saveRDS(sos, here::here(output_dir, 
                          paste(new_label, season, "interpolated.rds", sep = "_"))) # save object
# sos <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

sal <- create_plot(sos)
ggsave(plot = sal, filename = here::here(figure_dir, paste0(new_label, "_", season, ".png")), width = 14, height = 5, dpi = 600)

# ii. April-June
season <- "apr-jun"
sos_rs <- stars::read_ncdf(here::here(input_dir, 
                                      paste(old_label, paste0(season, ".nc"), sep = "_"))) %>% 
  terra::rast()
sos <- create_layer(sos_rs)
saveRDS(sos, here::here(output_dir, 
                          paste(new_label, season, "interpolated.rds", sep = "_"))) # save object
# sos <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

sal <- create_plot(sos)
ggsave(plot = sal, filename = here::here(figure_dir, paste0(new_label, "_", season, ".png")), width = 14, height = 5, dpi = 600)

# iii. July-September
season <- "jul-sep"
sos_rs <- stars::read_ncdf(here::here(input_dir, 
                                      paste(old_label, paste0(season, ".nc"), sep = "_"))) %>% 
  terra::rast()
sos <- create_layer(sos_rs)
saveRDS(sos, here::here(output_dir, 
                          paste(new_label, season, "interpolated.rds", sep = "_"))) # save object
# sos <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

sal <- create_plot(sos)
ggsave(plot = sal, filename = here::here(figure_dir, paste0(new_label, "_", season, ".png")), width = 14, height = 5, dpi = 600)

# iv. October-December
season <- "oct-dec"
sos_rs <- stars::read_ncdf(here::here(input_dir, 
                                      paste(old_label, paste0(season, ".nc"), sep = "_"))) %>% 
  terra::rast()
sos <- create_layer(sos_rs)
saveRDS(sos, here::here(output_dir, 
                          paste(new_label, season, "interpolated.rds", sep = "_"))) # save object
# sos <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

sal <- create_plot(sos)
ggsave(plot = sal, filename = here::here(figure_dir, paste0(new_label, "_", season, ".png")), width = 14, height = 5, dpi = 600)
