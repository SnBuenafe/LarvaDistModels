# DESCRIPTION: Creating seasonal nitrate layers

# Load preliminaries
source("00_PreparePredictors.R")
label <- "no3os_historical"

# Function to prepare no3os layer
create_layer <- function(rs) {
  names(rs) <- paste0("X", seq(1956, 1981, by = 1))  
  
  no3os <- rs2sf(rs) %>% 
    dplyr::rename(no3os = mean) %>% # using the mean of the models
    sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
    dplyr::as_tibble() %>% 
    dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
    sf::st_as_sf(crs = cCRS) %>% 
    replaceNN(., grid, "no3os") %>%
    dplyr::as_tibble() %>% 
    dplyr::select(cellID, no3os_transformed, geometry)  
  
}

# Function to prepare plots
create_plot <- function(ggno3os) {
  dataNO3 <- ggno3os %>% 
    sf::st_as_sf(sf_column_name = "geometry")
  
  gg <- ggplot() +
    geom_sf(data = dataNO3, aes(fill = no3os_transformed), color = NA, size = 0.01) +
    scale_fill_gradientn(colors = brewer.pal(9, "YlOrBr"),
                         na.value = "grey64",
                         oob = scales::squish,
                         guide = guide_colourbar(
                           title.vjust = 0.5,
                           barheight = grid::unit(0.01, "npc"),
                           barwidth = grid::unit(0.25, "npc"),
                           frame.colour = "black")) +
    geom_sf(data = landmass, fill = "black", color = "black") +
    labs(fill = expression('mol m'^"-3"*'')) +
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
no3os_rs <- stars::read_ncdf(here::here(input_dir, 
                                         paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
no3os <- create_layer(no3os_rs)
saveRDS(no3os, here::here(output_dir, 
                           paste(label, season, "interpolated.rds", sep = "_"))) # save object
# no3os <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

nit1 <- create_plot(no3os)

# ii. April-June
season <- "apr-jun"
no3os_rs <- stars::read_ncdf(here::here(input_dir, 
                                         paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
no3os <- create_layer(no3os_rs)
saveRDS(no3os, here::here(output_dir, 
                           paste(label, season, "interpolated.rds", sep = "_"))) # save object
# no3os <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

nit2 <- create_plot(no3os)

# iii. July-September
season <- "jul-sept"
no3os_rs <- stars::read_ncdf(here::here(input_dir, 
                                         paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
no3os <- create_layer(no3os_rs)
saveRDS(no3os, here::here(output_dir, 
                           paste(label, season, "interpolated.rds", sep = "_"))) # save object
# no3os <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

nit3 <- create_plot(no3os)

# iv. October-December
season <- "oct-dec"
no3os_rs <- stars::read_ncdf(here::here(input_dir, 
                                         paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
no3os <- create_layer(no3os_rs)
saveRDS(no3os, here::here(output_dir, 
                           paste(label, season, "interpolated.rds", sep = "_"))) # save object
# no3os <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

nit4 <- create_plot(no3os)

# Full nitrate plot
full_nit <- (nit1 + nit2) / (nit3 + nit4) +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = full_nit, filename = here::here(figure_dir, "global_historical_nitrate_full.png"), width = 27, height = 15, dpi = 300)
