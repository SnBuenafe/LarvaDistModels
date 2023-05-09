# DESCRIPTION: Creating seasonal phosphate layers

# Load preliminaries
source("00_PreparePredictors.R")
label <- "po4os_historical"

# Function to prepare po4os layer
create_layer <- function(rs) {
  names(rs) <- paste0("X", seq(1956, 1981, by = 1))  
  
  po4os <- rs2sf(rs) %>% 
    dplyr::rename(po4os = mean) %>% # using the mean of the models
    sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning uphoss
    dplyr::as_tibble() %>% 
    dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
    sf::st_as_sf(crs = cCRS) %>% 
    replaceNN(., grid, "po4os") %>%
    dplyr::as_tibble() %>% 
    dplyr::select(cellID, po4os_transformed, geometry)  
  
}

# Function to prepare plots
create_plot <- function(ggpo4os) {
  dataPO4OS <- ggpo4os %>% 
    sf::st_as_sf(sf_column_name = "geometry")
  
  gg <- ggplot() +
    geom_sf(data = dataPO4OS, aes(fill = po4os_transformed), color = NA, size = 0.01) +
    scale_fill_gradientn(colors = brewer.pal(9, "BuPu"),
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
po4os_rs <- stars::read_ncdf(here::here(input_dir, 
                                        paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
po4os <- create_layer(po4os_rs)
saveRDS(po4os, here::here(output_dir, 
                          paste(label, season, "interpolated.rds", sep = "_"))) # save object
# po4os <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

phos1 <- create_plot(po4os)

# ii. April-June
season <- "apr-jun"
po4os_rs <- stars::read_ncdf(here::here(input_dir, 
                                        paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
po4os <- create_layer(po4os_rs)
saveRDS(po4os, here::here(output_dir, 
                          paste(label, season, "interpolated.rds", sep = "_"))) # save object
# po4os <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

phos2 <- create_plot(po4os)

# iii. July-September
season <- "jul-sept"
po4os_rs <- stars::read_ncdf(here::here(input_dir, 
                                        paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
po4os <- create_layer(po4os_rs)
saveRDS(po4os, here::here(output_dir, 
                          paste(label, season, "interpolated.rds", sep = "_"))) # save object
# po4os <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

phos3 <- create_plot(po4os)

# iv. October-December
season <- "oct-dec"
po4os_rs <- stars::read_ncdf(here::here(input_dir, 
                                        paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
po4os <- create_layer(po4os_rs)
saveRDS(po4os, here::here(output_dir, 
                          paste(label, season, "interpolated.rds", sep = "_"))) # save object
# po4os <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

phos4 <- create_plot(po4os)

# Full phosphate plot
full_phos <- (phos1 + phos2) / (phos3 + phos4) +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = full_phos, filename = here::here(figure_dir, "global_historical_phosphate_full.png"), width = 27, height = 15, dpi = 300)
