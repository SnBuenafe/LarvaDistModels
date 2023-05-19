# DESCRIPTION: Creating seasonal phosphate layers

# Load preliminaries
source("00_PreparePredictors.R")
label <- "po4os_historical"
figure_dir <- here::here(figure_dir, "predictors")

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
create_plot <- function(ggpo4os, season) {
  dataPO4OS <- ggpo4os %>% 
    sf::st_as_sf(sf_column_name = "geometry")
  
  gg <- ggplot() +
    geom_sf(data = dataPO4OS, aes(fill = po4os_transformed), color = NA, size = 0.01) +
    scale_fill_gradientn(colors = brewer.pal(9, "BuPu"),
                         na.value = "grey64",
                         limits = c(0.015, 2.3),
                         oob = scales::squish,
                         guide = guide_colourbar(
                           title.vjust = 0.5,
                           barheight = grid::unit(0.01, "npc"),
                           barwidth = grid::unit(0.25, "npc"),
                           frame.colour = "black")) +
    geom_sf(data = landmass, fill = "black", color = "black") +
    ggtitle(season) +
    labs(fill = expression('Phosphate concentration (mmol m'^"-3"*')')) +
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
po4os_rs <- stars::read_ncdf(here::here(input_dir, 
                                        paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
po4os <- create_layer(po4os_rs)
saveRDS(po4os, here::here(output_dir, 
                          paste(label, season, "interpolated.rds", sep = "_"))) # save object
# po4os <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

phos1 <- create_plot(po4os %>% 
                       dplyr::mutate(po4os_transformed = po4os_transformed*1000), "January-March")

# ii. April-June
season <- "apr-jun"
po4os_rs <- stars::read_ncdf(here::here(input_dir, 
                                        paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
po4os <- create_layer(po4os_rs)
saveRDS(po4os, here::here(output_dir, 
                          paste(label, season, "interpolated.rds", sep = "_"))) # save object
# po4os <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

phos2 <- create_plot(po4os %>% 
                       dplyr::mutate(po4os_transformed = po4os_transformed*1000), "April-June")

# iii. July-September
season <- "jul-sept"
po4os_rs <- stars::read_ncdf(here::here(input_dir, 
                                        paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
po4os <- create_layer(po4os_rs)
saveRDS(po4os, here::here(output_dir, 
                          paste(label, season, "interpolated.rds", sep = "_"))) # save object
# po4os <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

phos3 <- create_plot(po4os %>% 
                       dplyr::mutate(po4os_transformed = po4os_transformed*1000), "July-September")

# iv. October-December
season <- "oct-dec"
po4os_rs <- stars::read_ncdf(here::here(input_dir, 
                                        paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
po4os <- create_layer(po4os_rs)
saveRDS(po4os, here::here(output_dir, 
                          paste(label, season, "interpolated.rds", sep = "_"))) # save object
# po4os <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

phos4 <- create_plot(po4os %>% 
                       dplyr::mutate(po4os_transformed = po4os_transformed*1000), "October-December")

# Full phosphate plot
full_phos <- (phos1 + phos2) / (phos3 + phos4) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") &
  theme(plot.tag = element_text(size = 30),
        legend.position = "bottom")

ggsave(plot = full_phos, filename = here::here(figure_dir, "PredictorPlots_po4os.png"), width = 27, height = 15, dpi = 300)
