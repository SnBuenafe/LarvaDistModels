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
create_plot <- function(ggvo, season) {
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
                           barheight = grid::unit(0.01, "npc"),
                           barwidth = grid::unit(0.25, "npc"),
                           frame.colour = "black")) +
    geom_sf(data = landmass, fill = "black", color = "black") +
    ggtitle(season) +
    labs(fill = expression('Meridional velocity (m s'^"-1"*') ')) +
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
vo_rs <- stars::read_ncdf(here::here(input_dir, 
                                     paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
vo <- create_layer(vo_rs)
saveRDS(vo, here::here(output_dir, 
                       paste(label, season, "interpolated.rds", sep = "_"))) # save object
# vo <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

vo1 <- create_plot(vo, "January-March")

# ii. April-June
season <- "apr-jun"
vo_rs <- stars::read_ncdf(here::here(input_dir, 
                                     paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
vo <- create_layer(vo_rs)
saveRDS(vo, here::here(output_dir, 
                       paste(label, season, "interpolated.rds", sep = "_"))) # save object
# vo <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

vo2 <- create_plot(vo, "April-June")

# iii. July-September
season <- "jul-sept"
vo_rs <- stars::read_ncdf(here::here(input_dir, 
                                     paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
vo <- create_layer(vo_rs)
saveRDS(vo, here::here(output_dir, 
                       paste(label, season, "interpolated.rds", sep = "_"))) # save object
# vo <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

vo3 <- create_plot(vo, "July-September")

# iv. October-December
season <- "oct-dec"
vo_rs <- stars::read_ncdf(here::here(input_dir, 
                                     paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
vo <- create_layer(vo_rs)
saveRDS(vo, here::here(output_dir, 
                       paste(label, season, "interpolated.rds", sep = "_"))) # save object
# vo <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

vo4 <- create_plot(vo, "October-December")

# Full zonal velocity plots
full_vo <- (vo1 + vo2) / (vo3 + vo4) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") &
  theme(plot.tag = element_text(size = 30),
        legend.position = "bottom")

ggsave(plot = full_vo, filename = here::here(figure_dir, "PredictorPlots_vo.png"), width = 27, height = 15, dpi = 300)
