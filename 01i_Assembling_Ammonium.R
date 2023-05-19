# DESCRIPTION: Creating seasonal ammonium layers

# Load preliminaries
source("00_PreparePredictors.R")
label <- "nh4os_historical"
figure_dir <- here::here(figure_dir, "predictors")

# Function to prepare nh4os layer
create_layer <- function(rs) {
  names(rs) <- paste0("X", seq(1956, 1981, by = 1))  
  
  nh4os <- rs2sf(rs) %>% 
    dplyr::rename(nh4os = mean) %>% # using the mean of the models
    sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
    dplyr::as_tibble() %>% 
    dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
    sf::st_as_sf(crs = cCRS) %>% 
    replaceNN(., grid, "nh4os") %>%
    dplyr::as_tibble() %>% 
    dplyr::select(cellID, nh4os_transformed, geometry)  
  
}

# Function to prepare plots
create_plot <- function(ggnh4os, season) {
  dataNH4OS <- ggnh4os %>% 
    sf::st_as_sf(sf_column_name = "geometry")
  
  gg <- ggplot() +
    geom_sf(data = dataNH4OS, aes(fill = nh4os_transformed), color = NA, size = 0.01) +
    scale_fill_gradientn(colors = brewer.pal(9, "Reds"),
                         na.value = "grey64",
                         limits = c(0.005, 1.5),
                         oob = scales::squish,
                         guide = guide_colourbar(
                           title.vjust = 0.5,
                           barheight = grid::unit(0.01, "npc"),
                           barwidth = grid::unit(0.25, "npc"),
                           frame.colour = "black")) +
    geom_sf(data = landmass, fill = "black", color = "black") +
    ggtitle(season) +
    labs(fill = expression('Ammonium concentration (mmol m'^"-3"*')')) +
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
nh4os_rs <- stars::read_ncdf(here::here(input_dir, 
                                        paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
nh4os <- create_layer(nh4os_rs)
saveRDS(nh4os, here::here(output_dir, 
                          paste(label, season, "interpolated.rds", sep = "_"))) # save object
# nh4os <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

amm1 <- create_plot(nh4os %>% 
                      dplyr::mutate(nh4os_transformed = nh4os_transformed*1000), "January-March")

# ii. April-June
season <- "apr-jun"
nh4os_rs <- stars::read_ncdf(here::here(input_dir, 
                                        paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
nh4os <- create_layer(nh4os_rs)
saveRDS(nh4os, here::here(output_dir, 
                          paste(label, season, "interpolated.rds", sep = "_"))) # save object
# nh4os <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

amm2 <- create_plot(nh4os %>% 
                      dplyr::mutate(nh4os_transformed = nh4os_transformed*1000), "April-June")

# iii. July-September
season <- "jul-sept"
nh4os_rs <- stars::read_ncdf(here::here(input_dir, 
                                        paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
nh4os <- create_layer(nh4os_rs)
saveRDS(nh4os, here::here(output_dir, 
                          paste(label, season, "interpolated.rds", sep = "_"))) # save object
# nh4os <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

amm3 <- create_plot(nh4os %>% 
                      dplyr::mutate(nh4os_transformed = nh4os_transformed*1000), "July-September")

# iv. October-December
season <- "oct-dec"
nh4os_rs <- stars::read_ncdf(here::here(input_dir, 
                                        paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
nh4os <- create_layer(nh4os_rs)
saveRDS(nh4os, here::here(output_dir, 
                          paste(label, season, "interpolated.rds", sep = "_"))) # save object
# nh4os <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

amm4 <- create_plot(nh4os %>% 
                      dplyr::mutate(nh4os_transformed = nh4os_transformed*1000), "October-December")

# Full ammonium plot
full_amm <- (amm1 + amm2) / (amm3 + amm4) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") &
  theme(plot.tag = element_text(size = 30),
        legend.position = "bottom")

ggsave(plot = full_amm, filename = here::here(figure_dir, "PredictorPlots_nh4os.png"), width = 27, height = 15, dpi = 300)
