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
    dplyr::select(cellID, chlos_transformed, geometry)  
  
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
                           barheight = grid::unit(0.01, "npc"),
                           barwidth = grid::unit(0.25, "npc"),
                           frame.colour = "black")) +
    geom_sf(data = landmass, fill = "black", color = "black") +
    ggtitle(season) +
    labs(fill = expression('Chlorophyll concentration (mg m'^"-3"*')')) +
    theme_bw() +
    theme(plot.title = element_text(size = 28, color = "black"),
          legend.position = "bottom",
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
chlos_rs <- stars::read_ncdf(here::here(input_dir, 
                                       paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
chlos <- create_layer(chlos_rs)
saveRDS(chlos, here::here(output_dir, 
                         paste(label, season, "interpolated.rds", sep = "_"))) # save object
# chlos <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

chl1 <- create_plot(chlos %>% 
                      dplyr::mutate(chlos_transformed = chlos_transformed*1000000), "January-March")

# ii. April-June
season <- "apr-jun"
chlos_rs <- stars::read_ncdf(here::here(input_dir, 
                                       paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
chlos <- create_layer(chlos_rs)
saveRDS(chlos, here::here(output_dir, 
                         paste(label, season, "interpolated.rds", sep = "_"))) # save object
# chlos <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

chl2 <- create_plot(chlos %>% 
                      dplyr::mutate(chlos_transformed = chlos_transformed*1000000), "April-June")

# iii. July-September
season <- "jul-sept"
chlos_rs <- stars::read_ncdf(here::here(input_dir, 
                                       paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
chlos <- create_layer(chlos_rs)
saveRDS(chlos, here::here(output_dir, 
                         paste(label, season, "interpolated.rds", sep = "_"))) # save object
# chlos <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

chl3 <- create_plot(chlos %>% 
                      dplyr::mutate(chlos_transformed = chlos_transformed*1000000), "July-September")

# iv. October-December
season <- "oct-dec"
chlos_rs <- stars::read_ncdf(here::here(input_dir, 
                                       paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
chlos <- create_layer(chlos_rs)
saveRDS(chlos, here::here(output_dir, 
                         paste(label, season, "interpolated.rds", sep = "_"))) # save object
# chlos <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

chl4 <- create_plot(chlos %>% 
                      dplyr::mutate(chlos_transformed = chlos_transformed*1000000), "October-December")

# Full chlorophyll plot
full_chl <- (chl1 + chl2) / (chl3 + chl4) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") &
  theme(plot.tag = element_text(size = 30),
        legend.position = "bottom")

ggsave(plot = full_chl, filename = here::here(figure_dir, "PredictorPlots_chlos.png"), width = 27, height = 15, dpi = 300)

