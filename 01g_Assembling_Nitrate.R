# DESCRIPTION: Creating seasonal nitrate layers

# Load preliminaries
source("00_PreparePredictors.R")
label <- "no3os_historical"
figure_dir <- here::here(figure_dir, "predictors")

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
create_plot <- function(ggno3os, season) {
  dataNO3 <- ggno3os %>% 
    sf::st_as_sf(sf_column_name = "geometry")
  
  gg <- ggplot() +
    geom_sf(data = dataNO3, aes(fill = no3os_transformed), color = NA, size = 0.01) +
    scale_fill_gradientn(colors = brewer.pal(9, "YlOrBr"),
                         na.value = "grey64",
                         limits = c(0.5, 35),
                         oob = scales::squish,
                         guide = guide_colourbar(
                           title.vjust = 0.5,
                           barheight = grid::unit(0.01, "npc"),
                           barwidth = grid::unit(0.25, "npc"),
                           frame.colour = "black")) +
    geom_sf(data = landmass, fill = "black", color = "black") +
    ggtitle(season) +
    labs(fill = expression('Nitrate concentration (mmol m'^"-3"*')')) +
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
no3os_rs <- stars::read_ncdf(here::here(input_dir, 
                                         paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
no3os <- create_layer(no3os_rs)
saveRDS(no3os, here::here(output_dir, 
                           paste(label, season, "interpolated.rds", sep = "_"))) # save object
# no3os <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

nit1 <- create_plot(no3os %>% 
                      dplyr::mutate(no3os_transformed = no3os_transformed*1000), "January-March")

# ii. April-June
season <- "apr-jun"
no3os_rs <- stars::read_ncdf(here::here(input_dir, 
                                         paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
no3os <- create_layer(no3os_rs)
saveRDS(no3os, here::here(output_dir, 
                           paste(label, season, "interpolated.rds", sep = "_"))) # save object
# no3os <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

nit2 <- create_plot(no3os %>% 
                      dplyr::mutate(no3os_transformed = no3os_transformed*1000), "April-June")

# iii. July-September
season <- "jul-sept"
no3os_rs <- stars::read_ncdf(here::here(input_dir, 
                                         paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
no3os <- create_layer(no3os_rs)
saveRDS(no3os, here::here(output_dir, 
                           paste(label, season, "interpolated.rds", sep = "_"))) # save object
# no3os <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

nit3 <- create_plot(no3os %>% 
                      dplyr::mutate(no3os_transformed = no3os_transformed*1000), "July-September")

# iv. October-December
season <- "oct-dec"
no3os_rs <- stars::read_ncdf(here::here(input_dir, 
                                         paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
no3os <- create_layer(no3os_rs)
saveRDS(no3os, here::here(output_dir, 
                           paste(label, season, "interpolated.rds", sep = "_"))) # save object
# no3os <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

nit4 <- create_plot(no3os %>% 
                      dplyr::mutate(no3os_transformed = no3os_transformed*1000), "October-December")

# Full nitrate plot
full_nit <- (nit1 + nit2) / (nit3 + nit4) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") &
  theme(plot.tag = element_text(size = 30),
        legend.position = "bottom")

ggsave(plot = full_nit, filename = here::here(figure_dir, "PredictorPlots_no3os.png"), width = 27, height = 15, dpi = 300)
