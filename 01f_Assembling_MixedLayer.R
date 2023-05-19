# DESCRIPTION: Creating seasonal mixed layer thickness layers

# Load preliminaries
source("00_PreparePredictors.R")
label <- "mlotst_historical"
figure_dir <- here::here(figure_dir, "predictors")

# Function to prepare mlotst layer
create_layer <- function(rs) {
  names(rs) <- paste0("X", seq(1956, 1981, by = 1))  
  
  mlotst <- rs2sf(rs) %>% 
    dplyr::rename(mlotst = mean) %>% # using the mean of the models
    sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
    dplyr::as_tibble() %>% 
    dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
    sf::st_as_sf(crs = cCRS) %>% 
    replaceNN(., grid, "mlotst") %>%
    dplyr::as_tibble() %>% 
    dplyr::select(cellID, mlotst_transformed, geometry)  
  
}

# Function to prepare plots
create_plot <- function(ggmlotst, season) {
  dataMixed <- ggmlotst %>% 
    sf::st_as_sf(sf_column_name = "geometry")
  
  gg <- ggplot() +
    geom_sf(data = dataMixed, aes(fill = mlotst_transformed), color = NA, size = 0.01) +
    scale_fill_gradientn(colors = rev(brewer.pal(9, "GnBu")),
                         na.value = "grey64",
                         limits = c(15, 400),
                         oob = scales::squish,
                         guide = guide_colourbar(
                           title.vjust = 0.5,
                           barheight = grid::unit(0.01, "npc"),
                           barwidth = grid::unit(0.25, "npc"),
                           frame.colour = "black")) +
    geom_sf(data = landmass, fill = "black", color = "black") +
    ggtitle(season) +
    labs(fill = expression('Mixed layer thickness (m)')) +
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
mlotst_rs <- stars::read_ncdf(here::here(input_dir, 
                                      paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
mlotst <- create_layer(mlotst_rs)
saveRDS(mlotst, here::here(output_dir, 
                        paste(label, season, "interpolated.rds", sep = "_"))) # save object
# mlotst <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

mix1 <- create_plot(mlotst, "January-March")

# ii. April-June
season <- "apr-jun"
mlotst_rs <- stars::read_ncdf(here::here(input_dir, 
                                      paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
mlotst <- create_layer(mlotst_rs)
saveRDS(mlotst, here::here(output_dir, 
                        paste(label, season, "interpolated.rds", sep = "_"))) # save object
# mlotst <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

mix2 <- create_plot(mlotst, "April-June")

# iii. July-September
season <- "jul-sept"
mlotst_rs <- stars::read_ncdf(here::here(input_dir, 
                                      paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
mlotst <- create_layer(mlotst_rs)
saveRDS(mlotst, here::here(output_dir, 
                        paste(label, season, "interpolated.rds", sep = "_"))) # save object
# mlotst <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

mix3 <- create_plot(mlotst, "July-September")

# iv. October-December
season <- "oct-dec"
mlotst_rs <- stars::read_ncdf(here::here(input_dir, 
                                      paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
mlotst <- create_layer(mlotst_rs)
saveRDS(mlotst, here::here(output_dir, 
                        paste(label, season, "interpolated.rds", sep = "_"))) # save object
# mlotst <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

mix4 <- create_plot(mlotst, "October-December")

# Full mixed layer depth plot
full_mix <- (mix1 + mix2) / (mix3 + mix4) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") &
  theme(plot.tag = element_text(size = 30),
        legend.position = "bottom")

ggsave(plot = full_mix, filename = here::here(figure_dir, "PredictorPlots_mlotst.png"), width = 27, height = 15, dpi = 300)
