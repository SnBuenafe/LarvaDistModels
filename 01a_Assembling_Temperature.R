# DESCRIPTION: Creating seasonal temperature layers

# Load preliminaries
source("00_PreparePredictors.R")
label <- "tos_historical"
figure_dir <- here::here(figure_dir, "predictors")

# Function to prepare tos layer
create_layer <- function(rs) {
  names(rs) <- paste0("X", seq(1956, 1981, by = 1))  
  
  tos <- rs2sf(rs) %>% 
    dplyr::rename(tos = mean) %>% # using the mean of the models
    sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
    dplyr::as_tibble() %>% 
    dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
    sf::st_as_sf(crs = cCRS) %>% 
    replaceNN(., grid, "tos") %>%
    dplyr::as_tibble() %>% 
    dplyr::select(cellID, tos_transformed, geometry)  
  
}

# Function to prepare plots
create_plot <- function(ggtos, season) {
  dataTmp <- ggtos %>% 
    sf::st_as_sf(sf_column_name = "geometry")
  
  gg <- ggplot() +
    geom_sf(data = dataTmp, aes(fill = tos_transformed), color = NA, size = 0.01) +
    scale_fill_cmocean(name = "deep",
                       #   alpha = 1,
                       aesthetics = c("fill"),
                       direction = -1,
                       na.value = "grey64",
                       limits = c(5,35),
                       guide = guide_colourbar(
                         title.vjust = 0.5,
                         barheight = grid::unit(0.035, "npc"),
                         barwidth = grid::unit(0.2, "npc"),
                         frame.colour = "black")) +
    geom_sf(data = landmass, fill = "black", color = "black") +
    ggtitle(season) +
    labs(fill = expression('Temperature ('^"o"*'C)')) +
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
tos_rs <- stars::read_ncdf(here::here(input_dir, 
                                      paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
tos <- create_layer(tos_rs)
saveRDS(tos, here::here(output_dir, 
                        paste(label, season, "interpolated.rds", sep = "_"))) # save object
# tos <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

tmp1 <- create_plot(tos, "January-March")
# ggsave(plot = tmp1, filename = here::here(figure_dir, paste0(label, "_", season, ".png")), width = 15, height = 7, dpi = 600)

# ii. April-June
season <- "apr-jun"
tos_rs <- stars::read_ncdf(here::here(input_dir, 
                                      paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
tos <- create_layer(tos_rs)
saveRDS(tos, here::here(output_dir, 
                        paste(label, season, "interpolated.rds", sep = "_"))) # save object
# tos <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

tmp2 <- create_plot(tos, "April-June")

# iii. July-September
season <- "jul-sept"
tos_rs <- stars::read_ncdf(here::here(input_dir, 
                                      paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
tos <- create_layer(tos_rs)
saveRDS(tos, here::here(output_dir, 
                        paste(label, season, "interpolated.rds", sep = "_"))) # save object
# tos <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

tmp3 <- create_plot(tos, "July-September")

# iv. October-December
season <- "oct-dec"
tos_rs <- stars::read_ncdf(here::here(input_dir, 
                                      paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
tos <- create_layer(tos_rs)
saveRDS(tos, here::here(output_dir, 
                        paste(label, season, "interpolated.rds", sep = "_"))) # save object
# tos <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

tmp4 <- create_plot(tos, "October-December")

# Full temperature plot
full_tmp <- (tmp1 + tmp2) / (tmp3 + tmp4) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") &
  theme(legend.position = "bottom",
        plot.tag = element_text(size = 30))

ggsave(plot = full_tmp, filename = here::here(figure_dir, "PredictorLayers_tos.png"), width = 27, height = 15, dpi = 300)
