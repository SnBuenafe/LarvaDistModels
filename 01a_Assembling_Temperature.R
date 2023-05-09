# DESCRIPTION: Creating seasonal temperature layers

# Load preliminaries
source("00_PreparePredictors.R")
label <- "tos_historical"

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
create_plot <- function(ggtos) {
  dataTmp <- ggtos %>% 
    sf::st_as_sf(sf_column_name = "geometry")
  
  gg <- ggplot() +
    geom_sf(data = dataTmp, aes(fill = tos_transformed), color = NA, size = 0.01) +
    scale_fill_cmocean(name = "deep",
                       #   alpha = 1,
                       aesthetics = c("fill"),
                       direction = -1,
                       na.value = "grey64",
                       guide = guide_colourbar(
                         title.vjust = 0.5,
                         barheight = grid::unit(0.035, "npc"),
                         barwidth = grid::unit(0.2, "npc"),
                         frame.colour = "black")) +
    geom_sf(data = landmass, fill = "black", color = "black") +
    labs(fill = expression(''^"o"*'C     ')) +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.title = element_blank(),
          legend.text = element_text(size = 18, color = "black"),
          legend.title = element_text(size = 25, color = "black"),
          axis.text = element_text(size = 12, color = "black"),
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

tmp1 <- create_plot(tos)
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

tmp2 <- create_plot(tos)

# iii. July-September
season <- "jul-sept"
tos_rs <- stars::read_ncdf(here::here(input_dir, 
                                      paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
tos <- create_layer(tos_rs)
saveRDS(tos, here::here(output_dir, 
                        paste(label, season, "interpolated.rds", sep = "_"))) # save object
# tos <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

tmp3 <- create_plot(tos)

# iv. October-December
season <- "oct-dec"
tos_rs <- stars::read_ncdf(here::here(input_dir, 
                                      paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
tos <- create_layer(tos_rs)
saveRDS(tos, here::here(output_dir, 
                        paste(label, season, "interpolated.rds", sep = "_"))) # save object
# tos <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

tmp4 <- create_plot(tos)

# Full temperature plot
full_tmp <- (tmp1 + tmp2) / (tmp3 + tmp4) +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = full_tmp, filename = here::here(figure_dir, "global_historical_temperature_full.png"), width = 27, height = 15, dpi = 300)
