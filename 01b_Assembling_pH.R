# DESCRIPTION: Creating seasonal pH layers

# Load preliminaries
source("00_Preliminaries.R")
pacman::p_load(ggridges, patchwork, purrr)
figure_dir <- here::here(figure_dir, "predictors")
spp_list <- c("skp", "yft", "alb", "bet", "fri", "sbft", "bft", "lit", "slt", "bon", "blum", "shos", "swo", "strm", "sail", "lesc", "sau")

fin_tmp <- assemblePreds("phos")

# Function to prepare phos layer
create_layer <- function(rs) {
  names(rs) <- paste0("X", seq(1956, 1981, by = 1))  
  
  phos <- rs2sf(rs) %>% 
    dplyr::rename(phos = mean) %>% # using the mean of the models
    sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
    dplyr::as_tibble() %>% 
    dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
    sf::st_as_sf(crs = cCRS) %>% 
    replaceNN(., grid, "phos") %>%
    dplyr::as_tibble() %>% 
    dplyr::select(cellID, phos_transformed, geometry)  
  
}

# Function to prepare plots
create_plot <- function(ggphos, season) {
  dataPH <- ggphos %>% 
    sf::st_as_sf(sf_column_name = "geometry")
  
  gg <- ggplot() +
    geom_sf(data = dataPH, aes(fill = phos_transformed), color = NA, size = 0.01) +
    scale_fill_gradientn(colors = brewer.pal(9, "RdPu"),
                         na.value = "grey64",
                         limits = c(7.9, 8.4),
                         guide = guide_colourbar(
                           title.vjust = 0.5,
                           barheight = grid::unit(0.01, "npc"),
                           barwidth = grid::unit(0.25, "npc"),
                           frame.colour = "black")) +
    geom_sf(data = landmass, fill = "black", color = "black") +
    ggtitle(season) +
    labs(fill = expression('pH ')) +
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
phos_rs <- stars::read_ncdf(here::here(input_dir, 
                                      paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
phos <- create_layer(phos_rs)
saveRDS(phos, here::here(output_dir, 
                        paste(label, season, "interpolated.rds", sep = "_"))) # save object
# phos <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

ph1 <- create_plot(phos, "January-March")

# ii. April-June
season <- "apr-jun"
phos_rs <- stars::read_ncdf(here::here(input_dir, 
                                      paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
phos <- create_layer(phos_rs)
saveRDS(phos, here::here(output_dir, 
                        paste(label, season, "interpolated.rds", sep = "_"))) # save object
# phos <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

ph2 <- create_plot(phos, "April-June")

# iii. July-September
season <- "jul-sept"
phos_rs <- stars::read_ncdf(here::here(input_dir, 
                                      paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
phos <- create_layer(phos_rs)
saveRDS(phos, here::here(output_dir, 
                        paste(label, season, "interpolated.rds", sep = "_"))) # save object
# phos <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

ph3 <- create_plot(phos, "July-September")

# iv. October-December
season <- "oct-dec"
phos_rs <- stars::read_ncdf(here::here(input_dir, 
                                      paste(label, "1956", "1981", season, "ensemble.nc", sep = "_"))) %>% 
  terra::rast()
phos <- create_layer(phos_rs)
saveRDS(phos, here::here(output_dir, 
                        paste(label, season, "interpolated.rds", sep = "_"))) # save object
# phos <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

ph4 <- create_plot(phos, "October-December")

# Full pH plot
full_ph <- (ph1 + ph2) / (ph3 + ph4) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") &
  theme(legend.position = "bottom",
        plot.tag = element_text(size = 30))

ggsave(plot = full_ph, filename = here::here(figure_dir, "PredictorLayers_phos.png"), width = 27, height = 15, dpi = 300)
