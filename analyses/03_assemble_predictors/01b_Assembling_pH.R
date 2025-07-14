# DESCRIPTION: Creating seasonal pH layers

# Load preliminaries
library(here)
preliminaries_dir <- here("analyses", "02_preliminaries")
source(here(preliminaries_dir, "00_PreparePredictors.R"))

# Set labeling parameters
old_label <- paste("phos", "Omon", "ensemble", "omip2", "r1i1p1f1", "seasonal", "19630101-19811231", sep = "_")
new_label <- "phos_omip2"
figure_dir <- here(figure_dir, "supplementary")

# Function to prepare phos layer
create_layer <- function(rs) {
  names(rs) <- paste0("X", seq(1963, 1981, by = 1))  
  
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
create_plot <- function(ggphos) {
  dataPH <- ggphos %>% 
    sf::st_as_sf(sf_column_name = "geometry")
  
  gg <- ggplot() +
    geom_sf(data = dataPH, aes(fill = phos_transformed), color = NA, size = 0.01) +
    scale_fill_gradientn(colors = brewer.pal(9, "RdPu"),
                         na.value = "grey64",
                         limits = c(7.5, 8.4),
                         guide = guide_colourbar(
                           title.vjust = 0.5,
                           barheight = grid::unit(0.035, "npc"),
                           barwidth = grid::unit(0.6, "npc"),
                           frame.colour = "black")) +
    geom_sf(data = landmass, fill = "black", color = "black") +
    labs(fill = expression('pH    ')) +
    change_gglayout()
}

#### Create seasonal layers ####
# i. January-March
season <- "jan-mar"
phos_rs <- stars::read_ncdf(here::here(input_dir, 
                                       paste(old_label, paste0(season, ".nc"), sep = "_"))) %>% 
  terra::rast()
phos <- create_layer(phos_rs)
saveRDS(phos, here::here(output_dir, 
                        paste(new_label, season, "interpolated.rds", sep = "_"))) # save object
# phos <- readRDS(here::here(output_dir, paste(new_label, season, "interpolated.rds", sep = "_")))

ph <- create_plot(phos)
ggsave(plot = ph, filename = here::here(figure_dir, paste0(new_label, "_", season, ".png")), width = 14, height = 5, dpi = 600)

# ii. April-June
season <- "apr-jun"
phos_rs <- stars::read_ncdf(here::here(input_dir, 
                                       paste(old_label, paste0(season, ".nc"), sep = "_"))) %>% 
  terra::rast()
phos <- create_layer(phos_rs)
saveRDS(phos, here::here(output_dir, 
                        paste(new_label, season, "interpolated.rds", sep = "_"))) # save object
# phos <- readRDS(here::here(output_dir, paste(new_label, season, "interpolated.rds", sep = "_")))

ph <- create_plot(phos)
ggsave(plot = ph, filename = here::here(figure_dir, paste0(new_label, "_", season, ".png")), width = 14, height = 5, dpi = 600)

# iii. July-September
season <- "jul-sep"
phos_rs <- stars::read_ncdf(here::here(input_dir, 
                                       paste(old_label, paste0(season, ".nc"), sep = "_"))) %>% 
  terra::rast()
phos <- create_layer(phos_rs)
saveRDS(phos, here::here(output_dir, 
                        paste(new_label, season, "interpolated.rds", sep = "_"))) # save object
# phos <- readRDS(here::here(output_dir, paste(new_label, season, "interpolated.rds", sep = "_")))

ph <- create_plot(phos)
ggsave(plot = ph, filename = here::here(figure_dir, paste0(new_label, "_", season, ".png")), width = 14, height = 5, dpi = 600)

# iv. October-December
season <- "oct-dec"
phos_rs <- stars::read_ncdf(here::here(input_dir, 
                                       paste(old_label, paste0(season, ".nc"), sep = "_"))) %>% 
  terra::rast()
phos <- create_layer(phos_rs)
saveRDS(phos, here::here(output_dir, 
                        paste(new_label, season, "interpolated.rds", sep = "_"))) # save object
# phos <- readRDS(here::here(output_dir, paste(new_label, season, "interpolated.rds", sep = "_")))

ph <- create_plot(phos)
ggsave(plot = ph, filename = here::here(figure_dir, paste0(new_label, "_", season, ".png")), width = 14, height = 5, dpi = 600)
