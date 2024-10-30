# DESCRIPTION: Creating seasonal phosphate layers

# Load preliminaries
source("00_PreparePredictors.R")
old_label <- paste("po4os", "Omon", "ensemble", "omip2", "r1i1p1f1", "seasonal", "19630101-19811231", sep = "_")
new_label <- "po4os_omip2"
figure_dir <- here::here(figure_dir, "predictors")

# Function to prepare po4os layer
create_layer <- function(rs) {
  names(rs) <- paste0("X", seq(1963, 1981, by = 1))  
  
  po4os <- rs2sf(rs) %>% 
    dplyr::rename(po4os = mean) %>% # using the mean of the models
    sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning uphoss
    dplyr::as_tibble() %>% 
    dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
    sf::st_as_sf(crs = cCRS) %>% 
    replaceNN(., grid, "po4os") %>%
    dplyr::as_tibble() %>% 
    dplyr::select(cellID, po4os_transformed, geometry) %>% 
    dplyr::mutate(po4os_transformed = po4os_transformed*1000) # transform from mol m-3 to mmol m-3  
  
}

# Function to prepare plots
create_plot <- function(ggpo4os) {
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
                           barheight = grid::unit(0.035, "npc"),
                           barwidth = grid::unit(0.4, "npc"),
                           frame.colour = "black")) +
    geom_sf(data = landmass, fill = "black", color = "black") +
    labs(fill = expression('Phosphate concentration (mmol m'^"-3"*')')) +
    change_gglayout()
}

#### Create seasonal layers ####
# i. January-March
season <- "jan-mar"
po4os_rs <- stars::read_ncdf(here::here(input_dir, 
                                        paste(old_label, paste0(season, ".nc"), sep = "_"))) %>% 
  terra::rast()
po4os <- create_layer(po4os_rs)
saveRDS(po4os, here::here(output_dir, 
                          paste(new_label, season, "interpolated.rds", sep = "_"))) # save object
# po4os <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

phos <- create_plot(po4os)
ggsave(plot = phos, filename = here::here(figure_dir, paste0(new_label, "_", season, ".png")), width = 14, height = 5, dpi = 600)

# ii. April-June
season <- "apr-jun"
po4os_rs <- stars::read_ncdf(here::here(input_dir, 
                                        paste(old_label, paste0(season, ".nc"), sep = "_"))) %>% 
  terra::rast()
po4os <- create_layer(po4os_rs)
saveRDS(po4os, here::here(output_dir, 
                          paste(new_label, season, "interpolated.rds", sep = "_"))) # save object
# po4os <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

phos <- create_plot(po4os)
ggsave(plot = phos, filename = here::here(figure_dir, paste0(new_label, "_", season, ".png")), width = 14, height = 5, dpi = 600)

# iii. July-September
season <- "jul-sep"
po4os_rs <- stars::read_ncdf(here::here(input_dir, 
                                        paste(old_label, paste0(season, ".nc"), sep = "_"))) %>% 
  terra::rast()
po4os <- create_layer(po4os_rs)
saveRDS(po4os, here::here(output_dir, 
                          paste(new_label, season, "interpolated.rds", sep = "_"))) # save object
# po4os <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

phos <- create_plot(po4os)
ggsave(plot = phos, filename = here::here(figure_dir, paste0(new_label, "_", season, ".png")), width = 14, height = 5, dpi = 600)

# iv. October-December
season <- "oct-dec"
po4os_rs <- stars::read_ncdf(here::here(input_dir, 
                                        paste(old_label, paste0(season, ".nc"), sep = "_"))) %>% 
  terra::rast()
po4os <- create_layer(po4os_rs)
saveRDS(po4os, here::here(output_dir, 
                          paste(new_label, season, "interpolated.rds", sep = "_"))) # save object
# po4os <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

phos <- create_plot(po4os)
ggsave(plot = phos, filename = here::here(figure_dir, paste0(new_label, "_", season, ".png")), width = 14, height = 5, dpi = 600)
