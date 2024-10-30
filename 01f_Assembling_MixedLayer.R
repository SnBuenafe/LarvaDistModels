# DESCRIPTION: Creating seasonal mixed layer thickness layers

# Load preliminaries
source("00_PreparePredictors.R")
old_label <- paste("mlotst", "Omon", "ensemble", "omip2", "r1i1p1f1", "seasonal", "19630101-19811231", sep = "_")
new_label <- "mlotst_omip2"
figure_dir <- here::here(figure_dir, "predictors")

# Function to prepare mlotst layer
create_layer <- function(rs) {
  names(rs) <- paste0("X", seq(1963, 1981, by = 1))  
  
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
create_plot <- function(ggmlotst) {
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
                           barheight = grid::unit(0.035, "npc"),
                           barwidth = grid::unit(0.5, "npc"),
                           frame.colour = "black")) +
    geom_sf(data = landmass, fill = "black", color = "black") +
    labs(fill = expression('Mixed layer thickness (m)')) +
    change_gglayout()
}

#### Create seasonal layers ####
# i. January-March
season <- "jan-mar"
mlotst_rs <- stars::read_ncdf(here::here(input_dir, 
                                         paste(old_label, paste0(season, ".nc"), sep = "_"))) %>% 
  terra::rast()
mlotst <- create_layer(mlotst_rs)
saveRDS(mlotst, here::here(output_dir, 
                        paste(new_label, season, "interpolated.rds", sep = "_"))) # save object
# mlotst <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

mix <- create_plot(mlotst)
ggsave(plot = mix, filename = here::here(figure_dir, paste0(new_label, "_", season, ".png")), width = 14, height = 5, dpi = 600)

# ii. April-June
season <- "apr-jun"
mlotst_rs <- stars::read_ncdf(here::here(input_dir, 
                                          paste(old_label, paste0(season, ".nc"), sep = "_"))) %>% 
  terra::rast()
mlotst <- create_layer(mlotst_rs)
saveRDS(mlotst, here::here(output_dir, 
                        paste(new_label, season, "interpolated.rds", sep = "_"))) # save object
# mlotst <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

mix <- create_plot(mlotst)
ggsave(plot = mix, filename = here::here(figure_dir, paste0(new_label, "_", season, ".png")), width = 14, height = 5, dpi = 600)

# iii. July-September
season <- "jul-sep"
mlotst_rs <- stars::read_ncdf(here::here(input_dir, 
                                         paste(old_label, paste0(season, ".nc"), sep = "_"))) %>% 
  terra::rast()
mlotst <- create_layer(mlotst_rs)
saveRDS(mlotst, here::here(output_dir, 
                        paste(new_label, season, "interpolated.rds", sep = "_"))) # save object
# mlotst <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

mix <- create_plot(mlotst)
ggsave(plot = mix, filename = here::here(figure_dir, paste0(new_label, "_", season, ".png")), width = 14, height = 5, dpi = 600)

# iv. October-December
season <- "oct-dec"
mlotst_rs <- stars::read_ncdf(here::here(input_dir, 
                                         paste(old_label, paste0(season, ".nc"), sep = "_"))) %>% 
  terra::rast()
mlotst <- create_layer(mlotst_rs)
saveRDS(mlotst, here::here(output_dir, 
                        paste(new_label, season, "interpolated.rds", sep = "_"))) # save object
# mlotst <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

mix <- create_plot(mlotst)
ggsave(plot = mix, filename = here::here(figure_dir, paste0(new_label, "_", season, ".png")), width = 14, height = 5, dpi = 600)
