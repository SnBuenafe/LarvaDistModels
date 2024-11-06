# DESCRIPTION: Creating seasonal zonal velocity layers

# Load preliminaries
library(here)
preliminaries_dir <- here("analyses", "02_preliminaries")
source(here(preliminaries_dir, "00_PreparePredictors.R"))

# Set labeling parameters
old_label <- paste("uo", "Omon", "ensemble", "omip2", "r1i1p1f1", "seasonal", "19630101-19811231", sep = "_")
new_label <- "uo_omip2"
figure_dir <- here(figure_dir, "predictors")

# Function to prepare uo layer
create_layer <- function(rs) {
  names(rs) <- paste0("X", seq(1963, 1981, by = 1))  
  
  uo <- rs2sf(rs) %>% 
    dplyr::rename(uo = mean) %>% # using the mean of the models
    sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
    dplyr::as_tibble() %>% 
    dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
    sf::st_as_sf(crs = cCRS) %>% 
    replaceNN(., grid, "uo") %>%
    dplyr::as_tibble() %>% 
    dplyr::select(cellID, uo_transformed, geometry)  
  
}

# Function to prepare plots
create_plot <- function(gguo) {
  dataUO <- gguo %>% 
    sf::st_as_sf(sf_column_name = "geometry")
  
  gg <- ggplot() +
    geom_sf(data = dataUO, aes(fill = uo_transformed), color = NA, size = 0.01) +
    scale_fill_gradientn(colors = rev(brewer.pal(9, "Blues")),
                         na.value = "grey64",
                         limits = c(-0.5, 0.5),
                         oob = scales::squish,
                         guide = guide_colourbar(
                           title.vjust = 0.5,
                           barheight = grid::unit(0.035, "npc"),
                           barwidth = grid::unit(0.5, "npc"),
                           frame.colour = "black")) +
    geom_sf(data = landmass, fill = "black", color = "black") +
    labs(fill = expression('Zonal velocity (m s'^"-1"*')  ')) +
    change_gglayout()
}

#### Create seasonal layers ####
# i. January-March
season <- "jan-mar"
uo_rs <- stars::read_ncdf(here::here(input_dir, 
                                     paste(old_label, paste0(season, ".nc"), sep = "_"))) %>% 
  terra::rast()
uo <- create_layer(uo_rs)
saveRDS(uo, here::here(output_dir, 
                          paste(new_label, season, "interpolated.rds", sep = "_"))) # save object
# uo <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

uo <- create_plot(uo)
ggsave(plot = uo, filename = here::here(figure_dir, paste0(new_label, "_", season, ".png")), width = 14, height = 5, dpi = 600)

# ii. April-June
season <- "apr-jun"
uo_rs <- stars::read_ncdf(here::here(input_dir, 
                                     paste(old_label, paste0(season, ".nc"), sep = "_"))) %>% 
  terra::rast()
uo <- create_layer(uo_rs)
saveRDS(uo, here::here(output_dir, 
                          paste(new_label, season, "interpolated.rds", sep = "_"))) # save object
# uo <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

uo <- create_plot(uo)
ggsave(plot = uo, filename = here::here(figure_dir, paste0(new_label, "_", season, ".png")), width = 14, height = 5, dpi = 600)

# iii. July-September
season <- "jul-sep"
uo_rs <- stars::read_ncdf(here::here(input_dir, 
                                     paste(old_label, paste0(season, ".nc"), sep = "_"))) %>% 
  terra::rast()
uo <- create_layer(uo_rs)
saveRDS(uo, here::here(output_dir, 
                          paste(new_label, season, "interpolated.rds", sep = "_"))) # save object
# uo <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

uo <- create_plot(uo)
ggsave(plot = uo, filename = here::here(figure_dir, paste0(new_label, "_", season, ".png")), width = 14, height = 5, dpi = 600)

# iv. October-December
season <- "oct-dec"
uo_rs <- stars::read_ncdf(here::here(input_dir, 
                                     paste(old_label, paste0(season, ".nc"), sep = "_"))) %>% 
  terra::rast()
uo <- create_layer(uo_rs)
saveRDS(uo, here::here(output_dir, 
                          paste(new_label, season, "interpolated.rds", sep = "_"))) # save object
# uo <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

uo <- create_plot(uo)
ggsave(plot = uo, filename = here::here(figure_dir, paste0(new_label, "_", season, ".png")), width = 14, height = 5, dpi = 600)
