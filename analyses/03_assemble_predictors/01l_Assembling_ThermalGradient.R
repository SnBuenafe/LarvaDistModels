# DESCRIPTION: Calculate broad-scale thermal gradients

# Load preliminaries
library(here)
preliminaries_dir <- here("analyses", "02_preliminaries")
source(here(preliminaries_dir, "00_PreparePredictors.R"))

# Set labeling parameters
old_label <- paste("tos", "Omon", "ensemble", "omip2", "r1i1p1f1", "seasonal", "19630101-19811231", sep = "_")
new_label <- "thermal_front_omip2"
figure_dir <- here(figure_dir, "supplementary")

# Function to prepare thermal gradient layer
create_layer <- function(rs) {
  names(rs) <- paste0("X", seq(1963, 1981, by = 1))  
  
  # Calculate thermal spatial gradient
  grad_rs <- spatGrad(rs, th = 0.0001, projected = FALSE)
  
  grad <- spat2sf(grad_rs) %>% 
    rename(thermal_front = Grad,
                  angle = Ang) %>% 
    st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
    as_tibble() %>% 
    left_join(grid, ., by = "geometry") %>% # left_join with the grid
    st_as_sf(crs = moll_pacific) %>% 
    replaceNN(., grid, "thermal_front") %>%
    replaceNN(., grid, "angle") %>% 
    as_tibble() %>% 
    dplyr::select(cellID, thermal_front_transformed, angle_transformed, geometry)
}

# Function to prepare plots
create_plot <- function(ggthermal) {
  dataThermal <- ggthermal %>% 
    st_as_sf(sf_column_name = "geometry")
  
  gg <- ggplot() +
    geom_sf(data = dataThermal, aes(fill = thermal_front_transformed), color = NA, size = 0.01) +
    scale_fill_cmocean(name = "deep",
                       #   alpha = 1,
                       aesthetics = c("fill"),
                       direction = -1,
                       na.value = "grey64",
                       limits = c(0.001, 0.02),
                       oob = scales::squish,
                       guide = guide_colorbar(
                         title.vjust = 0.5,
                         barheight = grid::unit(0.035, "npc"),
                         barwidth = grid::unit(0.5, "npc"),
                         frame.colour = "black")) +
    geom_sf(data = landmass, fill = "black", color = "black") +
    labs(fill = expression('Thermal gradient (Δ'^"o"*'C km'^"-1"*")")) +
    change_gglayout()
}

#### Create seasonal layers ####
# i. January-March
season <- "jan-mar"
rs <- read_ncdf(here(input_dir, 
                                  paste(old_label, paste0(season, ".nc"), sep = "_"))) %>% 
  rast()
grad <- create_layer(rs)
saveRDS(grad, here(output_dir, 
                          paste(new_label, season, "interpolated.rds", sep = "_"))) # save object
# grad <- readRDS(here::here(output_dir, paste(new_label, season, "interpolated.rds", sep = "_")))

tf <- create_plot(grad)
ggsave(plot = tf, filename = here(figure_dir, paste0(new_label, "_", season, ".png")), width = 15, height = 5, dpi = 600)

# ii. April-June
season <- "apr-jun"
rs <- read_ncdf(here(input_dir, 
                           paste(old_label, paste0(season, ".nc"), sep = "_"))) %>% 
  rast()
grad <- create_layer(rs)
saveRDS(grad, here(output_dir, 
                         paste(new_label, season, "interpolated.rds", sep = "_"))) # save object
# grad <- readRDS(here::here(output_dir, paste(new_label, season, "interpolated.rds", sep = "_")))

tf <- create_plot(grad)
ggsave(plot = tf, filename = here(figure_dir, paste0(new_label, "_", season, ".png")), width = 15, height = 5, dpi = 600)

# iii. July-September
season <- "jul-sep"
rs <- read_ncdf(here(input_dir, 
                     paste(old_label, paste0(season, ".nc"), sep = "_"))) %>% 
  rast()
grad <- create_layer(rs)
saveRDS(grad, here(output_dir, 
                   paste(new_label, season, "interpolated.rds", sep = "_"))) # save object
# grad <- readRDS(here::here(output_dir, paste(new_label, season, "interpolated.rds", sep = "_")))

tf <- create_plot(grad)
ggsave(plot = tf, filename = here(figure_dir, paste0(new_label, "_", season, ".png")), width = 15, height = 5, dpi = 600)

# iv. October-December
season <- "oct-dec"
rs <- read_ncdf(here(input_dir, 
                     paste(old_label, paste0(season, ".nc"), sep = "_"))) %>% 
  rast()
grad <- create_layer(rs)
saveRDS(grad, here(output_dir, 
                   paste(new_label, season, "interpolated.rds", sep = "_"))) # save object
# grad <- readRDS(here::here(output_dir, paste(new_label, season, "interpolated.rds", sep = "_")))

tf <- create_plot(grad)
ggsave(plot = tf, filename = here(figure_dir, paste0(new_label, "_", season, ".png")), width = 15, height = 5, dpi = 600)
