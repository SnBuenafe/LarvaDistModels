# DESCRIPTION: Creating seasonal temperature layers

# Load preliminaries
library(here)
preliminaries_dir <- here("analyses", "02_preliminaries")
source(here(preliminaries_dir, "00_PreparePredictors.R"))

# Set labeling parameters
old_label <- paste("tos", "Omon", "ensemble", "omip2", "r1i1p1f1", "seasonal", "19630101-19811231", sep = "_")
new_label <- "tos_omip2"
figure_dir <- here(figure_dir, "predictors")

# Function to prepare tos layer
create_layer <- function(rs) {
  names(rs) <- paste0("X", seq(1963, 1981, by = 1))  
  
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
                       limits = c(0,35),
                       guide = guide_colourbar(
                         title.vjust = 0.5,
                         barheight = grid::unit(0.035, "npc"),
                         barwidth = grid::unit(0.5, "npc"),
                         frame.colour = "black")) +
    geom_sf(data = landmass, fill = "black", color = "black") +
    labs(fill = expression('Temperature ('^"o"*'C)')) +
    change_gglayout()
}

#### Create seasonal layers ####

# i. January-March
season <- "jan-mar"
tos_rs <- read_ncdf(here(input_dir, 
                         paste(old_label, paste0(season, ".nc"), sep = "_"))) %>% 
  rast()
tos <- create_layer(tos_rs)
saveRDS(tos, here(output_dir, 
                        paste(new_label, season, "interpolated.rds", sep = "_"))) # save object
# tos <- readRDS(here::here(output_dir, paste(new_label, season, "interpolated.rds", sep = "_")))

tmp <- create_plot(tos)
ggsave(plot = tmp, filename = here(figure_dir, paste0(new_label, "_", season, ".png")), width = 14, height = 5, dpi = 600)

# ii. April-June
season <- "apr-jun"
tos_rs <- read_ncdf(here(input_dir, 
                         paste(old_label, paste0(season, ".nc"), sep = "_"))) %>% 
  rast()
tos <- create_layer(tos_rs)
saveRDS(tos, here(output_dir, 
                        paste(new_label, season, "interpolated.rds", sep = "_"))) # save object
# tos <- readRDS(here::here(output_dir, paste(new_label, season, "interpolated.rds", sep = "_")))

tmp <- create_plot(tos)
ggsave(plot = tmp, filename = here(figure_dir, paste0(new_label, "_", season, ".png")), width = 14, height = 5, dpi = 600)

# iii. July-September
season <- "jul-sep"
tos_rs <- read_ncdf(here(input_dir, 
                         paste(old_label, paste0(season, ".nc"), sep = "_"))) %>% 
  rast()
tos <- create_layer(tos_rs)
saveRDS(tos, here(output_dir, 
                  paste(new_label, season, "interpolated.rds", sep = "_"))) # save object
# tos <- readRDS(here::here(output_dir, paste(new_label, season, "interpolated.rds", sep = "_")))

tmp <- create_plot(tos)
ggsave(plot = tmp, filename = here(figure_dir, paste0(new_label, "_", season, ".png")), width = 14, height = 5, dpi = 600)

# iv. October-December
season <- "oct-dec"
tos_rs <- read_ncdf(here(input_dir, 
                                paste(old_label, paste0(season, ".nc"), sep = "_"))) %>% 
  rast()
tos <- create_layer(tos_rs)
saveRDS(tos, here(output_dir, 
                  paste(new_label, season, "interpolated.rds", sep = "_"))) # save object
# tos <- readRDS(here::here(output_dir, paste(new_label, season, "interpolated.rds", sep = "_")))

tmp <- create_plot(tos)
ggsave(plot = tmp, filename = here(figure_dir, paste0(new_label, "_", season, ".png")), width = 14, height = 5, dpi = 600)
