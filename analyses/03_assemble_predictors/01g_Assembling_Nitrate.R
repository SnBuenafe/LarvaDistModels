# DESCRIPTION: Creating seasonal nitrate layers

# Load preliminaries
library(here)
preliminaries_dir <- here("analyses", "02_preliminaries")
source(here(preliminaries_dir, "00_PreparePredictors.R"))

# Set labeling parameters
old_label <- paste("no3os", "Omon", "ensemble", "omip2", "r1i1p1f1", "seasonal", "19630101-19811231", sep = "_")
new_label <- "no3os_omip2"
figure_dir <- here::here(figure_dir, "predictors")

# Function to prepare no3os layer
create_layer <- function(rs) {
  names(rs) <- paste0("X", seq(1963, 1981, by = 1))  
  
  no3os <- rs2sf(rs) %>% 
    dplyr::rename(no3os = mean) %>% # using the mean of the models
    sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
    dplyr::as_tibble() %>% 
    dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
    sf::st_as_sf(crs = cCRS) %>% 
    replaceNN(., grid, "no3os") %>%
    dplyr::as_tibble() %>% 
    dplyr::select(cellID, no3os_transformed, geometry) %>% 
    dplyr::mutate(no3os_transformed = no3os_transformed*1000) # transform from mol m-3 to mmol m-3
  
}

# Function to prepare plots
create_plot <- function(ggno3os) {
  dataNO3 <- ggno3os %>% 
    sf::st_as_sf(sf_column_name = "geometry")
  
  gg <- ggplot() +
    geom_sf(data = dataNO3, aes(fill = no3os_transformed), color = NA, size = 0.01) +
    scale_fill_gradientn(colors = brewer.pal(9, "YlOrBr"),
                         na.value = "grey64",
                         limits = c(0.5, 35),
                         oob = scales::squish,
                         guide = guide_colourbar(
                           title.vjust = 0.5,
                           barheight = grid::unit(0.035, "npc"),
                           barwidth = grid::unit(0.4, "npc"),
                           frame.colour = "black")) +
    geom_sf(data = landmass, fill = "black", color = "black") +
    labs(fill = expression('Nitrate concentration (mmol m'^"-3"*')')) +
    change_gglayout()
}

#### Create seasonal layers ####
# i. January-March
season <- "jan-mar"
no3os_rs <- stars::read_ncdf(here::here(input_dir, 
                                        paste(old_label, paste0(season, ".nc"), sep = "_"))) %>% 
  terra::rast()
no3os <- create_layer(no3os_rs)
saveRDS(no3os, here::here(output_dir, 
                           paste(new_label, season, "interpolated.rds", sep = "_"))) # save object
# no3os <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

nit <- create_plot(no3os)
ggsave(plot = nit, filename = here::here(figure_dir, paste0(new_label, "_", season, ".png")), width = 14, height = 5, dpi = 600)

# ii. April-June
season <- "apr-jun"
no3os_rs <- stars::read_ncdf(here::here(input_dir, 
                                        paste(old_label, paste0(season, ".nc"), sep = "_"))) %>% 
  terra::rast()
no3os <- create_layer(no3os_rs)
saveRDS(no3os, here::here(output_dir, 
                           paste(new_label, season, "interpolated.rds", sep = "_"))) # save object
# no3os <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

nit <- create_plot(no3os)
ggsave(plot = nit, filename = here::here(figure_dir, paste0(new_label, "_", season, ".png")), width = 14, height = 5, dpi = 600)

# iii. July-September
season <- "jul-sep"
no3os_rs <- stars::read_ncdf(here::here(input_dir, 
                                        paste(old_label, paste0(season, ".nc"), sep = "_"))) %>% 
  terra::rast()
no3os <- create_layer(no3os_rs)
saveRDS(no3os, here::here(output_dir, 
                           paste(new_label, season, "interpolated.rds", sep = "_"))) # save object
# no3os <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

nit <- create_plot(no3os)
ggsave(plot = nit, filename = here::here(figure_dir, paste0(new_label, "_", season, ".png")), width = 14, height = 5, dpi = 600)

# iv. October-December
season <- "oct-dec"
no3os_rs <- stars::read_ncdf(here::here(input_dir, 
                                        paste(old_label, paste0(season, ".nc"), sep = "_"))) %>% 
  terra::rast()
no3os <- create_layer(no3os_rs)
saveRDS(no3os, here::here(output_dir, 
                           paste(new_label, season, "interpolated.rds", sep = "_"))) # save object
# no3os <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

nit <- create_plot(no3os)
ggsave(plot = nit, filename = here::here(figure_dir, paste0(new_label, "_", season, ".png")), width = 14, height = 5, dpi = 600)