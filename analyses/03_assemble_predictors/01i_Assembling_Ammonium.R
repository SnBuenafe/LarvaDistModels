# DESCRIPTION: Creating seasonal ammonium layers

# Load preliminaries
library(here)
preliminaries_dir <- here("analyses", "02_preliminaries")
source(here(preliminaries_dir, "00_PreparePredictors.R"))

# Set labeling parameters
old_label <- paste("nh4os", "Omon", "ensemble", "omip2", "r1i1p1f1", "seasonal", "19630101-19811231", sep = "_")
new_label <- "nh4os_omip2"
figure_dir <- here(figure_dir, "supplementary")

# Function to prepare nh4os layer
create_layer <- function(rs) {
  names(rs) <- paste0("X", seq(1963, 1981, by = 1))  
  
  nh4os <- rs2sf(rs) %>% 
    rename(nh4os = mean) %>% # using the mean of the models
    st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
    as_tibble() %>% 
    left_join(grid, ., by = "geometry") %>% # left_join with the grid
    st_as_sf(crs = cCRS) %>% 
    replaceNN(., grid, "nh4os") %>%
    as_tibble() %>% 
    dplyr::select(cellID, nh4os_transformed, geometry) %>% 
    mutate(nh4os_transformed = nh4os_transformed*1000) # transform from mol m-3 to mmol m-3 
  
}

# Function to prepare plots
create_plot <- function(ggnh4os) {
  dataNH4OS <- ggnh4os %>% 
    st_as_sf(sf_column_name = "geometry")
  
  gg <- ggplot() +
    geom_sf(data = dataNH4OS, aes(fill = nh4os_transformed), color = NA, size = 0.01) +
    scale_fill_gradientn(colors = brewer.pal(9, "Reds"),
                         na.value = "grey64",
                         limits = c(0.005, 1.5),
                         oob = scales::squish,
                         guide = guide_colourbar(
                           title.vjust = 0.5,
                           barheight = grid::unit(0.035, "npc"),
                           barwidth = grid::unit(0.35, "npc"),
                           frame.colour = "black")) +
    geom_sf(data = landmass, fill = "black", color = "black") +
    labs(fill = expression('Ammonium concentration (mmol m'^"-3"*')')) +
    change_gglayout()
}

#### Create layers ####
# i. January-March
season <- "jan-mar"
nh4os_rs <- read_ncdf(here(input_dir, 
                           paste(old_label, paste0(season, ".nc"), sep = "_"))) %>% 
  rast()
nh4os <- create_layer(nh4os_rs)
saveRDS(nh4os, here(output_dir, 
                    paste(new_label, season, "interpolated.rds", sep = "_"))) # save object
# nh4os <- readRDS(here::here(output_dir, paste(new_label, season, "interpolated.rds", sep = "_")))

amm <- create_plot(nh4os)
ggsave(plot = amm, filename = here(figure_dir, paste0(new_label, "_", season, ".png")), width = 14, height = 5, dpi = 600)

# ii. April-June
season <- "apr-jun"
nh4os_rs <- read_ncdf(here(input_dir, 
                           paste(old_label, paste0(season, ".nc"), sep = "_"))) %>% 
  rast()
nh4os <- create_layer(nh4os_rs)
saveRDS(nh4os, here(output_dir, 
                    paste(new_label, season, "interpolated.rds", sep = "_"))) # save object
# nh4os <- readRDS(here::here(output_dir, paste(new_label, season, "interpolated.rds", sep = "_")))

amm <- create_plot(nh4os)
ggsave(plot = amm, filename = here(figure_dir, paste0(new_label, "_", season, ".png")), width = 14, height = 5, dpi = 600)

# iii. July-September
season <- "jul-sep"
nh4os_rs <- read_ncdf(here(input_dir, 
                            paste(old_label, paste0(season, ".nc"), sep = "_"))) %>% 
  rast()
nh4os <- create_layer(nh4os_rs)
saveRDS(nh4os, here(output_dir, 
                    paste(new_label, season, "interpolated.rds", sep = "_"))) # save object
# nh4os <- readRDS(here::here(output_dir, paste(new_label, season, "interpolated.rds", sep = "_")))

amm <- create_plot(nh4os)
ggsave(plot = amm, filename = here(figure_dir, paste0(new_label, "_", season, ".png")), width = 14, height = 5, dpi = 600)

# iv. October-December
season <- "oct-dec"
nh4os_rs <- read_ncdf(here(input_dir, 
                           paste(old_label, paste0(season, ".nc"), sep = "_"))) %>% 
  rast()
nh4os <- create_layer(nh4os_rs)
saveRDS(nh4os, here(output_dir, 
                    paste(new_label, season, "interpolated.rds", sep = "_"))) # save object
# nh4os <- readRDS(here::here(output_dir, paste(new_label, season, "interpolated.rds", sep = "_")))

amm <- create_plot(nh4os)
ggsave(plot = amm, filename = here(figure_dir, paste0(new_label, "_", season, ".png")), width = 14, height = 5, dpi = 600)
