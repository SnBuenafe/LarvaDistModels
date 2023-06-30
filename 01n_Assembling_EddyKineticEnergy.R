# DESCRIPTION: Calculate eddy kinetic energy (in m^2/s^2)

# Load preliminaries
source("00_PreparePredictors.R")
input_dir <- here::here("Data", "Climatology", "sf")
input1 <- "uo_historical"
input2 <- "vo_historical"
label <- "eke_historical"
figure_dir <- here::here(figure_dir, "predictors")

# Function to calculate EKE
create_layer <- function(uo, vo) {
  comb <- dplyr::left_join(uo, vo) %>% 
    dplyr::select(cellID, uo_transformed, vo_transformed, geometry) %>%  # arrange columns
    dplyr::mutate(eke = 0.5*(uo_transformed^2 + vo_transformed^2)) %>% # calculate EKE
    dplyr::select(cellID, eke, geometry)
}

# Function to prepare plots
create_plot <- function(ggcomb) {
  dataMeso <- ggcomb %>% 
    sf::st_as_sf(sf_column_name = "geometry")
  
  gg <- ggplot() +
    geom_sf(data = dataMeso, aes(fill = eke), color = NA, size = 0.01) +
    scale_fill_gradientn(colors = rev(brewer.pal(9, "Blues")),
                         na.value = "grey64",
                         limits = c(0.001, 0.2),
                         oob = scales::squish,
                         guide = guide_colourbar(
                           title.vjust = 0.5,
                           barheight = grid::unit(0.035, "npc"),
                           barwidth = grid::unit(0.5, "npc"),
                           frame.colour = "black")) +
    geom_sf(data = landmass, fill = "black", color = "black") +
    labs(fill = expression('Eddy kinetic energy (m'^"2"*'s'^"-2"*')')) +
    change_gglayout()
}

#### Create seasonal layers ####
# i. January-March
season <- "jan-mar"
uo <- readRDS(here::here(input_dir, paste(input1, season, "interpolated.rds", sep = "_")))
vo <- readRDS(here::here(input_dir, paste(input2, season, "interpolated.rds", sep = "_")))
comb <- create_layer(uo, vo)
saveRDS(comb, here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))
# comb <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

eke <- create_plot(comb)
ggsave(plot = eke, filename = here::here(figure_dir, paste0(label, "_", season, ".png")), width = 14, height = 5, dpi = 600)

# ii. April-June
season <- "apr-jun"
uo <- readRDS(here::here(input_dir, paste(input1, season, "interpolated.rds", sep = "_")))
vo <- readRDS(here::here(input_dir, paste(input2, season, "interpolated.rds", sep = "_")))
comb <- create_layer(uo, vo)
saveRDS(comb, here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))
# comb <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

eke <- create_plot(comb)
ggsave(plot = eke, filename = here::here(figure_dir, paste0(label, "_", season, ".png")), width = 14, height = 5, dpi = 600)

# iii. July-September
season <- "jul-sept"
uo <- readRDS(here::here(input_dir, paste(input1, season, "interpolated.rds", sep = "_")))
vo <- readRDS(here::here(input_dir, paste(input2, season, "interpolated.rds", sep = "_")))
comb <- create_layer(uo, vo)
saveRDS(comb, here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))
# comb <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

eke <- create_plot(comb)
ggsave(plot = eke, filename = here::here(figure_dir, paste0(label, "_", season, ".png")), width = 14, height = 5, dpi = 600)

# iv. October-December
season <- "oct-dec"
uo <- readRDS(here::here(input_dir, paste(input1, season, "interpolated.rds", sep = "_")))
vo <- readRDS(here::here(input_dir, paste(input2, season, "interpolated.rds", sep = "_")))
comb <- create_layer(uo, vo)
saveRDS(comb, here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))
# comb <- readRDS(here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

eke <- create_plot(comb)
ggsave(plot = eke, filename = here::here(figure_dir, paste0(label, "_", season, ".png")), width = 14, height = 5, dpi = 600)

