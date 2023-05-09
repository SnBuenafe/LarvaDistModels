# DESCRIPTION: Calculate eddy kinetic energy (in m^2/s^2)

# Load preliminaries
source("00_PreparePredictors.R")
input_dir <- here::here("Data", "Climatology", "sf")
input1 <- "uo_historical"
input2 <- "vo_historical"
label <- "eke_front_historical"

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
                         oob = scales::squish,
                         guide = guide_colourbar(
                           title.vjust = 0.5,
                           barheight = grid::unit(0.01, "npc"),
                           barwidth = grid::unit(0.25, "npc"),
                           frame.colour = "black")) +
    geom_sf(data = landmass, fill = "black", color = "black") +
    labs(fill = expression('m'^"2"*'s'^"-2"*'')) +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.title = element_blank(),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 18),
          panel.border = element_blank()) +
    coord_sf(xlim = st_bbox(grid)$xlim, ylim = st_bbox(grid)$ylim)
}

#### Create layers ####
# i. January-March
season <- "jan-mar"
uo <- readRDS(here::here(input_dir, paste(input1, season, "interpolated.rds", sep = "_")))
vo <- readRDS(here::here(input_dir, paste(input2, season, "interpolated.rds", sep = "_")))
comb <- create_layer(uo, vo)
saveRDS(comb, here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

eke1 <- create_plot(comb)

# ii. April-June
season <- "apr-jun"
uo <- readRDS(here::here(input_dir, paste(input1, season, "interpolated.rds", sep = "_")))
vo <- readRDS(here::here(input_dir, paste(input2, season, "interpolated.rds", sep = "_")))
comb <- create_layer(uo, vo)
saveRDS(comb, here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

eke2 <- create_plot(comb)

# iii. July-September
season <- "jul-sept"
uo <- readRDS(here::here(input_dir, paste(input1, season, "interpolated.rds", sep = "_")))
vo <- readRDS(here::here(input_dir, paste(input2, season, "interpolated.rds", sep = "_")))
comb <- create_layer(uo, vo)
saveRDS(comb, here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

eke3 <- create_plot(comb)

# iv. October-December
season <- "oct-dec"
uo <- readRDS(here::here(input_dir, paste(input1, season, "interpolated.rds", sep = "_")))
vo <- readRDS(here::here(input_dir, paste(input2, season, "interpolated.rds", sep = "_")))
comb <- create_layer(uo, vo)
saveRDS(comb, here::here(output_dir, paste(label, season, "interpolated.rds", sep = "_")))

eke4 <- create_plot(comb)

# Full mesoscale features plots
full_eke <- (eke1 + eke2) / (eke3 + eke4) +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = full_eke, filename = here::here(figure_dir, "global_historical_eke_full.png"), width = 27, height = 15, dpi = 300)
