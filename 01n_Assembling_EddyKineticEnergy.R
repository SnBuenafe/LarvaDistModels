# DESCRIPTION: Calculate eddy kinetic energy (in m^2/s^2)

# Load preliminaries
source("00_Preliminaries.R")
input_dir <- here::here("Data", "Climatology", "sf")
output_dir <- here::here("Data", "Climatology", "sf")
figure_dir <- here::here("Figures")

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

# i. January-March
uo <- readRDS(here::here(input_dir, "uo_historical_jan-mar_interpolated.rds"))
vo <- readRDS(here::here(input_dir, "vo_historical_jan-mar_interpolated.rds"))
comb <- create_layer(uo, vo)
saveRDS(comb, here::here(output_dir, "eke_historical_jan-mar_interpolated.rds"))

eke1 <- create_plot(comb)

# ii. April-June
uo <- readRDS(here::here(input_dir, "uo_historical_apr-jun_interpolated.rds"))
vo <- readRDS(here::here(input_dir, "vo_historical_apr-jun_interpolated.rds"))
comb <- create_layer(uo, vo)
saveRDS(comb, here::here(output_dir, "eke_historical_apr-jun_interpolated.rds"))

eke2 <- create_plot(comb)

# iii. July-September
uo <- readRDS(here::here(input_dir, "uo_historical_jul-sept_interpolated.rds"))
vo <- readRDS(here::here(input_dir, "vo_historical_jul-sept_interpolated.rds"))
comb <- create_layer(uo, vo)
saveRDS(comb, here::here(output_dir, "eke_historical_jul-sept_interpolated.rds"))

eke3 <- create_plot(comb)

# iv. October-December
uo <- readRDS(here::here(input_dir, "uo_historical_oct-dec_interpolated.rds"))
vo <- readRDS(here::here(input_dir, "vo_historical_oct-dec_interpolated.rds"))
comb <- create_layer(uo, vo)
saveRDS(comb, here::here(output_dir, "eke_historical_oct-dec_interpolated.rds"))

eke4 <- create_plot(comb)

# Full mesoscale features plots
full_eke <- (eke1 + eke2) / (eke3 + eke4) +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = full_eke, filename = here::here(figure_dir, "global_historical_eke_full.png"), width = 27, height = 15, dpi = 300)
