# DESCRIPTION: Summary plots for surface temperature
# Scatter plots with Loess smoother

# Define preliminaries
source("00_Preliminaries.R")
pacman::p_load(ggridges, patchwork, purrr, sf)
figure_dir <- here::here(figure_dir, "predictors")

spp_list <- spec_dict %>% 
  dplyr::filter(!code %in% c("BON", "LIT")) %>% # remove bonitos and little tuna
  dplyr::select(code) %>% 
  pull() %>% 
  tolower()

#### Temperature (tos) ####
# Assemble data frame
df <- assemblePredictor(spp_list, "tos")

# Plot scatter plots
for(i in 1:length(spp_list)) {
  
  gg <- plot_scatter_predictor(df, "tos_transformed", spp_list[i])
  ggsave(filename = here::here(figure_dir, paste("tos", paste0(spp_list[i], ".png"), sep = "_")), 
         plot = gg, 
         width = 5,
         height = 3,
         dpi = 600)
  
  print(spp_list[i]) # Sanity check
}

#### pH (phos) ####
# Assemble data frame
df <- assemblePredictor(spp_list, "phos")

# Plot scatter plots
for(i in 1:length(spp_list)) {
  
  gg <- plot_scatter_predictor(df, "phos_transformed", spp_list[i])
  ggsave(filename = here::here(figure_dir, paste("phos", paste0(spp_list[i], ".png"), sep = "_")), 
         plot = gg, 
         width = 5,
         height = 3,
         dpi = 600)
  
  print(spp_list[i]) # Sanity check
}

#### Oxygen (o2os) ####
# Assemble data frame
df <- assemblePredictor(spp_list, "o2os")

# Plot scatter plots
for(i in 1:length(spp_list)) {
  
  gg <- plot_scatter_predictor(df, "o2os_transformed", spp_list[i])
  ggsave(filename = here::here(figure_dir, paste("o2os", paste0(spp_list[i], ".png"), sep = "_")), 
         plot = gg, 
         width = 5,
         height = 3,
         dpi = 600)
  
  print(spp_list[i]) # Sanity check
}

#### Chlorophyll-a (chlos) ####
# Assemble data frame
df <- assemblePredictor(spp_list, "chlos")

# Plot scatter plots
for(i in 1:length(spp_list)) {
  
  gg <- plot_scatter_predictor(df, "chlos_transformed", spp_list[i])
  ggsave(filename = here::here(figure_dir, paste("chlos", paste0(spp_list[i], ".png"), sep = "_")), 
         plot = gg, 
         width = 5,
         height = 3,
         dpi = 600)
  
  print(spp_list[i]) # Sanity check
}