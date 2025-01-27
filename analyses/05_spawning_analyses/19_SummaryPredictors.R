# DESCRIPTION: Summary plots for surface temperature
# Scatter plots with Loess smoother

# Define preliminaries
source(file.path("analyses", "02_preliminaries", "00_Preliminaries.R"))
pacman::p_load(ggridges, patchwork, purrr, sf)
figure_dir <- file.path(figure_dir, "predictors")

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
  ggsave(filename = file.path(figure_dir, paste("tos", paste0(spp_list[i], ".png"), sep = "_")), 
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
  ggsave(filename = file.path(figure_dir, paste("phos", paste0(spp_list[i], ".png"), sep = "_")), 
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
  ggsave(filename = file.path(figure_dir, paste("o2os", paste0(spp_list[i], ".png"), sep = "_")), 
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
  ggsave(filename = file.path(figure_dir, paste("chlos", paste0(spp_list[i], ".png"), sep = "_")), 
         plot = gg, 
         width = 5,
         height = 3,
         dpi = 600)
  
  print(spp_list[i]) # Sanity check
}

#### Salinity (sos) ####
# Assemble data frame
df <- assemblePredictor(spp_list, "sos")

# Plot scatter plots
for(i in 1:length(spp_list)) {
  
  gg <- plot_scatter_predictor(df, "sos_transformed", spp_list[i])
  ggsave(filename = file.path(figure_dir, paste("sos", paste0(spp_list[i], ".png"), sep = "_")), 
         plot = gg, 
         width = 5,
         height = 3,
         dpi = 600)
  
  print(spp_list[i]) # Sanity check
}

#### Mixed layer thickness (mlotst) ####
# Assemble data frame
df <- assemblePredictor(spp_list, "mlotst")

# Plot scatter plots
for(i in 1:length(spp_list)) {
  
  gg <- plot_scatter_predictor(df, "mlotst_transformed", spp_list[i])
  ggsave(filename = file.path(figure_dir, paste("mlotst", paste0(spp_list[i], ".png"), sep = "_")), 
         plot = gg, 
         width = 5,
         height = 3,
         dpi = 600)
  
  print(spp_list[i]) # Sanity check
}

#### Nitrate (no3os) ####
# Assemble data frame
df <- assemblePredictor(spp_list, "no3os")

# Plot scatter plots
for(i in 1:length(spp_list)) {
  
  gg <- plot_scatter_predictor(df, "no3os_transformed", spp_list[i])
  ggsave(filename = file.path(figure_dir, paste("no3os", paste0(spp_list[i], ".png"), sep = "_")), 
         plot = gg, 
         width = 5,
         height = 3,
         dpi = 600)
  
  print(spp_list[i]) # Sanity check
}

#### Phosphate (po4os) ####
# Assemble data frame
df <- assemblePredictor(spp_list, "po4os")

# Plot scatter plots
for(i in 1:length(spp_list)) {
  
  gg <- plot_scatter_predictor(df, "po4os_transformed", spp_list[i])
  ggsave(filename = file.path(figure_dir, paste("po4os", paste0(spp_list[i], ".png"), sep = "_")), 
         plot = gg, 
         width = 5,
         height = 3,
         dpi = 600)
  
  print(spp_list[i]) # Sanity check
}

#### Ammonium (nh4os) ####
# Assemble data frame
df <- assemblePredictor(spp_list, "nh4os")

# Plot scatter plots
for(i in 1:length(spp_list)) {
  
  gg <- plot_scatter_predictor(df, "nh4os_transformed", spp_list[i])
  ggsave(filename = file.path(figure_dir, paste("nh4os", paste0(spp_list[i], ".png"), sep = "_")), 
         plot = gg, 
         width = 5,
         height = 3,
         dpi = 600)
  
  print(spp_list[i]) # Sanity check
}

#### Thermal front (thermal_front) ####
# Assemble data frame
df <- assemblePredictor(spp_list, "thermal_front")

# Plot scatter plots
for(i in 1:length(spp_list)) {
  
  gg <- plot_scatter_predictor(df, "thermal_front_transformed", spp_list[i])
  ggsave(filename = file.path(figure_dir, paste("thermal_front", paste0(spp_list[i], ".png"), sep = "_")), 
         plot = gg, 
         width = 5,
         height = 3,
         dpi = 600)
  
  print(spp_list[i]) # Sanity check
}

#### Salinity front (salinity_front) ####
# Assemble data frame
df <- assemblePredictor(spp_list, "salinity_front")

# Plot scatter plots
for(i in 1:length(spp_list)) {
  
  gg <- plot_scatter_predictor(df, "salinity_front_transformed", spp_list[i])
  ggsave(filename = file.path(figure_dir, paste("salinity_front", paste0(spp_list[i], ".png"), sep = "_")), 
         plot = gg, 
         width = 9,
         height = 4,
         dpi = 600)
  
  print(spp_list[i]) # Sanity check
}


#### Eddy kinetic energy (EKE) ####
# Assemble data frame
df <- assemblePredictor(spp_list, "eke")

for(i in 1:length(spp_list)) {
  
  gg <- plot_scatter_predictor(df, "eke", spp_list[i])
  ggsave(filename = file.path(figure_dir, paste("eke", paste0(spp_list[i], ".png"), sep = "_")), 
         plot = gg, 
         width = 5,
         height = 3,
         dpi = 600)
  
  print(spp_list[i]) # Sanity check
}

#### Distance to nearest coastline (coast_distance) ####
df <- assemblePredictor(spp_list, "coast_distance") %>% 
  dplyr::mutate(coastDistance = coastDistance/1000) # convert from m to km

for(i in 1:length(spp_list)) {
  
  gg <- plot_scatter_predictor(df, "coastDistance", spp_list[i])
  ggsave(filename = file.path(figure_dir, paste("coast_distance", paste0(spp_list[i], ".png"), sep = "_")), 
         plot = gg, 
         width = 5,
         height = 3,
         dpi = 600)
  
  print(spp_list[i]) # Sanity check
}

#### Mean depth (mean_depth) ####
df <- assemblePredictor(spp_list, "gebco") %>% 
  dplyr::mutate(meanDepth = meanDepth/1000) # convert from m to km

for(i in 1:length(spp_list)) {
  
  gg <- plot_scatter_predictor(df, "meanDepth", spp_list[i])
  ggsave(filename = file.path(figure_dir, paste("mean_depth", paste0(spp_list[i], ".png"), sep = "_")), 
         plot = gg, 
         width = 5,
         height = 3,
         dpi = 600)
  
  print(spp_list[i]) # Sanity check
}

