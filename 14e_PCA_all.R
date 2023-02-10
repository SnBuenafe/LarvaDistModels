source("02a_YFT_Data.R") # load dummy data

# Function for plotting PC1
plot_PC1 <- function(dummy) {
  ggplot() + 
    geom_sf(data = dummy, aes(fill = Comp.1, color = Comp.1), size = 0.1) +
    scale_fill_gradientn(name = "PC score 1",
                         aesthetics = c("fill", "color"),
                         colors = rev(brewer.pal(11, "RdBu")),
                         na.value = "grey64",
                         oob = scales::squish,
                         guide = guide_colourbar(
                           title.vjust = 0.5,
                           barheight = grid::unit(0.01, "npc"),
                           barwidth = grid::unit(0.25, "npc"),
                           frame.colour = "black")) +
    geom_sf(data = landmass, fill = "black", color = NA, size = 0.01) +
    xlab("Longitude") +
    ylab("Latitude") +
    theme_bw() +
    gg_add_text(., "white")
}

# Function for plotting PC2
plot_PC2 <- function(dummy) {
  ggplot() + 
    geom_sf(data = dummy, aes(fill = Comp.2, color = Comp.2), size = 0.1) +
    scale_fill_gradientn(name = "PC score 2",
                         aesthetics = c("fill", "color"),
                         colors = rev(brewer.pal(11, "RdBu")),
                         na.value = "grey64",
                         oob = scales::squish,
                         guide = guide_colourbar(
                           title.vjust = 0.5,
                           barheight = grid::unit(0.01, "npc"),
                           barwidth = grid::unit(0.25, "npc"),
                           frame.colour = "black")) +
    geom_sf(data = landmass, fill = "black", color = NA, size = 0.01) +
    xlab("Longitude") +
    ylab("Latitude") +
    theme_bw() +
    gg_add_text(., "black")
}

#### Plotting January-March ####
PC_scores <- read_csv("Output/PCA/hotspots_jan-mar_scores.csv") %>% 
  dplyr::select(-1)

# Creating dummy sf for PCA
dummy <- plotSeasonPredict(train_tmp,
                           test_tmp,
                           "jan-mar", # season
                           YFT_predict_season1 %>% dplyr::filter(latitude >= min(YFT_build$latitude) & latitude <= max(YFT_build$latitude)),
                           YFT_model12, # BRT model
                           `grid_YFT_jan-mar` %>% dplyr::filter(latitude >= min(YFT_build$latitude) & latitude <= max(YFT_build$latitude))
)

dummy <- dummy[[1]] %>% 
  dplyr::select(-ocean, -model) %>% 
  cbind(., PC_scores)

# Plot PC1: Jan-Mar
pc1 <- plot_PC1(dummy)

# Plot PC2: Jan-Mar
pc2 <- plot_PC2(dummy)

#### Plotting April-June ####
PC_scores <- read_csv("Output/PCA/hotspots_apr-jun_scores.csv") %>% 
  dplyr::select(-1)

# Creating dummy sf for PCA
dummy <- plotSeasonPredict(train_tmp,
                           test_tmp,
                           "apr-jun", # season
                           YFT_predict_season2 %>% dplyr::filter(latitude >= min(YFT_build$latitude) & latitude <= max(YFT_build$latitude)),
                           YFT_model12, # BRT model
                           `grid_YFT_apr-jun` %>% dplyr::filter(latitude >= min(YFT_build$latitude) & latitude <= max(YFT_build$latitude))
)

dummy <- dummy[[1]] %>% 
  dplyr::select(-ocean, -model) %>% 
  cbind(., PC_scores)

# Plot PC1: Apr-Jun
pc3 <- plot_PC1(dummy)

# Plot PC2: Apr-Jun
pc4 <- plot_PC2(dummy)

#### Plotting July-September ####
PC_scores <- read_csv("Output/PCA/hotspots_jul-sept_scores.csv") %>% 
  dplyr::select(-1)

# Creating dummy sf for PCA
dummy <- plotSeasonPredict(train_tmp,
                           test_tmp,
                           "jul-sept", # season
                           YFT_predict_season3 %>% dplyr::filter(latitude >= min(YFT_build$latitude) & latitude <= max(YFT_build$latitude)),
                           YFT_model12, # BRT model
                           `grid_YFT_jul-sept` %>% dplyr::filter(latitude >= min(YFT_build$latitude) & latitude <= max(YFT_build$latitude))
)

dummy <- dummy[[1]] %>% 
  dplyr::select(-ocean, -model) %>% 
  cbind(., PC_scores)

# Plot PC1: Jul-Sept
pc5 <- plot_PC1(dummy)

# Plot PC2: Jul-Sept
pc6 <- plot_PC2(dummy)

#### Plotting October-December ####
PC_scores <- read_csv("Output/PCA/hotspots_oct-dec_scores.csv") %>% 
  dplyr::select(-1)

# Creating dummy sf for PCA
dummy <- plotSeasonPredict(train_tmp,
                           test_tmp,
                           "oct-dec", # season
                           YFT_predict_season4 %>% dplyr::filter(latitude >= min(YFT_build$latitude) & latitude <= max(YFT_build$latitude)),
                           YFT_model12, # BRT model
                           `grid_YFT_oct-dec` %>% dplyr::filter(latitude >= min(YFT_build$latitude) & latitude <= max(YFT_build$latitude))
)

dummy <- dummy[[1]] %>% 
  dplyr::select(-ocean, -model) %>% 
  cbind(., PC_scores)

# Plot PC1: Oct-Dec
pc7 <- plot_PC1(dummy)

# Plot PC2: Oct-Dec
pc8 <- plot_PC2(dummy)

#### Plot everything ####
pc_plot <- (pc1 + pc2) / (pc3 + pc4) / (pc5 + pc6) / (pc7 + pc8) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = pc_plot, filename = "Figures/PC_plot_all.png", width = 27, height = 30, dpi = 300)


