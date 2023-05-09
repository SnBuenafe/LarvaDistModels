# Description: Plotting all seasonal PC scores for components 1 and 2.

# Define preliminaries
source("00_Preliminaries.R")
source("00_SetupGrid.R")
pacman::p_load(purrr, Hmisc, RColorBrewer, patchwork)

# January-March
PC_scores <- read_csv(here::here(pc_dir, "hotspots_jan-mar_scores.csv")) # Load PC scores

# Creating dummy sf for PCA plots
dummy <- readRDS(here::here(preds_dir, "YFT_jan-mar.rds"))
dummy <- dummy %>% 
  dplyr::select(-ocean, -model) %>% 
  cbind(., PC_scores)

# Plot PC1
pc1 <- plotPC1_limits(dummy, "Comp.1", "PC score 1")
ggsave(plot = pc1, filename = here::here(fig_dir, paste("PC1", "jan-mar.png", sep = "_")), width = 15, height = 7, dpi = 600)

# Plot PC2
pc2 <- plotPC2_limits(dummy, "Comp.2", "PC score 2 ")
ggsave(plot = pc2, filename = here::here(fig_dir, paste("PC2", "jan-mar.png", sep = "_")), width = 15, height = 7, dpi = 600)

# April-June
PC_scores <- read_csv(here::here(pc_dir, "hotspots_apr-jun_scores.csv")) # Load PC scores

# Creating dummy sf for PCA plots
dummy <- readRDS(here::here(preds_dir, "YFT_apr-jun.rds"))
dummy <- dummy %>% 
  dplyr::select(-ocean, -model) %>% 
  cbind(., PC_scores)

# Plot PC1
pc3 <- plotPC1_limits(dummy, "Comp.1", "PC score 1")
ggsave(plot = pc3, filename = here::here(fig_dir, paste("PC1", "apr-jun.png", sep = "_")), width = 15, height = 7, dpi = 600)

# Plot PC2
pc4 <- plotPC2_limits(dummy, "Comp.2", "PC score 2 ")
ggsave(plot = pc4, filename = here::here(fig_dir, paste("PC2", "apr-jun.png", sep = "_")), width = 15, height = 7, dpi = 600)

# July-September
PC_scores <- read_csv(here::here(pc_dir, "hotspots_jul-sept_scores.csv")) # Load PC scores

# Creating dummy sf for PCA plots
dummy <- readRDS(here::here(preds_dir, "YFT_jul-sept.rds"))
dummy <- dummy %>% 
  dplyr::select(-ocean, -model) %>% 
  cbind(., PC_scores)

# Plot PC1
pc5 <- plotPC1_limits(dummy, "Comp.1", "PC score 1")
ggsave(plot = pc5, filename = here::here(fig_dir, paste("PC1", "jul-sept.png", sep = "_")), width = 15, height = 7, dpi = 600)

# Plot PC2
pc6 <- plotPC2_limits(dummy, "Comp.2", "PC score 2 ")
ggsave(plot = pc6, filename = here::here(fig_dir, paste("PC2", "jul-sept.png", sep = "_")), width = 15, height = 7, dpi = 600)

# October-December
PC_scores <- read_csv(here::here(pc_dir, "hotspots_oct-dec_scores.csv")) # Load PC scores

# Creating dummy sf for PCA plots
dummy <- readRDS(here::here(preds_dir, "YFT_oct-dec.rds"))
dummy <- dummy %>% 
  dplyr::select(-ocean, -model) %>% 
  cbind(., PC_scores)

# Plot PC1
pc7 <- plotPC1_limits(dummy, "Comp.1", "PC score 1")
ggsave(plot = pc7, filename = here::here(fig_dir, paste("PC1", "oct-dec.png", sep = "_")), width = 15, height = 7, dpi = 600)

# Plot PC2
pc8 <- plotPC2_limits(dummy, "Comp.2", "PC score 2  ")
ggsave(plot = pc8, filename = here::here(fig_dir, paste("PC2", "oct-dec.png", sep = "_")), width = 15, height = 7, dpi = 600)

# Plot everything

evry <- (pc1 + pc2) / (pc3 + pc4) / (pc5 + pc6) / (pc7 + pc8) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = evry, filename = here::here(fig_dir, "PC_plot_all.png"), width = 27, height = 30, dpi = 300)


