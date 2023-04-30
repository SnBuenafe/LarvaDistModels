# Description: Plotting all seasonal PC scores for components 1 and 2.

# Define preliminaries
source("00_Preliminaries.R")
pc_dir <- here::here("Output", "PCA")
pred_dir <- here::here("Output", "Predictions")
fig_dir <- here::here("Figures")

# January-March
PC_scores <- read_csv(here::here(pc_dir, "hotspots_jan-mar_scores.csv")) # Load PC scores

# Creating dummy sf for PCA plots
dummy <- readRDS(here::here(pred_dir, "YFT_jan-mar.rds"))
dummy <- dummy %>% 
  dplyr::select(-ocean, -model) %>% 
  cbind(., PC_scores)

# Plot PC1
pc1 <- plotPC(dummy, "Comp.1", "PC score 1")

# Plot PC2
pc2 <- plotPC(dummy, "Comp.2", "PC score 2")

# April-June
PC_scores <- read_csv(here::here(pc_dir, "hotspots_apr-jun_scores.csv")) # Load PC scores

# Creating dummy sf for PCA plots
dummy <- readRDS(here::here(pred_dir, "YFT_apr-jun.rds"))
dummy <- dummy %>% 
  dplyr::select(-ocean, -model) %>% 
  cbind(., PC_scores)

# Plot PC1
pc3 <- plotPC(dummy, "Comp.1", "PC score 1")

# Plot PC2
pc4 <- plotPC(dummy, "Comp.2", "PC score 2")

# July-September
PC_scores <- read_csv(here::here(pc_dir, "hotspots_jul-sept_scores.csv")) # Load PC scores

# Creating dummy sf for PCA plots
dummy <- readRDS(here::here(pred_dir, "YFT_jul-sept.rds"))
dummy <- dummy %>% 
  dplyr::select(-ocean, -model) %>% 
  cbind(., PC_scores)

# Plot PC1
pc5 <- plotPC(dummy, "Comp.1", "PC score 1")

# Plot PC2
pc6 <- plotPC(dummy, "Comp.2", "PC score 2")

# October-December
PC_scores <- read_csv(here::here(pc_dir, "hotspots_oct-dec_scores.csv")) # Load PC scores

# Creating dummy sf for PCA plots
dummy <- readRDS(here::here(pred_dir, "YFT_oct-dec.rds"))
dummy <- dummy %>% 
  dplyr::select(-ocean, -model) %>% 
  cbind(., PC_scores)

# Plot PC1
pc7 <- plotPC(dummy, "Comp.1", "PC score 1")

# Plot PC2
pc8 <- plotPC(dummy, "Comp.2", "PC score 2")

# Plot everything

evry <- (pc1 + pc2) / (pc3 + pc4) / (pc5 + pc6) / (pc7 + pc8) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = evry, filename = here::here(fig_dir, "PC_plot_all.png"), width = 27, height = 30, dpi = 300)


