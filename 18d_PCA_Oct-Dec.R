# DESCRIPTION: Using PCA to determine hotspots for October-December

# Define preliminaries
source("00_SetupGrid.R")
source("00_Preliminaries.R")
pacman::p_load(purrr, Hmisc, RColorBrewer, patchwork)
figure_dir <- here::here(figure_dir, "PCA")

#### Prepare components for data frame ####
spp_list <- c("yft", "skp", "alb", "swo", "blum", "fri", "bet", "bft", "sau", "sail", "sbft", "slt",
              "shos", "strm", "lesc")

df <- list() # empty list
for(i in 1:length(spp_list)) {
  obj <- readRDS(here::here(preds_dir, paste(toupper(spp_list[i]), "oct-dec.rds", sep = "_")))
  df[[i]] <- prepare_components(obj, spp_list[i])
}

df <- purrr::reduce(df, dplyr::left_join, by = c("cellID", "grid_100_category", "geometry"))

#### Run PCA ####
PCA <- stats::princomp(df %>% 
                         dplyr::select(-cellID, -grid_100_category, -geometry), cor = FALSE)

summary(PCA)

PC_scores <- PCA$scores[,1:2] %>% # first two axes
  tibble::as_tibble()
write.csv(PC_scores, file = here::here(pc_dir, "hotspots_oct-dec_scores.csv"))
# PC_scores <- read_csv(here::here(pc_dir, "hotspots_oct-dec_scores.csv"))

loadings <- PCA$loadings %>% 
  as.data.frame.matrix()
write.csv(loadings, file = here::here(pc_dir, "hotspots_oct-dec_loadings.csv"))

#### Plotting PCA ####
pc_obj <- df %>% 
  dplyr::select(cellID, grid_100_category, geometry) %>% 
  cbind(., PC_scores) %>% 
  sf::st_as_sf(crs = cCRS)

# Plot PC1
pc1 <- plotPC1_limits(pc_obj, "Comp.1", "PC1 score")
ggsave(plot = pc1, filename = here::here(figure_dir, "PCA1_oct-dec.png"), width = 14, height = 5, dpi = 600)

# Plot PC2
pc2 <- plotPC2_limits(pc_obj, "Comp.2", "PC2 score  ")
ggsave(plot = pc2, filename = here::here(figure_dir, "PCA2_oct-dec.png"), width = 14, height = 5, dpi = 600)

#### Plotting scree plots ####
var_explained = PCA$sdev^2 / sum(PCA$sdev^2)
scree_gg <- plotScree(var_explained)

ggsave(plot = scree_gg, filename = here::here(figure_dir, "PCA_scree_oct-dec.png"), width = 7, height = 5, dpi = 600)
