# Description: Using PCA to determine hotspots for July-September

# Define preliminaries
source("00_Preliminaries.R")
source("00_SetupGrid.R")
pacman::p_load(purrr, Hmisc, RColorBrewer, patchwork)

#### Prepare components for data frame ####

# Yellowfin tuna
obj <- readRDS(here::here(preds_dir, "YFT_jul-sept.rds"))
yft <- prepare_components(obj, "yft")

# Skipjack tuna
obj <- readRDS(here::here(preds_dir, "SKP_jul-sept.rds"))
skp <- prepare_components(obj, "skp")

# Albacore
obj <- readRDS(here::here(preds_dir, "ALB_jul-sept.rds"))
alb <- prepare_components(obj, "alb")

# Swordfish
obj <- readRDS(here::here(preds_dir, "SWO_jul-sept.rds"))
swo <- prepare_components(obj, "swo")

# Blue marlin
obj <- readRDS(here::here(preds_dir, "BLUM_jul-sept.rds"))
blum <- prepare_components(obj, "blum")

# Frigate tuna
obj <- readRDS(here::here(preds_dir, "FRI_jul-sept.rds"))
fri <- prepare_components(obj, "fri")

# Bigeye tuna
obj <- readRDS(here::here(preds_dir, "BET_jul-sept.rds"))
bet <- prepare_components(obj, "bet")

# Pacific bluefin tuna
obj <- readRDS(here::here(preds_dir, "BFT_jul-sept.rds"))
bft <- prepare_components(obj, "bft")

# Sauries
obj <- readRDS(here::here(preds_dir, "SAU_jul-sept.rds"))
sau <- prepare_components(obj, "sau")

# Sailfish
obj <- readRDS(here::here(preds_dir, "SAIL_jul-sept.rds"))
sail <- prepare_components(obj, "sail")

# Southern bluefin tuna
obj <- readRDS(here::here(preds_dir, "SBFT_jul-sept.rds"))
sbft <- prepare_components(obj, "sbft")

# Slender tuna
obj <- readRDS(here::here(preds_dir, "SLT_jul-sept.rds"))
slt <- prepare_components(obj, "slt")

# Bonitos
obj <- readRDS(here::here(preds_dir, "BON_jul-sept.rds"))
bon <- prepare_components(obj, "bon")

# Shortbill spearfish
obj <- readRDS(here::here(preds_dir, "SHOS_jul-sept.rds"))
shos <- prepare_components(obj, "shos")

# Striped marlin
obj <- readRDS(here::here(preds_dir, "STRM_jul-sept.rds"))
strm <- prepare_components(obj, "strm")

# Longfin escolar
obj <- readRDS(here::here(preds_dir, "LESC_jul-sept.rds"))
lesc <- prepare_components(obj, "lesc")

# Little tuna
obj <- readRDS(here::here(preds_dir, "LIT_jul-sept.rds"))
lit <- prepare_components(obj, "lit")

#### Assembling data frame ####

df <- list(yft, 
           skp, 
           alb,
           swo,
           blum,
           fri,
           bet,
           bft,
           sau,
           sail,
           sbft,
           slt,
           bon,
           shos,
           strm,
           lesc,
           lit
)

df <- purrr::reduce(df, dplyr::left_join, by = 'cellID')

write.csv(df, file = here::here(preds_dir, "FULL_predictions_jul-sept.csv"))

#### Run PCA ####
PCA <- stats::princomp(df %>% 
                         dplyr::select(-cellID), cor = FALSE)

summary(PCA)

PC_scores <- PCA$scores[,1:2] %>% 
  tibble::as_tibble()
write.csv(PC_scores, file = here::here(pc_dir, "hotspots_jul-sept_scores.csv"))
# PC_scores <- read_csv(here::here(pc_dir, "hotspots_jul-sept_scores.csv"))
loadings <- PCA$loadings %>% 
  as.data.frame.matrix()
write.csv(loadings, file = here::here(pc_dir, "hotspots_jul-sept_loadings.csv"))

# Creating dummy sf for PCA plots
dummy <- readRDS(here::here(preds_dir, "YFT_jul-sept.rds"))
dummy <- dummy %>% 
  dplyr::select(-ocean, -model) %>% 
  cbind(., PC_scores)

# Plot PC1
pc1 <- plotPC1_limits(dummy, "Comp.1", "PC score 1")

# Plot PC2
pc2 <- plotPC2_limits(dummy, "Comp.2", "PC score 2")

pc_plot <- (pc1 + pc2) #+
  #plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  #theme(plot.tag = element_text(size = 25))

ggsave(plot = pc_plot, filename = here::here(figure_dir, "PC_plot_jul-sept.png"), width = 35, height = 7.5, dpi = 300)

#### Pearson's correlation ####

mat <- dplyr::bind_cols(PC_scores$Comp.1, df) %>% 
  dplyr::rename(Comp = `...1`) %>% 
  dplyr::select(Comp, yft, skp, alb, fri, bet, bft, sbft, slt, bon, lit, swo, blum, sail, shos, strm, sau, lesc) %>% # reorder the columns
  as.matrix()

res <- rcorr(mat)
write_csv(res$r %>% dplyr::as_tibble(), here::here(pc_dir, "CorrMatrix_PC1_jul-sept_r.csv"))
write_csv(res$P %>% dplyr::as_tibble(), here::here(pc_dir, "CorrMatrix_PC1_jul-sept_p.csv"))

mat <- dplyr::bind_cols(PC_scores$Comp.2, df) %>% 
  dplyr::rename(Comp = `...1`) %>% 
  dplyr::select(Comp, yft, skp, alb, fri, bet, bft, sbft, slt, bon, lit, swo, blum, sail, shos, strm, sau, lesc) %>% # reorder the columns
  as.matrix()

res <- rcorr(mat)
write_csv(res$r %>% dplyr::as_tibble(), here::here(pc_dir, "CorrMatrix_PC2_jul-sept_r.csv"))
write_csv(res$P %>% dplyr::as_tibble(), here::here(pc_dir, "CorrMatrix_PC2_jul-sept_p.csv"))

rm(list=ls())  # free up environment
