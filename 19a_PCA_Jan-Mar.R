# DESCRIPTION: Using PCA to determine hotspots for January-March

# Define preliminaries
source("00_Preliminaries.R")
pred_dir <- here::here("Output", "Predictions")
model_dir <- here::here("Output", "Models")
pc_dir <- here::here("Output", "PCA")

#### Prepare components for data frame ####

# Yellowfin tuna
obj <- readRDS(here::here(pred_dir, "YFT_jan-mar.rds"))
yft <- prepare_components(obj, "yft")

# Skipjack tuna
obj <- readRDS(here::here(pred_dir, "SKP_jan-mar.rds"))
skp <- prepare_components(obj, "skp")

# Albacore
obj <- readRDS(here::here(pred_dir, "ALB_jan-mar.rds"))
alb <- prepare_components(obj, "alb")

# Swordfish
obj <- readRDS(here::here(pred_dir, "SWO_jan-mar.rds"))
swo <- prepare_components(obj, "swo")

# Blue marlin
obj <- readRDS(here::here(pred_dir, "BLUM_jan-mar.rds"))
blum <- prepare_components(obj, "blum")

# Frigate tuna
obj <- readRDS(here::here(pred_dir, "FRI_jan-mar.rds"))
fri <- prepare_components(obj, "fri")

# Bigeye tuna
obj <- readRDS(here::here(pred_dir, "BET_jan-mar.rds"))
bet <- prepare_components(obj, "bet")

# Pacific bluefin tuna
obj <- readRDS(here::here(pred_dir, "BFT_jan-mar.rds"))
bft <- prepare_components(obj, "bft")

# Sauries
obj <- readRDS(here::here(pred_dir, "SAU_jan-mar.rds"))
sau <- prepare_components(obj, "sau")

# Sailfish
obj <- readRDS(here::here(pred_dir, "SAIL_jan-mar.rds"))
sail <- prepare_components(obj, "sail")

# Southern bluefin tuna
obj <- readRDS(here::here(pred_dir, "SBFT_jan-mar.rds"))
sbft <- prepare_components(obj, "sbft")

# Slender tuna
obj <- readRDS(here::here(pred_dir, "SLT_jan-mar.rds"))
slt <- prepare_components(obj, "slt")

# Bonitos
obj <- readRDS(here::here(pred_dir, "BON_jan-mar.rds"))
bon <- prepare_components(obj, "bon")

# Shortbill spearfish
obj <- readRDS(here::here(pred_dir, "SHOS_jan-mar.rds"))
shos <- prepare_components(obj, "shos")

# Striped marlin
obj <- readRDS(here::here(pred_dir, "STRM_jan-mar.rds"))
strm <- prepare_components(obj, "strm")

# Longfin escolar
obj <- readRDS(here::here(pred_dir, "LESC_jan-mar.rds"))
lesc <- prepare_components(obj, "lesc")

# Little tuna
obj <- readRDS(here::here(pred_dir, "LIT_jan-mar.rds"))
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

write.csv(df, file = here::here(pred_dir, "FULL_predictions_jan-mar.csv"))

#### Run PCA ####
PCA <- stats::princomp(df %>% 
                         dplyr::select(-cellID), cor = FALSE)

summary(PCA)

PC_scores <- PCA$scores[,1:2] %>% 
  tibble::as_tibble()
write.csv(PC_scores, file = here::here(pc_dir, "hotspots_jan-mar_scores.csv"))
# PC_scores <- read_csv(here::here(pc_dir, "hotspots_jan-mar_scores.csv"))
write.csv(PCA$loadings, file = here::here(pc_dir, "hotspots_jan-mar_loadings.csv"))

# Creating dummy sf for PCA plots
dummy <- readRDS(here::here(pred_dir, "YFT_jan-mar.rds"))
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

ggsave(plot = pc_plot, filename = "Figures/PC_plot_jan-mar.png", width = 35, height = 7.5, dpi = 300)

#### Pearson's correlation ####

mat <- dplyr::bind_cols(PC_scores$Comp.1, df) %>% 
  dplyr::rename(Comp = `...1`) %>% 
  dplyr::select(Comp, yft, skp, alb, fri, bet, bft, sbft, slt, bon, lit, swo, blum, sail, shos, strm, sau, lesc) %>% # reorder the columns
  as.matrix()

res <- rcorr(mat) 
write_csv(res$r %>% dplyr::as_tibble(), here::here(pc_dir, "CorrMatrix_PC1_jan-mar_r.csv"))
write_csv(res$P %>% dplyr::as_tibble(), here::here(pc_dir, "CorrMatrix_PC1_jan-mar_p.csv"))

mat <- dplyr::bind_cols(PC_scores$Comp.2, df) %>% 
  dplyr::rename(Comp = `...1`) %>% 
  dplyr::select(Comp, yft, skp, alb, fri, bet, bft, sbft, slt, bon, lit, swo, blum, sail, shos, strm, sau, lesc) %>% # reorder the columns
  as.matrix()

res <- rcorr(mat)
write_csv(res$r %>% dplyr::as_tibble(), here::here(pc_dir, "CorrMatrix_PC2_jan-mar_r.csv"))
write_csv(res$P %>% dplyr::as_tibble(), here::here(pc_dir, "CorrMatrix_PC2_jan-mar_p.csv"))

rm(list=ls())  # free up environment
