# DESCRIPTION: Using PCA to determine hotspots for January-March

# Define preliminaries
source("00_Preliminaries.R")
pred_dir <- here::here("Output", "Predictions")
model_dir <- here::here("Output", "Models")

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
           bon
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
write.csv(PCA$loadings, file = here::here(pc_dir, "hotspots_jan-mar_loadings.csv"))

# Creating dummy sf for PCA plots
dummy <- readRDS(here::here(pred_dir, "YFT_jan-mar.rds"))
dummy <- dummy %>% 
  dplyr::select(-ocean, -model) %>% 
  cbind(., PC_scores)

# Plot PC1
pc1 <- plotPC(dummy, "Comp.1", "PC score 1")

# Plot PC2
pc2 <- plotPC(dummy, "Comp.2", "PC score 2")

pc_plot <- (pc1 + pc2) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = pc_plot, filename = "Figures/PC_plot_jan-mar.png", width = 27, height = 7.5, dpi = 300)

#### Pearson's correlation ####

file_path_test = "Figures/CorrMatrix_PC1_jan-mar.png"
png(height=1200, width=1200, res = 200, file=file_path_test, type = "cairo")

mat <- dplyr::bind_cols(PC_scores$Comp.1, df) %>% 
  dplyr::rename(Comp = `...1`) %>% 
  dplyr::select(Comp, yft, skp, alb, fri, bet, bft, sbft, slt, bon, swo, blum, sail, sau) %>% # reorder the columns
  as.matrix()

res <- rcorr(mat)

corrplot(res$r, 
         type = "upper", 
         order = "original", 
         tl.col = "black", # change to white when preparing the figures for publication
         # addCoef.col = "black", 
         tl.srt = 45, 
         insig = "blank", 
         col = COL2('BrBG', 200),
         diag = FALSE)

dev.off()

file_path_test = "Figures/CorrMatrix_PC2_jan-mar.png"
png(height=1200, width=1200, res = 200, file=file_path_test, type = "cairo")

mat <- dplyr::bind_cols(PC_scores$Comp.2, df) %>% 
  dplyr::rename(Comp = `...1`) %>% 
  dplyr::select(Comp, yft, skp, alb, fri, bet, bft, sbft, slt, bon, swo, blum, sail, sau) %>% # reorder the columns
  as.matrix()

res <- rcorr(mat)

corrplot(res$r, 
         type = "lower", 
         order = "original", 
         tl.col = "black", # change to white when preparing the figures for publication
         #addCoef.col = "black", 
         tl.srt = 45, 
         insig = "blank", 
         col = COL2('BrBG', 200),
         diag = FALSE)

dev.off()

rm(list=ls())  # free up environment
