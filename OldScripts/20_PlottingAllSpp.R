# Load all sfs
source("01a_DataLayers_Assembling.R")

createInset <- function(df) {
  inset <- ggplot(data = df) + 
    geom_bar(aes(x = as.factor(abundance_presence), y = (..count..)/sum(..count..), fill = as.factor(abundance_presence)), show.legend = FALSE) +
    scale_color_manual(aesthetics = c("color", "fill"),
                       values = c("#feebe2", "#7a0177")) +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank())
  
  return(inset)
}

plotPresenceAbsence <- function(tib, code, name) {
  df <- tib %>% 
    dplyr::mutate(abundance_presence = case_when(abundance > 0 ~ 1,
                                                 abundance == 0 ~ 0))
  pres <- round(sum(df$abundance_presence)/nrow(df) * 100, 2)
  gg <- plotPA(df, pres, paste0("Figures/", code, "_presabs.png")) +
      xlab("Longitude") +
      ylab("Latitude") +
      ggtitle(name) +
      inset_element(createInset(df), 0, 0.7, 0.2, 1)
  return(gg)
}

plotModelAll <- function(df, code, name) {
  if(code == "YFT") {
    model <- readRDS(paste0("Output/", code, "_model5.rds"))
  } else {
    model <- readRDS(paste0("Output/", code, "_model1.rds"))
  }
  df$model <- model$fitted
  if(code == "YFT") {
    gg <- plotModel(df, paste0("Figures/", code, "_Model5.png"))
  } else {
    gg <- plotModel(df, paste0("Figures/", code, "_Model1.png"))
  }
  
  gg <- gg +
    xlab("Longitude") +
    ylab("Latitude") +
    ggtitle(name)
  return(gg)
  
}

plotPredictionsAll <- function(df, code, name) {
  ds <- read_csv(paste0("Output/", code, "_full.csv"), show_col_types = FALSE) %>% 
    dplyr::filter(is.na(abundance)) %>% # filter those with data!
    dplyr::mutate(across(where(is.character), ~factor(.))) %>% # convert all characters to factors
    dplyr::mutate(abundance_presence = case_when(abundance > 0 ~ 1,
                                                 abundance == 0 ~ 0)) %>% 
    dplyr::select(-geometry) %>% 
    as.data.frame()
  
  if(code == "YFT") {
    model <- readRDS(paste0("Output/", code, "_model5.rds"))
  } else {
    model <- readRDS(paste0("Output/", code, "_model1.rds"))
  }
  
  preds <- dismo::predict(model, ds, n.trees = model$gbm.call$best.trees, type = "response")
  ds$predictions <- preds
  
  sf <- df %>% # convert to sf so we can plot
    dplyr::left_join(ds, ., by = "cellID") %>% 
    sf::st_as_sf(sf_column_name = "geometry")
  
  if(code == "YFT") {
    gg <- plotPredictions(sf, paste0("Figures/", code, "_Model5preds.png")) # Plot predictions
  } else {
    gg <- plotPredictions(sf, paste0("Figures/", code, "_Model1preds.png")) # Plot predictions
  }
  
  gg <- gg +
    xlab("Longitude") +
    ylab("Latitude") +
    ggtitle(name)
  return(gg)
  
}

########################################################
## Plot all the presence/absence of all species
########################################################
# 1. Skipjack tuna
(ggSKP <- plotPresenceAbsence(SKP_sf, code = "SKP", name = "Skipjack tuna"))

# 2. Blue marlin & atlantic blue marlin
(ggBLUM <- plotPresenceAbsence(BLUM_sf, code = "BLUM", name = "Blue marlin"))

# 3. Yellowfin tuna
(ggYFT <- plotPresenceAbsence(YFT_sf, code = "YFT", name = "Yellowfin tuna"))

# 4. Albacore
(ggALB <- plotPresenceAbsence(ALB_sf, code = "ALB", name = "Albacore"))

# 5. Shortbill spearfish and longbill spearfish
(ggSHOS <- plotPresenceAbsence(SHOS_sf, code = "SHOS", name = "Shortbill spearfish"))

# 6. Frigate tuna
(ggFRI <- plotPresenceAbsence(FRI_sf, code = "FRI", name = "Frigate tuna"))

collect1 <- (ggSKP + ggBLUM) / (ggYFT + ggALB) / (ggSHOS + ggFRI) + plot_layout(guides = "collect")
pdf(file = "Figures/PresenceAbsence_1.pdf", width = 20, height = 22)
collect1
dev.off()

# 7. Bigeye tuna
(ggBET <- plotPresenceAbsence(BET_sf, code = "BET", name = "Bigeye tuna"))

# 8. Swordfish
(ggSWO <- plotPresenceAbsence(SWO_sf, code = "SWO", name = "Swordfish"))

# 9. Striped marlin
(ggSTRM <- plotPresenceAbsence(STRM_sf, code = "STRM", name = "Striped marlin"))

# 10. Sauries
(ggSAU <- plotPresenceAbsence(SAU_sf, code = "SAU", name = "Sauries"))

# 11. Sailfish
(ggSAIL <- plotPresenceAbsence(SAIL_sf, code = "SAIL", name = "Sailfish"))

# 12. Longfin escolar
(ggLESC <- plotPresenceAbsence(LESC_sf, code = "LESC", name = "Longfin escolar"))

collect2 <- (ggBET + ggSWO) / (ggSTRM + ggSAU) / (ggSAIL + ggLESC) + plot_layout(guides = "collect")
pdf(file = "Figures/PresenceAbsence_2.pdf", width = 20, height = 22)
collect2
dev.off()

# 13. Bluefin tuna
(ggBFT <- plotPresenceAbsence(BFT_sf, code = "BFT", name = "Bluefin tuna"))

# 14. Little tuna
(ggLIT <- plotPresenceAbsence(LIT_sf, code = "LIT", name = "Little tuna"))

# 15. Southern bluefin tuna
(ggSBFT <- plotPresenceAbsence(SBFT_sf, code = "SBFT", name = "Southern bluefin tuna"))

# 16. Slender tuna
(ggSLT <- plotPresenceAbsence(SLT_sf, code = "SLT", name = "Slender tuna"))

# 17. Bonitos
(ggBON<- plotPresenceAbsence(BON_sf, code = "BON", name = "Bonitos"))

# 18. Black marlin
(ggBLAM <- plotPresenceAbsence(BLAM_sf, code = "BLAM", name = "Black marlin"))

collect3 <- (ggBFT + ggLIT) / (ggSBFT + ggSLT) / (ggBON + ggBLAM) + plot_layout(guides = "collect")
pdf(file = "Figures/PresenceAbsence_3.pdf", width = 20, height = 22)
collect3
dev.off()

########################################################
## Plot all model outputs of all species
########################################################
# 1. Skipjack tuna
(ggSKP <- plotModelAll(SKP_sf, "SKP", "Skipjack tuna"))

# 2. Blue marlin & atlantic blue marlin
(ggBLUM <- plotModelAll(SKP_sf, "BLUM", "Blue marlin"))

# 3. Yellowfin tuna
(ggYFT <- plotModelAll(YFT_sf, code = "YFT", name = "Yellowfin tuna"))

# 4. Albacore
(ggALB <- plotModelAll(ALB_sf, code = "ALB", name = "Albacore"))

# 5. Shortbill spearfish and longbill spearfish
(ggSHOS <- plotModelAll(SHOS_sf, code = "SHOS", name = "Shortbill spearfish"))

# 6. Frigate tuna
(ggFRI <- plotModelAll(FRI_sf, code = "FRI", name = "Frigate tuna"))

collect1 <- (ggSKP + ggBLUM) / (ggYFT + ggALB) / (ggSHOS + ggFRI) + plot_layout(guides = "collect")
pdf(file = "Figures/Model_1.pdf", width = 20, height = 22)
collect1
dev.off()

# 7. Bigeye tuna
(ggBET <- plotModelAll(BET_sf, code = "BET", name = "Bigeye tuna"))

# 8. Swordfish
(ggSWO <- plotModelAll(SWO_sf, code = "SWO", name = "Swordfish"))

# 9. Striped marlin
(ggSTRM <- plotModelAll(STRM_sf, code = "STRM", name = "Striped marlin"))

# 10. Sauries
(ggSAU <- plotModelAll(SAU_sf, code = "SAU", name = "Sauries"))

# 11. Sailfish
(ggSAIL <- plotModelAll(SAIL_sf, code = "SAIL", name = "Sailfish"))

# 12. Longfin escolar
(ggLESC <- plotModelAll(LESC_sf, code = "LESC", name = "Longfin escolar"))

collect2 <- (ggBET + ggSWO) / (ggSTRM + ggSAU) / (ggSAIL + ggLESC) + plot_layout(guides = "collect")
pdf(file = "Figures/Model_2.pdf", width = 20, height = 22)
collect2
dev.off()

# 13. Bluefin tuna
(ggBFT <- plotModelAll(BFT_sf, code = "BFT", name = "Bluefin tuna"))

# 14. Little tuna
(ggLIT <- plotModelAll(LIT_sf, code = "LIT", name = "Little tuna"))

# 15. Southern bluefin tuna
(ggSBFT <- plotModelAll(SBFT_sf, code = "SBFT", name = "Southern bluefin tuna"))

# 16. Slender tuna
(ggSLT <- plotModelAll(SLT_sf, code = "SLT", name = "Slender tuna"))

# 17. Bonitos
(ggBON<- plotModelAll(BON_sf, code = "BON", name = "Bonitos"))

# 18. Black marlin
(ggBLAM <- plotModelAll(BLAM_sf, code = "BLAM", name = "Black marlin"))

collect3 <- (ggBFT + ggLIT) / (ggSBFT + ggSLT) / (ggBON + plot_spacer()) + plot_layout(guides = "collect")
pdf(file = "Figures/Model_3.pdf", width = 20, height = 22)
collect3
dev.off()

########################################################
## Plot all the historical predictions of all species
########################################################

# 1. Skipjack tuna
(ggSKP <- plotPredictionsAll(grid_SKP, code = "SKP", name = "Skipjack tuna"))

# 2. Blue marlin & atlantic blue marlin
(ggBLUM <- plotPredictionsAll(grid_BLUM, code = "BLUM", name = "Blue marlin"))

# 3. Yellowfin tuna
(ggYFT <- plotPredictionsAll(grid_YFT, code = "YFT", name = "Yellowfin tuna"))

# 4. Albacore
(ggALB <- plotPredictionsAll(grid_ALB, code = "ALB", name = "Albacore"))

# 5. Shortbill spearfish and longbill spearfish
(ggSHOS <- plotPredictionsAll(grid_SHOS, code = "SHOS", name = "Shortbill spearfish"))

# 6. Frigate tuna
(ggFRI <- plotPredictionsAll(grid_FRI, code = "FRI", name = "Frigate tuna"))

collect1 <- (ggSKP + ggBLUM) / (ggYFT + ggALB) / (ggSHOS + ggFRI) + plot_layout(guides = "collect")
pdf(file = "Figures/Predictions_1.pdf", width = 20, height = 22)
collect1
dev.off()

# 7. Bigeye tuna
(ggBET <- plotPredictionsAll(grid_BET, code = "BET", name = "Bigeye tuna"))

# 8. Swordfish
(ggSWO <- plotPredictionsAll(grid_SWO, code = "SWO", name = "Swordfish"))

# 9. Striped marlin
(ggSTRM <- plotPredictionsAll(grid_STRM, code = "STRM", name = "Striped marlin"))

# 10. Sauries
(ggSAU <- plotPredictionsAll(grid_SAU, code = "SAU", name = "Sauries"))

# 11. Sailfish
(ggSAIL <- plotPredictionsAll(grid_SAIL, code = "SAIL", name = "Sailfish"))

# 12. Longfin escolar
(ggLESC <- plotPredictionsAll(grid_LESC, code = "LESC", name = "Longfin escolar"))

collect2 <- (ggBET + ggSWO) / (ggSTRM + ggSAU) / (ggSAIL + ggLESC) + plot_layout(guides = "collect")
pdf(file = "Figures/Predictions_2.pdf", width = 20, height = 22)
collect2
dev.off()

# 13. Bluefin tuna
(ggBFT <- plotPredictionsAll(grid_BFT, code = "BFT", name = "Bluefin tuna"))

# 14. Little tuna
(ggLIT <- plotPredictionsAll(grid_LIT, code = "LIT", name = "Little tuna"))

# 15. Southern bluefin tuna
(ggSBFT <- plotPredictionsAll(grid_SBFT, code = "SBFT", name = "Southern bluefin tuna"))

# 16. Slender tuna
(ggSLT <- plotPredictionsAll(grid_SLT, code = "SLT", name = "Slender tuna"))

# 17. Bonitos
(ggBON<- plotPredictionsAll(grid_BON, code = "BON", name = "Bonitos"))

# 18. Black marlin
(ggBLAM <- plotPredictionsAll(grid_BLAM, code = "BLAM", name = "Black marlin"))

collect3 <- (ggBFT + ggLIT) / (ggSBFT + ggSLT) / (ggBON + plot_spacer()) + plot_layout(guides = "collect")
pdf(file = "Figures/Predictions_3.pdf", width = 20, height = 22)
collect3
dev.off()
