# Description: Using PCA to determine hotspots for July-September

#### Prepare components for data frame ####
# ---- YFT ----
source("02a_YFT_Data.R") # Load YFT data
YFT_model6 <- readRDS("Output/Models/YFT_model6.rds") # load model

train_tmp <- train %>% 
  dplyr::mutate(model = YFT_model6$fitted)
preds <- gbm::predict.gbm(YFT_model6, test, n.trees = YFT_model6$gbm.call$best.trees, type = "response") # predict to test

test_tmp <- test %>% 
  dplyr::mutate(model = preds)

yft <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                         test_tmp, # testing object with model column (predictions)
                         "jul-sept", # season
                         YFT_predict_season3 %>% dplyr::filter(latitude >= min(YFT_build$latitude) & latitude <= max(YFT_build$latitude)), # rest of the ocean cells
                         YFT_model6, # BRT model
                         `grid_YFT_jul-sept` %>% dplyr::filter(latitude >= min(YFT_build$latitude) & latitude <= max(YFT_build$latitude)) # grid of species for specific season with restricted ranges
)

yft <- yft[[1]] %>% 
  dplyr::arrange(cellID) %>% 
  tibble::as_tibble() %>% 
  dplyr::select(model) %>% 
  pull()

# ---- SKP ----
source("03a_SKP_Data.R") # Load SKP data
SKP_model2 <- readRDS("Output/Models/SKP_model2.rds") # load model

train_tmp <- train %>% 
  dplyr::mutate(model = SKP_model2$fitted)
preds <- gbm::predict.gbm(SKP_model2, test, n.trees = SKP_model2$gbm.call$best.trees, type = "response") # predict to test

test_tmp <- test %>% 
  dplyr::mutate(model = preds)

skp <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                         test_tmp, # testing object with model column (predictions)
                         "jul-sept", # season
                         SKP_predict_season3 %>% dplyr::filter(latitude >= min(SKP_build$latitude) & latitude <= max(SKP_build$latitude)), # rest of the ocean cells
                         SKP_model2, # BRT model
                         `grid_SKP_jul-sept` %>% dplyr::filter(latitude >= min(SKP_build$latitude) & latitude <= max(SKP_build$latitude)) # grid of species for specific season with restricted ranges
)

skp <- skp[[1]] %>% 
  dplyr::arrange(cellID) %>% 
  tibble::as_tibble() %>% 
  dplyr::select(model) %>% 
  pull()

# ---- ALB ----
source("04a_ALB_Data.R") # Load ALB data
ALB_model2 <- readRDS("Output/Models/ALB_model2.rds") # load model

train_tmp <- train %>% 
  dplyr::mutate(model = ALB_model2$fitted)
preds <- gbm::predict.gbm(ALB_model2, test, n.trees = ALB_model2$gbm.call$best.trees, type = "response") # predict to test

test_tmp <- test %>% 
  dplyr::mutate(model = preds)

alb <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                         test_tmp, # testing object with model column (predictions)
                         "jul-sept", # season
                         ALB_predict_season3 %>% dplyr::filter(latitude >= min(ALB_build$latitude) & latitude <= max(ALB_build$latitude)), # rest of the ocean cells
                         ALB_model2, # BRT model
                         `grid_ALB_jul-sept` %>% dplyr::filter(latitude >= min(ALB_build$latitude) & latitude <= max(ALB_build$latitude)) # grid of species for specific season with restricted ranges
)

alb <- alb[[1]] %>% 
  dplyr::arrange(cellID) %>% 
  tibble::as_tibble() %>% 
  dplyr::select(model) %>% 
  pull()

# ---- SWO ----
source("05a_SWO_Data.R") # Load SWO data
SWO_model2 <- readRDS("Output/Models/SWO_model2.rds") # load model

train_tmp <- train %>% 
  dplyr::mutate(model = SWO_model2$fitted)
preds <- gbm::predict.gbm(SWO_model2, test, n.trees = SWO_model2$gbm.call$best.trees, type = "response") # predict to test

test_tmp <- test %>% 
  dplyr::mutate(model = preds)

swo <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                         test_tmp, # testing object with model column (predictions)
                         "jul-sept", # season
                         SWO_predict_season3 %>% dplyr::filter(latitude >= min(SWO_build$latitude) & latitude <= max(SWO_build$latitude)), # rest of the ocean cells
                         SWO_model2, # BRT model
                         `grid_SWO_jul-sept` %>% dplyr::filter(latitude >= min(SWO_build$latitude) & latitude <= max(SWO_build$latitude)) # grid of species for specific season with restricted ranges
)

swo <- swo[[1]] %>% 
  dplyr::arrange(cellID) %>% 
  tibble::as_tibble() %>% 
  dplyr::select(model) %>% 
  pull()

# ---- BLUM ----
source("06a_BLUM_Data.R") # Load BLUM data
BLUM_model2 <- readRDS("Output/Models/BLUM_model2.rds") # load model

train_tmp <- train %>% 
  dplyr::mutate(model = BLUM_model2$fitted)
preds <- gbm::predict.gbm(BLUM_model2, test, n.trees = BLUM_model2$gbm.call$best.trees, type = "response") # predict to test

test_tmp <- test %>% 
  dplyr::mutate(model = preds)

blum <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                          test_tmp, # testing object with model column (predictions)
                          "jul-sept", # season
                          BLUM_predict_season3 %>% dplyr::filter(latitude >= min(BLUM_build$latitude) & latitude <= max(BLUM_build$latitude)), # rest of the ocean cells
                          BLUM_model2, # BRT model
                          `grid_BLUM_jul-sept` %>% dplyr::filter(latitude >= min(BLUM_build$latitude) & latitude <= max(BLUM_build$latitude)) # grid of species for specific season with restricted ranges
)

blum <- blum[[1]] %>% 
  dplyr::arrange(cellID) %>% 
  tibble::as_tibble() %>% 
  dplyr::select(model) %>% 
  pull()

# ---- SHOS ----
source("07a_SHOS_Data.R") # Load SHOS data
SHOS_model2 <- readRDS("Output/Models/SHOS_model2.rds") # load model

train_tmp <- train %>% 
  dplyr::mutate(model = SHOS_model2$fitted)
preds <- gbm::predict.gbm(SHOS_model2, test, n.trees = SHOS_model2$gbm.call$best.trees, type = "response") # predict to test

test_tmp <- test %>% 
  dplyr::mutate(model = preds)

shos <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                          test_tmp, # testing object with model column (predictions)
                          "jul-sept", # season
                          SHOS_predict_season3 %>% dplyr::filter(latitude >= min(SHOS_build$latitude) & latitude <= max(SHOS_build$latitude)), # rest of the ocean cells
                          SHOS_model2, # BRT model
                          `grid_SHOS_jul-sept` %>% dplyr::filter(latitude >= min(SHOS_build$latitude) & latitude <= max(SHOS_build$latitude)) # grid of species for specific season with restricted ranges
)

shos <- shos[[1]] %>% 
  dplyr::arrange(cellID) %>% 
  tibble::as_tibble() %>% 
  dplyr::select(model) %>% 
  pull()

# ---- FRI ----
source("08a_FRI_Data.R") # Load FRI data
FRI_model2 <- readRDS("Output/Models/FRI_model2.rds") # load model

train_tmp <- train %>% 
  dplyr::mutate(model = FRI_model2$fitted)
preds <- gbm::predict.gbm(FRI_model2, test, n.trees = FRI_model2$gbm.call$best.trees, type = "response") # predict to test

test_tmp <- test %>% 
  dplyr::mutate(model = preds)

fri <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                         test_tmp, # testing object with model column (predictions)
                         "jul-sept", # season
                         FRI_predict_season3 %>% dplyr::filter(latitude >= min(FRI_build$latitude) & latitude <= max(FRI_build$latitude)), # rest of the ocean cells
                         FRI_model2, # BRT model
                         `grid_FRI_jul-sept` %>% dplyr::filter(latitude >= min(FRI_build$latitude) & latitude <= max(FRI_build$latitude)) # grid of species for specific season with restricted ranges
)

fri <- fri[[1]] %>% 
  dplyr::arrange(cellID) %>% 
  tibble::as_tibble() %>% 
  dplyr::select(model) %>% 
  pull()

# ---- BET ----
source("09a_BET_Data.R") # Load BET data
BET_model2 <- readRDS("Output/Models/BET_model2.rds") # load model

train_tmp <- train %>% 
  dplyr::mutate(model = BET_model2$fitted)
preds <- gbm::predict.gbm(BET_model2, test, n.trees = BET_model2$gbm.call$best.trees, type = "response") # predict to test

test_tmp <- test %>% 
  dplyr::mutate(model = preds)

bet <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                         test_tmp, # testing object with model column (predictions)
                         "jul-sept", # season
                         BET_predict_season3 %>% dplyr::filter(latitude >= min(BET_build$latitude) & latitude <= max(BET_build$latitude)), # rest of the ocean cells
                         BET_model2, # BRT model
                         `grid_BET_jul-sept` %>% dplyr::filter(latitude >= min(BET_build$latitude) & latitude <= max(BET_build$latitude)) # grid of species for specific season with restricted ranges
)

bet <- bet[[1]] %>% 
  dplyr::arrange(cellID) %>% 
  tibble::as_tibble() %>% 
  dplyr::select(model) %>% 
  pull()

# ---- STRM ----
source("10a_STRM_Data.R") # Load STRM data
STRM_model3 <- readRDS("Output/Models/STRM_model3.rds") # load model

train_tmp <- train %>% 
  dplyr::mutate(model = STRM_model3$fitted)
preds <- gbm::predict.gbm(STRM_model3, test, n.trees = STRM_model3$gbm.call$best.trees, type = "response") # predict to test

test_tmp <- test %>% 
  dplyr::mutate(model = preds)

strm <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                          test_tmp, # testing object with model column (predictions)
                          "jul-sept", # season
                          STRM_predict_season3 %>% dplyr::filter(latitude >= min(STRM_build$latitude) & latitude <= max(STRM_build$latitude)), # rest of the ocean cells
                          STRM_model3, # BRT model
                          `grid_STRM_jul-sept` %>% dplyr::filter(latitude >= min(STRM_build$latitude) & latitude <= max(STRM_build$latitude)) # grid of species for specific season with restricted ranges
)

strm <- strm[[1]] %>% 
  dplyr::arrange(cellID) %>% 
  tibble::as_tibble() %>% 
  dplyr::select(model) %>% 
  pull()

# ---- SAU ----
source("11a_SAU_Data.R") # Load SAU data
SAU_model2 <- readRDS("Output/Models/SAU_model2.rds") # load model

train_tmp <- train %>% 
  dplyr::mutate(model = SAU_model2$fitted)
preds <- gbm::predict.gbm(SAU_model2, test, n.trees = SAU_model2$gbm.call$best.trees, type = "response") # predict to test

test_tmp <- test %>% 
  dplyr::mutate(model = preds)

sau <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                         test_tmp, # testing object with model column (predictions)
                         "jul-sept", # season
                         SAU_predict_season3 %>% dplyr::filter(latitude >= min(SAU_build$latitude) & latitude <= max(SAU_build$latitude)), # rest of the ocean cells
                         SAU_model2, # BRT model
                         `grid_SAU_jul-sept` %>% dplyr::filter(latitude >= min(SAU_build$latitude) & latitude <= max(SAU_build$latitude)) # grid of species for specific season with restricted ranges
)

sau <- sau[[1]] %>% 
  dplyr::arrange(cellID) %>% 
  tibble::as_tibble() %>% 
  dplyr::select(model) %>% 
  pull()

# ---- SAIL ----
source("12a_SAIL_Data.R") # Load SAIL data
SAIL_model2 <- readRDS("Output/Models/SAIL_model2.rds") # load model

train_tmp <- train %>% 
  dplyr::mutate(model = SAIL_model2$fitted)
preds <- gbm::predict.gbm(SAIL_model2, test, n.trees = SAIL_model2$gbm.call$best.trees, type = "response") # predict to test

test_tmp <- test %>% 
  dplyr::mutate(model = preds)

sail <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                          test_tmp, # testing object with model column (predictions)
                          "jul-sept", # season
                          SAIL_predict_season3 %>% dplyr::filter(latitude >= min(SAIL_build$latitude) & latitude <= max(SAIL_build$latitude)), # rest of the ocean cells
                          SAIL_model2, # BRT model
                          `grid_SAIL_jul-sept` %>% dplyr::filter(latitude >= min(SAIL_build$latitude) & latitude <= max(SAIL_build$latitude)) # grid of species for specific season with restricted ranges
)

sail <- sail[[1]] %>% 
  dplyr::arrange(cellID) %>% 
  tibble::as_tibble() %>% 
  dplyr::select(model) %>% 
  pull()

# ---- LESC ----
source("13a_LESC_Data.R") # Load LESC data
LESC_model3 <- readRDS("Output/Models/LESC_model3.rds") # load model

train_tmp <- train %>% 
  dplyr::mutate(model = LESC_model3$fitted)
preds <- gbm::predict.gbm(LESC_model3, test, n.trees = LESC_model3$gbm.call$best.trees, type = "response") # predict to test

test_tmp <- test %>% 
  dplyr::mutate(model = preds)

lesc <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                          test_tmp, # testing object with model column (predictions)
                          "jul-sept", # season
                          LESC_predict_season3 %>% dplyr::filter(latitude >= min(LESC_build$latitude) & latitude <= max(LESC_build$latitude)), # rest of the ocean cells
                          LESC_model3, # BRT model
                          `grid_LESC_jul-sept` %>% dplyr::filter(latitude >= min(LESC_build$latitude) & latitude <= max(LESC_build$latitude)) # grid of species for specific season with restricted ranges
)

lesc <- lesc[[1]] %>% 
  dplyr::arrange(cellID) %>% 
  tibble::as_tibble() %>% 
  dplyr::select(model) %>% 
  pull()

#### Assembling data frame ####

df <- list(yft = yft, 
           skp = skp, 
           alb = alb, 
           swo = swo, 
           blum = blum, 
           shos = shos, 
           fri = fri, 
           bet = bet, 
           strm = strm, 
           sau = sau, 
           sail = sail, 
           lesc = lesc) %>% 
  do.call(bind_cols, .)

write.csv(df, file = "Output/CSV/FULL_predictions_jul-sept.csv")

#### Run PCA ####
PCA <- princomp(df, cor = FALSE)

summary(PCA)

PC_scores <- PCA$scores[,1:2] %>% 
  tibble::as_tibble()
write.csv(PC_scores, file = "Output/PCA/hotspots_jul-sept_scores.csv")
write.csv(PCA$loadings, file = "Output/PCA/hotspots_jul-sept_loadings.csv")

# Creating dummy sf for PCA
dummy <- plotSeasonPredict(train_tmp,
                           test_tmp,
                           "jul-sept", # season
                           YFT_predict_season3 %>% dplyr::filter(latitude >= min(YFT_build$latitude) & latitude <= max(YFT_build$latitude)),
                           YFT_model6, # BRT model
                           `grid_YFT_jul-sept` %>% dplyr::filter(latitude >= min(YFT_build$latitude) & latitude <= max(YFT_build$latitude))
)

dummy <- dummy[[1]] %>% 
  dplyr::select(-ocean, -model) %>% 
  cbind(., PC_scores)

# Plot PC1
pc1 <- ggplot() + 
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

# Plot PC2
pc2 <- ggplot() + 
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

pc_plot <- (pc1 + pc2) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = pc_plot, filename = "Figures/PC_plot_jul-sept.png", width = 27, height = 7.5, dpi = 300)

#### Pearson's correlation ####

file_path_test = "Figures/CorrMatrix_PC1_jul-sept.png"
png(height=1200, width=1200, res = 200, file=file_path_test, type = "cairo")

mat <- dplyr::bind_cols(PC_scores$Comp.1, df) %>% 
  dplyr::rename(Comp = `...1`) %>% 
  dplyr::select(Comp, yft, skp, alb, fri, bet, swo, blum, shos, strm, sail, sau, lesc) %>% # reorder the columns
  as.matrix()

res <- rcorr(mat)

corrplot(res$r, 
         type = "upper", 
         order = "original", 
         tl.col = "white", 
         # addCoef.col = "black", 
         tl.srt = 45, 
         insig = "blank", 
         col = COL2('BrBG', 200))

dev.off()

file_path_test = "Figures/CorrMatrix_PC2_jul-sept.png"
png(height=1200, width=1200, res = 200, file=file_path_test, type = "cairo")

mat <- dplyr::bind_cols(PC_scores$Comp.2, df) %>% 
  dplyr::rename(Comp = `...1`) %>% 
  dplyr::select(Comp, yft, skp, alb, fri, bet, swo, blum, shos, strm, sail, sau, lesc) %>% # reorder the columns
  as.matrix()

res <- rcorr(mat)

corrplot(res$r, type = "lower", order = "original", 
         tl.col = "white", 
         #addCoef.col = "black", 
         tl.srt = 45, 
         insig = "blank", 
         col = COL2('BrBG', 200))

dev.off()

rm(list=ls())  # free up environment
