# DESCRIPTION: Building optimal model for blue marlin

# Load preliminaries
source("06a_BLUM_Data.R") # Load BLUM data
figure_dir <- here::here(figure_dir, species)

#### Model 1: With longitude and latitude ####
# 5-fold grid search
CVGrid <- CVgridSearch(train, test, tc = c(1, 2), bf = c(0.5, 0.75), lr = seq(0.005, 0.01, 0.001), pred_in = c(7:24), resp_in = 5)

print(CVGrid %>% dplyr::arrange(desc(test_AUC)), n = 1) # BEST TEST AUC

# Building most optimal model
BLUM_model1 <- dismo::gbm.step(data = train, gbm.x = c(7:24),
                              gbm.y = 5, family = "bernoulli", n.folds = 5,
                              tree.complexity = 2, bag.fraction = 0.75, learning.rate = 0.009
)
saveRDS(BLUM_model1, here::here(model_dir, paste(species, "model1.rds", sep = "_")))
# BLUM_model1 <- readRDS(here::here(model_dir, paste(species, "model1.rds", sep = "_")))

# Show the relative importance of each of the predictors
summary(BLUM_model1)

# Printing AUCs
BLUM_model1$self.statistics$discrimination # Training AUC Score
BLUM_model1$cv.statistics$discrimination.mean # Validation AUC Score

# Predict to the testing dataset
preds <- gbm::predict.gbm(BLUM_model1, test, n.trees = BLUM_model1$gbm.call$best.trees, type = "response")
dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
get_testAUC(test$abundance_presence, preds) # Print testing AUC

# Plot maps
train_tmp <- train %>% 
  dplyr::mutate(model = BLUM_model1$fitted)
test_tmp <- test %>% 
  dplyr::mutate(model = preds)
limits = c(0, 0.9)

# January-March
gg <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jan-mar", # season
                        BLUM_predict_season1, # rest of the ocean cells
                        BLUM_model1, # BRT model
                        `grid_BLUM_jan-mar` # grid of species for specific season
)

gg1 <- plotModel(gg, limits)
# ggsave(plot = gg1, filename = here::here(figure_dir, "BLUM_model1_base_jan-mar.png"), width = 14, height = 5, dpi = 600)
nish1 <- plotNish(`grid_BLUM_jan-mar`)
#ggsave(plot = nish1, filename = here::here(figure_dir, "BLUM_nishikawa_jan-mar.png"), width = 14, height = 5, dpi = 600)

# April-June
gg <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "apr-jun", # season
                        BLUM_predict_season2, # rest of the ocean cells
                        BLUM_model1, # BRT model
                        `grid_BLUM_apr-jun` # grid of species for specific season
)

gg2 <- plotModel(gg, limits)
#ggsave(plot = gg2, filename = here::here(figure_dir, "BLUM_model1_base_apr-jun.png"), width = 14, height = 5, dpi = 600)
nish2 <- plotNish(`grid_BLUM_apr-jun`)
#ggsave(plot = nish2, filename = here::here(figure_dir, "BLUM_nishikawa_apr-jun.png"), width = 14, height = 5, dpi = 600)

# July-September
gg <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jul-sept", # season
                        BLUM_predict_season3, # rest of the ocean cells
                        BLUM_model1, # BRT model
                        `grid_BLUM_jul-sept` # grid of species for specific season
)

gg3 <- plotModel(gg, limits)
#ggsave(plot = gg3, filename = here::here(figure_dir, "BLUM_model1_base_jul-sept.png"), width = 14, height = 5, dpi = 600)
nish3 <- plotNish(`grid_BLUM_jul-sept`)
#ggsave(plot = nish3, filename = here::here(figure_dir, "BLUM_nishikawa_jul-sept.png"), width = 14, height = 5, dpi = 600)

# October-December
gg <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "oct-dec", # season
                        BLUM_predict_season4, # rest of the ocean cells
                        BLUM_model1, # BRT model
                        `grid_BLUM_oct-dec` # grid of species for specific season
)

gg4 <- plotModel(gg, limits)
#ggsave(plot = gg4, filename = here::here(figure_dir, "BLUM_model1_base_oct-dec.png"), width = 14, height = 5, dpi = 600)
nish4 <- plotNish(`grid_BLUM_oct-dec`)
#ggsave(plot = nish4, filename = here::here(figure_dir, "BLUM_nishikawa_oct-dec.png"), width = 14, height = 5, dpi = 600)

ggsquished <- (gg1 + gg2) / (gg3 + gg4) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = ggsquished, filename = here::here(figure_dir, paste(species, "model1", "base.png", sep = "_")), width = 27, height = 15, dpi = 600)

#### Model 2: Without longitude and latitude ####
# 5-fold grid search
CVGrid <- CVgridSearch(train, test, tc = c(1, 2), bf = c(0.5, 0.75), lr = seq(0.005, 0.01, 0.001), pred_in = c(9:24), resp_in = 5)

print(CVGrid %>% dplyr::arrange(desc(test_AUC)), n = 1) # BEST TEST AUC

# Building most optimal model
BLUM_model2 <- dismo::gbm.step(data = train, gbm.x = c(9:24),
                              gbm.y = 5, family = "bernoulli", n.folds = 5,
                              tree.complexity = 2, bag.fraction = 0.75, learning.rate = 0.008
)
saveRDS(BLUM_model2, here::here(model_dir, paste(species, "model2.rds")))
# BLUM_model2 <- readRDS(here::here(model_dir, paste(species, "model2.rds")))

# Show the relative importance of each of the predictors
summary(BLUM_model2)

# Printing AUCs
BLUM_model2$self.statistics$discrimination # Training AUC Score
BLUM_model2$cv.statistics$discrimination.mean # Validation AUC Score

# Predict to the testing dataset
preds <- gbm::predict.gbm(BLUM_model2, test, n.trees = BLUM_model2$gbm.call$best.trees, type = "response")
dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
get_testAUC(test$abundance_presence, preds) # Print testing AUC

# Plot maps
train_tmp <- train %>% 
  dplyr::mutate(model = BLUM_model2$fitted)
test_tmp <- test %>% 
  dplyr::mutate(model = preds)
limits = c(0, 0.9)

# January-March
gg <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jan-mar", # season
                        BLUM_predict_season1, # rest of the ocean cells
                        BLUM_model2, # BRT model
                        `grid_BLUM_jan-mar` # grid of species for specific season
)

gg1 <- plotModel(gg, limits)
hatch1 <- plotHatch(gg1, gg, BLUM_ds1)

# April-June
gg <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "apr-jun", # season
                        BLUM_predict_season2, # rest of the ocean cells
                        BLUM_model2, # BRT model
                        `grid_BLUM_apr-jun` # grid of species for specific season
)

gg2 <- plotModel(gg, limits)
hatch2 <- plotHatch(gg2, gg, BLUM_ds2)

# July-September
gg <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jul-sept", # season
                        BLUM_predict_season3, # rest of the ocean cells
                        BLUM_model2, # BRT model
                        `grid_BLUM_jul-sept` # grid of species for specific season
)

gg3 <- plotModel(gg, limits)
hatch3 <- plotHatch(gg3, gg, BLUM_ds3)

# October-December
gg <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "oct-dec", # season
                        BLUM_predict_season4, # rest of the ocean cells
                        BLUM_model2, # BRT model
                        `grid_BLUM_oct-dec` # grid of species for specific season
)

gg4 <- plotModel(gg, limits)
hatch4 <- plotHatch(gg4, gg, BLUM_ds4)

ggsquished <- (gg1 + gg2) / (gg3 + gg4) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = ggsquished, filename = here::here(figure_dir, paste(species, "model2", "base.png", sep = "_")), width = 27, height = 15, dpi = 600)

gghatch <- (hatch1 + hatch2) / (hatch3 + hatch4) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = gghatch, filename = here::here(figure_dir, paste(species, "model2", "hatched.png", sep = "_")), width = 27, height = 15, dpi = 600)