# DESCRIPTION: Building optimal model for striped marlin

# Load preliminaries
source("09a_BFT_Data.R")

#### Model 1: With longitude and latitude ####
# Grid search
CVGrid <- CVgridSearch(train, test, tc = c(1, 2), bf = c(0.5, 0.75), lr = seq(0.005, 0.01, 0.001), pred_in = c(7:23, 25), resp_in = 5)

print(CVGrid %>% dplyr::arrange(desc(test_AUC)), n = 1) # BEST TEST AUC

# Building most optimal model
BFT_model1 <- dismo::gbm.step(data = train, gbm.x = c(7:23, 25),
                              gbm.y = 5, family = "bernoulli", n.folds = 5,
                              tree.complexity = 2, bag.fraction = 0.75, learning.rate = 0.008
)
saveRDS(BFT_model1, here::here(model_dir, paste(species, "model1.rds", sep = "_")))
# BFT_model1 <- readRDS(here::here(model_dir, paste(species, "model1.rds", sep = "_")))

# Show the relative importance of each of the predictors
summary(BFT_model1) 

# Printing AUCs
BFT_model1$self.statistics$discrimination # Training AUC Score
BFT_model1$cv.statistics$discrimination.mean # Validation AUC Score

# Predict to the testing dataset
preds <- gbm::predict.gbm(BFT_model1, test, n.trees = BFT_model1$gbm.call$best.trees, type = "response")
dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
get_testAUC(test$abundance_presence, preds) # Print testing AUC

# Plot maps
train_tmp <- train %>% 
  dplyr::mutate(model = BFT_model1$fitted)
test_tmp <- test %>% 
  dplyr::mutate(model = preds)
limits = c(0, 0.95)

# January-March
gg <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jan-mar", # season
                        BFT_predict_season1, # rest of the ocean cells
                        BFT_model1, # BRT model
                        `grid_BFT_jan-mar` # grid of species for specific season
)

gg1 <- plotModel(gg, limits)
hatch1 <- plotHatch(gg1, gg, BFT_ds1)

# April-June
gg <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "apr-jun", # season
                        BFT_predict_season2, # rest of the ocean cells
                        BFT_model1, # BRT model
                        `grid_BFT_apr-jun` # grid of species for specific season
)

gg2 <- plotModel(gg, limits)
hatch2 <- plotHatch(gg2, gg, BFT_ds2)

# July-September
gg <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jul-sept", # season
                        BFT_predict_season3, # rest of the ocean cells
                        BFT_model1, # BRT model
                        `grid_BFT_jul-sept` # grid of species for specific season
)

gg3 <- plotModel(gg, limits)
hatch3 <- plotHatch(gg3, gg, BFT_ds3)

# October-December
gg <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "oct-dec", # season
                        BFT_predict_season4, # rest of the ocean cells
                        BFT_model1, # BRT model
                        `grid_BFT_oct-dec` # grid of species for specific season
)

gg4 <- plotModel(gg, limits)
hatch4 <- plotHatch(gg4, gg, BFT_ds4)

ggsquished <- (gg1 + gg2) / (gg3 + gg4) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = ggsquished, filename = here::here(figure_dir, paste(species, "model1", "base.png", sep = "_")), width = 27, height = 15, dpi = 600)

gghatch <- (hatch1 + hatch2) / (hatch3 + hatch4) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = gghatch, filename = here::here(figure_dir, paste(species, "model1", "hatched.png")), width = 27, height = 15, dpi = 600)

#### Model 2: Without longitude and latitude ####

# 5-fold grid search
CVGrid <- CVgridSearch(train, test, tc = c(1, 2), bf = c(0.5, 0.75), lr = seq(0.005, 0.01, 0.001), pred_in = c(9:23, 25), resp_in = 5) # with longitude and latitude

print(CVGrid %>% dplyr::arrange(desc(test_AUC)), n = 1) # BEST TEST AUC

# Building most optimal model
BFT_model2 <- dismo::gbm.step(data = train, gbm.x = c(9:23, 25),
                              gbm.y = 5, family = "bernoulli", n.folds = 5,
                              tree.complexity = 1, bag.fraction = 0.75, learning.rate = 0.009
)
saveRDS(BFT_model2, here::here(output_dir, paste(species, "model2.rds", sep = "_")))
# BFT_model2 <- readRDS(here::here(output_dir, paste(species, "model2.rds", sep = "_")))

# Show the relative importance of each of the predictors
summary(BFT_model2)

# Printing AUCs
BFT_model2$self.statistics$discrimination # Training AUC Score
BFT_model2$cv.statistics$discrimination.mean # Validation AUC Score

# Predict to the testing dataset
preds <- gbm::predict.gbm(BFT_model2, test, n.trees = BFT_model2$gbm.call$best.trees, type = "response")
dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
get_testAUC(test$abundance_presence, preds) # Print testing AUC

# Plot maps
train_tmp <- train %>% 
  dplyr::mutate(model = BFT_model2$fitted)
test_tmp <- test %>% 
  dplyr::mutate(model = preds)
limits = c(0, 0.95)

# January-March
gg <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jan-mar", # season
                        BFT_predict_season1, # rest of the ocean cells
                        BFT_model2, # BRT model
                        `grid_BFT_jan-mar` # grid of species for specific season
)

gg1 <- plotModel(gg, limits)
hatch1 <- plotHatch(gg1, gg, BFT_ds1)

# April-June
gg <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "apr-jun", # season
                        BFT_predict_season2, # rest of the ocean cells
                        BFT_model2, # BRT model
                        `grid_BFT_apr-jun` # grid of species for specific season
)

gg2 <- plotModel(gg, limits)
hatch2 <- plotHatch(gg2, gg, BFT_ds2)

# July-September
gg <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jul-sept", # season
                        BFT_predict_season3, # rest of the ocean cells
                        BFT_model2, # BRT model
                        `grid_BFT_jul-sept` # grid of species for specific season
)

gg3 <- plotModel(gg, limits)
hatch3 <- plotHatch(gg3, gg, BFT_ds3)

# October-December
gg <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "oct-dec", # season
                        BFT_predict_season4, # rest of the ocean cells
                        BFT_model2, # BRT model
                        `grid_BFT_oct-dec` # grid of species for specific season
)

gg4 <- plotModel(gg, limits)
hatch4 <- plotHatch(gg4, gg, BFT_ds4)

ggsquished <- (gg1 + gg2) / (gg3 + gg4) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = ggsquished, filename = here::here(figure_dir, paste(species, "model2", "base.png", sep = "_")), width = 27, height = 15, dpi = 600)

gghatch <- (hatch1 + hatch2) / (hatch3 + hatch4) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = gghatch, filename = here::here(figure_dir, paste(species, "model2", "hatched.png", sep = "_")), width = 27, height = 15, dpi = 600)