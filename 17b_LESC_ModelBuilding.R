# DESCRIPTION: Building optimal model for longfin escolar

# Load preliminaries
source("17a_LESC_Data.R") # Load LESC data

#### Model 1: With longitude and latitude ####
# 5-fold grid search
CVGrid <- CVgridSearch(train, test, tc = c(1, 2), bf = c(0.5, 0.75), lr = seq(0.005, 0.01, 0.001), pred_in = c(7:23, 25), resp_in = 5)

print(CVGrid %>% dplyr::arrange(desc(test_AUC)), n = 1) # BEST TEST AUC

# Building most optimal model
LESC_model1 <- dismo::gbm.step(data = train, gbm.x = c(7:23, 25),
                               gbm.y = 5, family = "bernoulli", n.folds = 5,
                               tree.complexity = 1, bag.fraction = 0.75, learning.rate = 0.008
)
saveRDS(LESC_model1, here::here(model_dir, paste(species, "model1.rds", sep = "_")))
# LESC_model1 <- readRDS(here::here(model_dir, paste(species, "model1.rds", sep = "_")))

# Show the relative importance of each of the predictors
summary(LESC_model1)

# Printing AUCs
LESC_model1$self.statistics$discrimination # Training AUC Score
LESC_model1$cv.statistics$discrimination.mean # Validation AUC Score

# Predict to the testing dataset
preds <- gbm::predict.gbm(LESC_model1, test, n.trees = LESC_model1$gbm.call$best.trees, type = "response")
dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
get_testAUC(test$abundance_presence, preds) # Print testing AUC

# Plot maps
train_tmp <- train %>% 
  dplyr::mutate(model = LESC_model1$fitted)
test_tmp <- test %>% 
  dplyr::mutate(model = preds)
limits = c(0, 0.9)

# January-March
gg <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jan-mar", # season
                        LESC_predict_season1, # rest of the ocean cells
                        LESC_model1, # BRT model
                        `grid_LESC_jan-mar` # grid of species for specific season
)

gg1 <- plotModel(gg, limits)
hatch1 <- plotHatch(gg1, gg, LESC_ds1)

# April-June
gg <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "apr-jun", # season
                        LESC_predict_season2, # rest of the ocean cells
                        LESC_model1, # BRT model
                        `grid_LESC_apr-jun` # grid of species for specific season
)

gg2 <- plotModel(gg, limits)
hatch2 <- plotHatch(gg2, gg, LESC_ds2)

# July-September
gg <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jul-sept", # season
                        LESC_predict_season3, # rest of the ocean cells
                        LESC_model1, # BRT model
                        `grid_LESC_jul-sept` # grid of species for specific season
)

gg3 <- plotModel(gg, limits)
hatch3 <- plotHatch(gg3, gg, LESC_ds3)

# October-December
gg <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "oct-dec", # season
                        LESC_predict_season4, # rest of the ocean cells
                        LESC_model1, # BRT model
                        `grid_LESC_oct-dec` # grid of species for specific season
)

gg4 <- plotModel(gg, limits)
hatch4 <- plotHatch(gg4, gg, LESC_ds4)

ggsquished <- (gg1 + gg2) / (gg3 + gg4) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = ggsquished, filename = here::here(figure_dir, paste(species, "model1", "base.png", sep = "_")), width = 27, height = 15, dpi = 600)

gghatch <- (hatch1 + hatch2) / (hatch3 + hatch4) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = gghatch, filename = here::here(figure_dir, paste(species, "model1", "hatched.png", sep = "_")), width = 27, height = 15, dpi = 600)

#### Model 2: Without longitude and latitude ####
# 5-fold grid search
CVGrid <- CVgridSearch(train, test, tc = c(1, 2), bf = c(0.5, 0.75), lr = seq(0.005, 0.01, 0.001), pred_in = c(9:23, 25), resp_in = 5)

print(CVGrid %>% dplyr::arrange(desc(test_AUC)), n = 1) # BEST TEST AUC

# Building most optimal model
LESC_model2 <- dismo::gbm.step(data = train, gbm.x = c(9:23, 25),
                               gbm.y = 5, family = "bernoulli", n.folds = 5,
                               tree.complexity = 2, bag.fraction = 0.5, learning.rate = 0.01
)
saveRDS(LESC_model2, here::here(model_dir, paste(species, "model2.rds", sep = "_")))
# LESC_model2 <- readRDS(here::here(model_dir, paste(species, "model2.rds", sep = "_)))

# Show the relative importance of each of the predictors
summary(LESC_model2)

# Printing AUCs
LESC_model2$self.statistics$discrimination # Training AUC Score
LESC_model2$cv.statistics$discrimination.mean # Validation AUC Score

# Predict to the testing dataset
preds <- gbm::predict.gbm(LESC_model2, test, n.trees = LESC_model2$gbm.call$best.trees, type = "response")
dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
get_testAUC(test$abundance_presence, preds) # Print testing AUC

# Plot maps
train_tmp <- train %>% 
  dplyr::mutate(model = LESC_model2$fitted)
test_tmp <- test %>% 
  dplyr::mutate(model = preds)
limits = c(0, 0.9)

# January-March
gg <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jan-mar", # season
                        LESC_predict_season1, # rest of the ocean cells
                        LESC_model2, # BRT model
                        `grid_LESC_jan-mar` # grid of species for specific season
)

gg1 <- plotModel(gg, limits)
hatch1 <- plotHatch(gg1, gg, LESC_ds1)

# April-June
gg <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "apr-jun", # season
                        LESC_predict_season2, # rest of the ocean cells
                        LESC_model2, # BRT model
                        `grid_LESC_apr-jun` # grid of species for specific season
)

gg2 <- plotModel(gg, limits)
hatch2 <- plotHatch(gg2, gg, LESC_ds2)

# July-September
gg <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jul-sept", # season
                        LESC_predict_season3, # rest of the ocean cells
                        LESC_model2, # BRT model
                        `grid_LESC_jul-sept` # grid of species for specific season
)

gg3 <- plotModel(gg, limits)
hatch3 <- plotHatch(gg3, gg, LESC_ds3)

# October-December
gg <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "oct-dec", # season
                        LESC_predict_season4, # rest of the ocean cells
                        LESC_model2, # BRT model
                        `grid_LESC_oct-dec` # grid of species for specific season
)

gg4 <- plotModel(gg, limits)
hatch4 <- plotHatch(gg4, gg, LESC_ds4)

ggsquished <- (gg1 + gg2) / (gg3 + gg4) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = ggsquished, filename = here::here(figure_dir, paste(species, "model2", "base.png", sep = "_")), width = 27, height = 15, dpi = 600)

gghatch <- (hatch1 + hatch2) / (hatch3 + hatch4) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = gghatch, filename = here::here(figure_dir, paste(species, "model2", "hatched.png", sep = "_")), width = 27, height = 15, dpi = 600)

