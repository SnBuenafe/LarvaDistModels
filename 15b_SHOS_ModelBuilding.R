# DESCRIPTION: Building optimal model for shortbill spearfish

# Load preliminaries
source("15a_SHOS_Data.R") # Load SHOS data
output_dir <- here::here("Output", "Models")
figure_dir <- here::here("Figures", "SHOS")

#### Model 1: With longitude and latitude ####

# 5-fold grid search
CVGrid <- CVgridSearch(train, test, tc = c(1, 2), bf = c(0.5, 0.75), lr = seq(0.005, 0.01, 0.001), pred_in = c(7:23, 25), resp_in = 5)

print(CVGrid %>% dplyr::arrange(desc(test_AUC)), n = 1) # BEST TEST AUC

# Building most optimal model
SHOS_model1 <- dismo::gbm.step(data = train, gbm.x = c(7:23, 25),
                              gbm.y = 5, family = "bernoulli", n.folds = 5,
                              tree.complexity = 2, bag.fraction = 0.75, learning.rate = 0.006
)
saveRDS(SHOS_model1, here::here(output_dir, "SHOS_model1.rds"))
# SHOS_model1 <- readRDS(here::here(output_dir, "SHOS_model1.rds"))

# Show the relative importance of each of the predictors
summary(SHOS_model1)

# Printing AUCs
SHOS_model1$self.statistics$discrimination # Training AUC Score
SHOS_model1$cv.statistics$discrimination.mean # Validation AUC Score

# Predict to the testing dataset
preds <- gbm::predict.gbm(SHOS_model1, test, n.trees = SHOS_model1$gbm.call$best.trees, type = "response")
dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
get_testAUC(test$abundance_presence, preds) # Print testing AUC

# Plot maps
train_tmp <- train %>% 
  dplyr::mutate(model = SHOS_model1$fitted)
test_tmp <- test %>% 
  dplyr::mutate(model = preds)
limits = c(0, 0.8)

# January-March
gg <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jan-mar", # season
                        SHOS_predict_season1, # rest of the ocean cells
                        SHOS_model1, # BRT model
                        `grid_SHOS_jan-mar` # grid of species for specific season
)

gg1 <- plotModel(gg, limits)
hatch1 <- plotHatch(gg1, gg, SHOS_ds1)

# April-June
gg <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "apr-jun", # season
                        SHOS_predict_season2, # rest of the ocean cells
                        SHOS_model1, # BRT model
                        `grid_SHOS_apr-jun` # grid of species for specific season
)

gg2 <- plotModel(gg, limits)
hatch2 <- plotHatch(gg2, gg, SHOS_ds2)

# July-September
gg <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jul-sept", # season
                        SHOS_predict_season3, # rest of the ocean cells
                        SHOS_model1, # BRT model
                        `grid_SHOS_jul-sept` # grid of species for specific season
)

gg3 <- plotModel(gg, limits)
hatch3 <- plotHatch(gg3, gg, SHOS_ds3)

# October-December
gg <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "oct-dec", # season
                        SHOS_predict_season4, # rest of the ocean cells
                        SHOS_model1, # BRT model
                        `grid_SHOS_oct-dec` # grid of species for specific season
)

gg4 <- plotModel(gg, limits)
hatch4 <- plotHatch(gg4, gg, SHOS_ds4)

ggsquished <- (gg1 + gg2) / (gg3 + gg4) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = ggsquished, filename = here::here(figure_dir, "SHOS_model1_base.png"), width = 27, height = 15, dpi = 600)

gghatch <- (hatch1 + hatch2) / (hatch3 + hatch4) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = gghatch, filename = here::here(figure_dir, "SHOS_model1_hatched.png"), width = 27, height = 15, dpi = 600)

#### Model 2: Without longitude and latitude ####

# 5-fold grid search
CVGrid <- CVgridSearch(train, test, tc = c(1, 2), bf = c(0.5, 0.75), lr = seq(0.005, 0.01, 0.001), pred_in = c(9:23, 25), resp_in = 5)

print(CVGrid %>% dplyr::arrange(desc(test_AUC)), n = 1) # BEST TEST AUC

# Building most optimal model
SHOS_model2 <- dismo::gbm.step(data = train, gbm.x = c(9:23, 25),
                              gbm.y = 5, family = "bernoulli", n.folds = 5,
                              tree.complexity = 2, bag.fraction = 0.75, learning.rate = 0.008
)
saveRDS(SHOS_model2, here::here(output_dir, "SHOS_model2.rds"))
# SHOS_model2 <- readRDS(here::here(output_dir, "SHOS_model2.rds"))

# Show the relative importance of each of the predictors
summary(SHOS_model2)

# Printing AUCs
SHOS_model2$self.statistics$discrimination # Training AUC Score
SHOS_model2$cv.statistics$discrimination.mean # Validation AUC Score

# Predict to the testing dataset
preds <- gbm::predict.gbm(SHOS_model2, test, n.trees = SHOS_model2$gbm.call$best.trees, type = "response")
dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
get_testAUC(test$abundance_presence, preds) # Print testing AUC

# Plot maps
train_tmp <- train %>% 
  dplyr::mutate(model = SHOS_model2$fitted)
test_tmp <- test %>% 
  dplyr::mutate(model = preds)
limits = c(0, 0.8)

# January-March
gg <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jan-mar", # season
                        SHOS_predict_season1, # rest of the ocean cells
                        SHOS_model2, # BRT model
                        `grid_SHOS_jan-mar` # grid of species for specific season
)

gg1 <- plotModel(gg, limits)
hatch1 <- plotHatch(gg1, gg, SHOS_ds1)

# April-June
gg <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "apr-jun", # season
                        SHOS_predict_season2, # rest of the ocean cells
                        SHOS_model2, # BRT model
                        `grid_SHOS_apr-jun` # grid of species for specific season
)

gg2 <- plotModel(gg, limits)
hatch2 <- plotHatch(gg2, gg, SHOS_ds2)

# July-September
gg <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jul-sept", # season
                        SHOS_predict_season3, # rest of the ocean cells
                        SHOS_model2, # BRT model
                        `grid_SHOS_jul-sept` # grid of species for specific season
)

gg3 <- plotModel(gg, limits)
hatch3 <- plotHatch(gg3, gg, SHOS_ds3)

# October-December
gg <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "oct-dec", # season
                        SHOS_predict_season4, # rest of the ocean cells
                        SHOS_model2, # BRT model
                        `grid_SHOS_oct-dec` # grid of species for specific season
)

gg4 <- plotModel(gg, limits)
hatch4 <- plotHatch(gg4, gg, SHOS_ds4)

ggsquished <- (gg1 + gg2) / (gg3 + gg4) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = ggsquished, filename = here::here(figure_dir, "SHOS_model2_base.png"), width = 27, height = 15, dpi = 600)

gghatch <- (hatch1 + hatch2) / (hatch3 + hatch4) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = gghatch, filename = here::here(figure_dir, "SHOS_model2_hatched.png"), width = 27, height = 15, dpi = 600)

