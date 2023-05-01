# DESCRIPTION: Building optimal model for striped marlin

# Load preliminaries
source("16a_STRM_Data.R") # Load STRM data
output_dir <- here::here("Output", "Models")
figure_dir <- here::here("Figures", "STRM")

#### Model 1: With longitude and latitude ####

# 5-fold grid search
CVGrid <- CVgridSearch(train, test, tc = c(1, 2), bf = c(0.5, 0.75), lr = seq(0.005, 0.01, 0.001), pred_in = c(7:23, 26), resp_in = 5)

print(CVGrid %>% dplyr::arrange(desc(test_AUC)), n = 1) # BEST TEST AUC

# Building most optimal model
STRM_model1 <- dismo::gbm.step(data = train, gbm.x = c(7:23, 26),
                               gbm.y = 5, family = "bernoulli", n.folds = 5,
                               tree.complexity = 2, bag.fraction = 0.75, learning.rate = 0.008
)
saveRDS(STRM_model1, here::here(output_dir, "STRM_model1.rds"))
# STRM_model1 <- readRDS(here::here(output_dir, "STRM_model1.rds"))

# Show the relative importance of each of the predictors
summary(STRM_model1)

# Printing AUCs
STRM_model1$self.statistics$discrimination # Training AUC Score
STRM_model1$cv.statistics$discrimination.mean # Validation AUC Score

# Predict to the testing dataset
preds <- gbm::predict.gbm(STRM_model1, test, n.trees = STRM_model1$gbm.call$best.trees, type = "response")
dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
get_testAUC(test$abundance_presence, preds) # Print testing AUC

# Plot maps
train_tmp <- train %>% 
  dplyr::mutate(model = STRM_model1$fitted)
test_tmp <- test %>% 
  dplyr::mutate(model = preds)
limits = c(0, 0.85)

# January-March
gg <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jan-mar", # season
                        STRM_predict_season1, # rest of the ocean cells
                        STRM_model1, # BRT model
                        `grid_STRM_jan-mar` # grid of species for specific season
)

gg1 <- plotModel(gg, limits)
hatch1 <- plotHatch(gg1, gg, STRM_ds1)

# April-June
gg <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "apr-jun", # season
                        STRM_predict_season2, # rest of the ocean cells
                        STRM_model1, # BRT model
                        `grid_STRM_apr-jun` # grid of species for specific season
)

gg2 <- plotModel(gg, limits)
hatch2 <- plotHatch(gg2, gg, STRM_ds2)

# July-September
gg <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jul-sept", # season
                        STRM_predict_season3, # rest of the ocean cells
                        STRM_model1, # BRT model
                        `grid_STRM_jul-sept` # grid of species for specific season
)

gg3 <- plotModel(gg, limits)
hatch3 <- plotHatch(gg3, gg, STRM_ds3)

# October-December
gg <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "oct-dec", # season
                        STRM_predict_season4, # rest of the ocean cells
                        STRM_model1, # BRT model
                        `grid_STRM_oct-dec` # grid of species for specific season
)

gg4 <- plotModel(gg, limits)
hatch4 <- plotHatch(gg4, gg, STRM_ds4)

ggsquished <- (gg1 + gg2) / (gg3 + gg4) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = ggsquished, filename = here::here(figure_dir, "STRM_model1_base.png"), width = 27, height = 15, dpi = 600)

gghatch <- (hatch1 + hatch2) / (hatch3 + hatch4) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = gghatch, filename = here::here(figure_dir, "STRM_model1_hatched.png"), width = 27, height = 15, dpi = 600)

#### Model 2: Without longitude and latitude ####

# 5-fold grid search
CVGrid <- CVgridSearch(train, test, tc = c(1, 2), bf = c(0.5, 0.75), lr = seq(0.005, 0.01, 0.001), pred_in = c(9:23, 26), resp_in = 5)

print(CVGrid %>% dplyr::arrange(desc(test_AUC)), n = 1) # BEST TEST AUC

# Building most optimal model
STRM_model2 <- dismo::gbm.step(data = train, gbm.x = c(9:23, 26),
                               gbm.y = 5, family = "bernoulli", n.folds = 5,
                               tree.complexity = 2, bag.fraction = 0.75, learning.rate = 0.007
)
saveRDS(STRM_model2, here::here(output_dir, "STRM_model2.rds"))
# STRM_model2 <- readRDS(here::here(output_dir, "STRM_model2.rds"))

# Show the relative importance of each of the predictors
summary(STRM_model2)

# Printing AUCs
STRM_model2$self.statistics$discrimination # Training AUC Score
STRM_model2$cv.statistics$discrimination.mean # Validation AUC Score

# Predict to the testing dataset
preds <- gbm::predict.gbm(STRM_model2, test, n.trees = STRM_model2$gbm.call$best.trees, type = "response")
dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
get_testAUC(test$abundance_presence, preds) # Print testing AUC

# Plot maps
train_tmp <- train %>% 
  dplyr::mutate(model = STRM_model2$fitted)
test_tmp <- test %>% 
  dplyr::mutate(model = preds)
limits = c(0, 0.9)

# January-March
gg <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jan-mar", # season
                        STRM_predict_season1, # rest of the ocean cells
                        STRM_model2, # BRT model
                        `grid_STRM_jan-mar` # grid of species for specific season
)

gg1 <- plotModel(gg, limits)
hatch1 <- plotHatch(gg1, gg, STRM_ds1)

# April-June
gg <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "apr-jun", # season
                        STRM_predict_season2, # rest of the ocean cells
                        STRM_model2, # BRT model
                        `grid_STRM_apr-jun` # grid of species for specific season
)

gg2 <- plotModel(gg, limits)
hatch2 <- plotHatch(gg2, gg, STRM_ds2)

# July-September
gg <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jul-sept", # season
                        STRM_predict_season3, # rest of the ocean cells
                        STRM_model2, # BRT model
                        `grid_STRM_jul-sept` # grid of species for specific season
)

gg3 <- plotModel(gg, limits)
hatch3 <- plotHatch(gg3, gg, STRM_ds3)

# October-December
gg <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "oct-dec", # season
                        STRM_predict_season4, # rest of the ocean cells
                        STRM_model2, # BRT model
                        `grid_STRM_oct-dec` # grid of species for specific season
)

gg4 <- plotModel(gg, limits)
hatch4 <- plotHatch(gg4, gg, STRM_ds4)

ggsquished <- (gg1 + gg2) / (gg3 + gg4) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = ggsquished, filename = here::here(figure_dir, "STRM_model2_base.png"), width = 27, height = 15, dpi = 600)

gghatch <- (hatch1 + hatch2) / (hatch3 + hatch4) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = gghatch, filename = here::here(figure_dir, "STRM_model2_hatched.png"), width = 27, height = 15, dpi = 600)

