# DESCRIPTION: Building optimal model for yellowfin tuna

# Load preliminaries
source("02a_YFT_Data.R") # Load YFT data
output_dir <- here::here("Output", "Models")

#### Model 1: With longitude and latitude ####

# 5-fold grid search
CVGrid <- CVgridSearch(train, test, tc = c(1, 2), bf = c(0.5, 0.75), lr = seq(0.005, 0.01, 0.001), pred_in = c(7:24), resp_in = 5) # with longitude and latitude

print(CVGrid %>% dplyr::arrange(desc(test_AUC)), n = 1) # BEST TEST AUC

# Building most optimal model
YFT_model1 <- dismo::gbm.step(data = train, gbm.x = c(7:24),
                              gbm.y = 5, family = "bernoulli", n.folds = 5,
                              tree.complexity = 2, bag.fraction = 0.5, learning.rate = 0.008
)
saveRDS(YFT_model1, here::here(output_dir, "YFT_model1.rds"))
# YFT_model1 <- readRDS("Output/Models/YFT_model1.rds")

# Show the relative importance of each of the predictors
summary(YFT_model1) 

# Printing AUCs
YFT_model1$self.statistics$discrimination # Training AUC Score
YFT_model1$cv.statistics$discrimination.mean # Validation AUC Score

# Predict to the testing dataset
preds <- gbm::predict.gbm(YFT_model1, test, n.trees = YFT_model1$gbm.call$best.trees, type = "response")
dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
get_testAUC(test$abundance_presence, preds) # Print testing AUC

# Plot maps
train_tmp <- train %>% 
  dplyr::mutate(model = YFT_model1$fitted)
test_tmp <- test %>% 
  dplyr::mutate(model = preds)

# January-March
gg <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jan-mar", # season
                        YFT_predict_season1, # rest of the ocean cells
                        YFT_model1, # BRT model
                        `grid_YFT_jan-mar` # grid of species for specific season
)

gg1 <- plotModel(gg[[1]], YFT_ds1) # Plot the squished model

# April-June
gg <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "apr-jun", # season
                        YFT_predict_season2, # rest of the ocean cells
                        YFT_model1, # BRT model
                        `grid_YFT_apr-jun` # grid of species for specific season
)

gg2 <- plotModel(gg[[1]], YFT_ds2) # Plot the squished model

# July-September
gg <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jul-sept", # season
                        YFT_predict_season3, # rest of the ocean cells
                        YFT_model1, # BRT model
                        `grid_YFT_jul-sept` # grid of species for specific season
)

gg3 <- plotModel(gg[[1]], YFT_ds3) # Plot the squished model

# October-December
gg <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "oct-dec", # season
                        YFT_predict_season4, # rest of the ocean cells
                        YFT_model1, # BRT model
                        `grid_YFT_oct-dec` # grid of species for specific season
)

gg4 <- plotModel(gg[[1]], YFT_ds4) # Plot the squished model

ggsquished <- (gg1 + gg2) / (gg3 + gg4) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = ggsquished, filename = "Figures/YFT/YFT_model1_base.png", width = 27, height = 15, dpi = 600)

#### Model 2: Without longitude and latitude ####

# 5-fold grid search
CVGrid <- CVgridSearch(train, test, tc = c(1, 2), bf = c(0.5, 0.75), lr = seq(0.005, 0.01, 0.001), pred_in = c(9:24), resp_in = 5) # with longitude and latitude

print(CVGrid %>% dplyr::arrange(desc(test_AUC)), n = 1) # BEST TEST AUC

# Building most optimal model
YFT_model2 <- dismo::gbm.step(data = train, gbm.x = c(9:24),
                              gbm.y = 5, family = "bernoulli", n.folds = 5,
                              tree.complexity = 2, bag.fraction = 0.75, learning.rate = 0.008
)
saveRDS(YFT_model2, here::here(output_dir, "YFT_model2.rds"))
# YFT_model2 <- readRDS("Output/Models/YFT_model2.rds")

# Show the relative importance of each of the predictors
summary(YFT_model2) 

# Printing AUCs
YFT_model2$self.statistics$discrimination # Training AUC Score
YFT_model2$cv.statistics$discrimination.mean # Validation AUC Score

# Predict to the testing dataset
preds <- gbm::predict.gbm(YFT_model2, test, n.trees = YFT_model2$gbm.call$best.trees, type = "response")
dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
get_testAUC(test$abundance_presence, preds) # Print testing AUC

# Plot maps
train_tmp <- train %>% 
  dplyr::mutate(model = YFT_model2$fitted)
test_tmp <- test %>% 
  dplyr::mutate(model = preds)

# January-March
gg <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jan-mar", # season
                        YFT_predict_season1, # rest of the ocean cells
                        YFT_model2, # BRT model
                        `grid_YFT_jan-mar` # grid of species for specific season
)

gg1 <- plotModel(gg[[1]], YFT_ds1) # Plot the squished model

# April-June
gg <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "apr-jun", # season
                        YFT_predict_season2, # rest of the ocean cells
                        YFT_model2, # BRT model
                        `grid_YFT_apr-jun` # grid of species for specific season
)

gg2 <- plotModel(gg[[1]], YFT_ds2) # Plot the squished model

# July-September
gg <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jul-sept", # season
                        YFT_predict_season3, # rest of the ocean cells
                        YFT_model2, # BRT model
                        `grid_YFT_jul-sept` # grid of species for specific season
)

gg3 <- plotModel(gg[[1]], YFT_ds3) # Plot the squished model

# October-December
gg <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "oct-dec", # season
                        YFT_predict_season4, # rest of the ocean cells
                        YFT_model2, # BRT model
                        `grid_YFT_oct-dec` # grid of species for specific season
)

gg4 <- plotModel(gg[[1]], YFT_ds4) # Plot the squished model

ggsquished <- (gg1 + gg2) / (gg3 + gg4) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = ggsquished, filename = "Figures/YFT/YFT_model2_base.png", width = 27, height = 15, dpi = 600)
