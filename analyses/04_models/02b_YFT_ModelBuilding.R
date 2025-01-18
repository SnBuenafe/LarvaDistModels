# DESCRIPTION: Building optimal model for yellowfin tuna

# Load preliminaries
source(file.path("analyses", "04_models", "02a_YFT_Data.R")) # Load YFT data

#### Create model ####

# With longitude and latitude
# 5-fold grid search
CVGrid <- CVgridSearch(train, test, tc = c(1, 2), bf = c(0.5, 0.75), lr = seq(0.005, 0.01, 0.001), pred_in = brt_cols, resp_in = 5)

saveRDS(CVGrid, file.path(CVgrid_dir, paste(species, "CVGrid.rds", sep = "_")))

best <- CVGrid %>% 
  dplyr::arrange(desc(test_AUC)) %>%  # BEST TEST AUC
  dplyr::slice_head(n = 1)

# Building most optimal model
YFT_model <- dismo::gbm.step(data = train, gbm.x = brt_cols,
                             gbm.y = 5, family = "bernoulli", n.folds = 5,
                             tree.complexity = best$tree_complexity, 
                             bag.fraction = best$bag_fraction, 
                             learning.rate = best$learning_rate)

saveRDS(YFT_model, file.path(model_dir, paste(species, "model.rds", sep = "_")))
# YFT_model <- readRDS(file.path(model_dir, paste(species, "model.rds", sep = "_")))

# Show the relative importance of each of the predictors
summary(YFT_model)

# Number of trees
YFT_model$n.trees

# Printing AUCs
YFT_model$self.statistics$discrimination # Training AUC Score
YFT_model$cv.statistics$discrimination.mean # Validation AUC Score

# Predict to the testing dataset
preds <- gbm::predict.gbm(YFT_model, test, n.trees = YFT_model$gbm.call$best.trees, type = "response")
dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
get_testAUC(test$abundance_presence, preds) # Print testing AUC

#### Plotting maps ####

train_tmp <- train %>% 
  dplyr::mutate(model = YFT_model$fitted)
test_tmp <- test %>% 
  dplyr::mutate(model = preds)
limits = c(0, 0.9)

# January-March
gg_obj <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                            test_tmp, # testing object with model column (predictions)
                            "jan-mar", # season
                            YFT_predict_season1, # rest of the ocean cells
                            YFT_model, # BRT model
                            `grid_YFT_jan-mar` # grid of species for specific season
)

gg <- plotModel(gg_obj, limits)
ggsave(plot = gg, filename = file.path(figure_dir, paste(species, "jan-mar", "base.png", sep = "_")), 
       width = 14, height = 5, dpi = 600)

nish <- plotNish(`grid_YFT_jan-mar`) # plot nishikawa presence-absence data for Jan-Mar
ggsave(plot = nish, filename = file.path(figure_dir, paste(species, "jan-mar", "nishikawa.png", sep = "_")),
       width = 14, height = 5, dpi = 600)

# April-June
gg_obj <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                            test_tmp, # testing object with model column (predictions)
                            "apr-jun", # season
                            YFT_predict_season2, # rest of the ocean cells
                            YFT_model, # BRT model
                            `grid_YFT_apr-jun` # grid of species for specific season
)

gg <- plotModel(gg_obj, limits)
ggsave(plot = gg, filename = file.path(figure_dir, paste(species, "apr-jun", "base.png", sep = "_")), 
       width = 14, height = 5, dpi = 600)

nish <- plotNish(`grid_YFT_apr-jun`) # plot nishikawa presence-absence data for April-June
ggsave(plot = nish, filename = file.path(figure_dir, paste(species, "apr-jun", "nishikawa.png", sep = "_")),
       width = 14, height = 5, dpi = 600)

# July-September
gg_obj <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                            test_tmp, # testing object with model column (predictions)
                            "jul-sep", # season
                            YFT_predict_season3, # rest of the ocean cells
                            YFT_model, # BRT model
                            `grid_YFT_jul-sep` # grid of species for specific season
)

gg <- plotModel(gg_obj, limits)
ggsave(plot = gg, filename = file.path(figure_dir, paste(species, "jul-sep", "base.png", sep = "_")), 
       width = 14, height = 5, dpi = 600)

nish <- plotNish(`grid_YFT_jul-sep`) # plot nishikawa presence-absence data for July-September
ggsave(plot = nish, filename = file.path(figure_dir, paste(species, "jul-sep", "nishikawa.png", sep = "_")),
       width = 14, height = 5, dpi = 600)

# October-December
gg_obj <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                            test_tmp, # testing object with model column (predictions)
                            "oct-dec", # season
                            YFT_predict_season4, # rest of the ocean cells
                            YFT_model, # BRT model
                            `grid_YFT_oct-dec` # grid of species for specific season
)

gg <- plotModel(gg_obj, limits) # plot model extrapolation for Oct-Dec
ggsave(plot = gg, filename = file.path(figure_dir, paste(species, "oct-dec", "base.png", sep = "_")), 
       width = 14, height = 5, dpi = 600)

nish <- plotNish(`grid_YFT_oct-dec`) # plot nishikawa presence-absence data for October-December
ggsave(plot = nish, filename = file.path(figure_dir, paste(species, "oct-dec", "nishikawa.png", sep = "_")),
       width = 14, height = 5, dpi = 600)
