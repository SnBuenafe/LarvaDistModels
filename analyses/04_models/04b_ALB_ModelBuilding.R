# DESCRIPTION: Building optimal model for albacore

# Load preliminaries
source(file.path("analyses", "04_models", "04a_ALB_Data.R")) # Load ALB data

#### Create model ####

# With longitude and latitude
# 5-fold grid search
CVGrid <- CVgridSearch(train, test, tc = c(1, 2), bf = c(0.5, 0.75), lr = seq(0.005, 0.01, 0.001), pred_in = brt_cols, resp_in = 5)

saveRDS(CVGrid, file.path(CVgrid_dir, paste(species, "CVGrid.rds", sep = "_")))

best <- CVGrid %>% 
  dplyr::arrange(desc(test_AUC)) %>%  # BEST TEST AUC
  dplyr::slice_head(n = 1)

# Building most optimal model
ALB_model <- dismo::gbm.step(data = train, gbm.x = brt_cols,
                              gbm.y = 5, family = "bernoulli", n.folds = 5,
                             tree.complexity = best$tree_complexity, 
                             bag.fraction = best$bag_fraction, 
                             learning.rate = best$learning_rate)


saveRDS(ALB_model, file.path(model_dir, paste(species, "model.rds", sep = "_")))
# ALB_model <- readRDS(file.path(model_dir, paste(species, "model.rds", sep = "_")))

# Show the relative importance of each of the predictors
summary(ALB_model)

# Number of trees
ALB_model$n.trees

# Printing AUCs
ALB_model$self.statistics$discrimination # Training AUC Score
ALB_model$cv.statistics$discrimination.mean # Validation AUC Score

# Predict to the testing dataset
preds <- gbm::predict.gbm(ALB_model, test, n.trees = ALB_model$gbm.call$best.trees, type = "response")
dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
get_testAUC(test$abundance_presence, preds) # Print testing AUC

#### Plotting maps ####

train_tmp <- train %>% 
  dplyr::mutate(model = ALB_model$fitted)
test_tmp <- test %>% 
  dplyr::mutate(model = preds)
limits = c(0, 0.8)

# January-March
gg_obj <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                            test_tmp, # testing object with model column (predictions)
                            "jan-mar", # season
                            ALB_predict_season1, # rest of the ocean cells
                            ALB_model, # BRT model
                            `grid_ALB_jan-mar` # grid of species for specific season
)

gg <- plotModel(gg_obj, limits) # plot extrapolation for Jan-Mar
ggsave(plot = gg, filename = file.path(figure_dir, paste(species, "jan-mar", "base.png", sep = "_")), 
       width = 14, height = 5, dpi = 600)


# April-June
gg_obj <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                            test_tmp, # testing object with model column (predictions)
                            "apr-jun", # season
                            ALB_predict_season2, # rest of the ocean cells
                            ALB_model, # BRT model
                            `grid_ALB_apr-jun` # grid of species for specific season
)

gg <- plotModel(gg_obj, limits) # plot extrapolation for Apr-Jun
ggsave(plot = gg, filename = file.path(figure_dir, paste(species, "apr-jun", "base.png", sep = "_")), 
       width = 14, height = 5, dpi = 600)

# July-September
gg_obj <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                            test_tmp, # testing object with model column (predictions)
                            "jul-sep", # season
                            ALB_predict_season3, # rest of the ocean cells
                            ALB_model, # BRT model
                            `grid_ALB_jul-sep` # grid of species for specific season
)

gg <- plotModel(gg_obj, limits) # plot extrapolation for jul-sep
ggsave(plot = gg, filename = file.path(figure_dir, paste(species, "jul-sep", "base.png", sep = "_")), 
       width = 14, height = 5, dpi = 600)

# October-December
gg_obj <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                            test_tmp, # testing object with model column (predictions)
                            "oct-dec", # season
                            ALB_predict_season4, # rest of the ocean cells
                            ALB_model, # BRT model
                            `grid_ALB_oct-dec` # grid of species for specific season
)

gg <- plotModel(gg_obj, limits) # plot extrapolation for Oct-Dec
ggsave(plot = gg, filename = file.path(figure_dir, paste(species, "oct-dec", "base.png", sep = "_")), 
       width = 14, height = 5, dpi = 600)
