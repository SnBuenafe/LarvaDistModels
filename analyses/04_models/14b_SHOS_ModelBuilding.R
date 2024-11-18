# DESCRIPTION: Building optimal model for shortbill spearfish

# Load preliminaries
source(file.path("analyses", "04_models", "14a_SHOS_Data.R")) # Load SHOS data

#### Create model ####

# With longitude and latitude
# 5-fold grid search
CVGrid <- CVgridSearch(train, test, tc = c(1, 2), bf = c(0.5, 0.75), lr = seq(0.005, 0.01, 0.001), pred_in = brt_cols, resp_in = 5)

print(CVGrid %>% dplyr::arrange(desc(test_AUC)), n = 1) # BEST TEST AUC

# Building most optimal model
SHOS_model <- dismo::gbm.step(data = train, gbm.x = brt_cols,
                              gbm.y = 5, family = "bernoulli", n.folds = 5,
                              tree.complexity = 2, bag.fraction = 0.75, learning.rate = 0.009
)
saveRDS(SHOS_model, here::here(model_dir, paste(species, "model.rds", sep = "_")))
# SHOS_model <- readRDS(here::here(model_dir, paste(species, "model.rds", sep = "_")))

# Show the relative importance of each of the predictors
summary(SHOS_model)

# Number of trees
SHOS_model$n.trees

# Printing AUCs
SHOS_model$self.statistics$discrimination # Training AUC Score
SHOS_model$cv.statistics$discrimination.mean # Validation AUC Score

# Predict to the testing dataset
preds <- gbm::predict.gbm(SHOS_model, test, n.trees = SHOS_model$gbm.call$best.trees, type = "response")
dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
get_testAUC(test$abundance_presence, preds) # Print testing AUC

#### Plotting maps ####
train_tmp <- train %>% 
  dplyr::mutate(model = SHOS_model$fitted)
test_tmp <- test %>% 
  dplyr::mutate(model = preds)
limits = c(0, 0.7)

# January-March
gg_obj <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                            test_tmp, # testing object with model column (predictions)
                            "jan-mar", # season
                            SHOS_predict_season1, # rest of the ocean cells
                            SHOS_model, # BRT model
                            `grid_SHOS_jan-mar` # grid of species for specific season
)

gg <- plotModel(gg_obj, limits) # get extrapolation for Jan-Mar
ggsave(plot = gg, filename = here::here(figure_dir, paste(species, "jan-mar", "base.png", sep = "_")), 
       width = 14, height = 5, dpi = 600)

# April-June
gg_obj <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                            test_tmp, # testing object with model column (predictions)
                            "apr-jun", # season
                            SHOS_predict_season2, # rest of the ocean cells
                            SHOS_model, # BRT model
                            `grid_SHOS_apr-jun` # grid of species for specific season
)

gg <- plotModel(gg_obj, limits) # plot extrapolation for Apr-Jun
ggsave(plot = gg, filename = here::here(figure_dir, paste(species, "apr-jun", "base.png", sep = "_")), 
       width = 14, height = 5, dpi = 600)

# July-September
gg_obj <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                            test_tmp, # testing object with model column (predictions)
                            "jul-sept", # season
                            SHOS_predict_season3, # rest of the ocean cells
                            SHOS_model, # BRT model
                            `grid_SHOS_jul-sept` # grid of species for specific season
)

gg <- plotModel(gg_obj, limits) # plot extrapolation for Jul-Sept
ggsave(plot = gg, filename = here::here(figure_dir, paste(species, "jul-sept", "base.png", sep = "_")), 
       width = 14, height = 5, dpi = 600)

# October-December
gg_obj <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                            test_tmp, # testing object with model column (predictions)
                            "oct-dec", # season
                            SHOS_predict_season4, # rest of the ocean cells
                            SHOS_model, # BRT model
                            `grid_SHOS_oct-dec` # grid of species for specific season
)

gg <- plotModel(gg_obj, limits) # plot extrapolation for Oct-Dec
ggsave(plot = gg, filename = here::here(figure_dir, paste(species, "oct-dec", "base.png", sep = "_")), 
       width = 14, height = 5, dpi = 600)
