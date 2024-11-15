# DESCRIPTION: Building optimal model for frigate tuna

# Load preliminaries
source(file.path("analyses", "04_models", "07a_FRI_Data.R")) # Load FRI data

#### Create model ####

# With longitude and latitude
# 5-fold grid search
CVGrid <- CVgridSearch(train, test, tc = c(1, 2), bf = c(0.5, 0.75), lr = seq(0.005, 0.01, 0.001), pred_in = c(7:23, 26), resp_in = 5)

print(CVGrid %>% dplyr::arrange(desc(test_AUC)), n = 1) # BEST TEST AUC

# Building most optimal model
FRI_model <- dismo::gbm.step(data = train, gbm.x = c(7:23, 26),
                               gbm.y = 5, family = "bernoulli", n.folds = 5,
                               tree.complexity = 2, bag.fraction = 0.5, learning.rate = 0.009
)
saveRDS(FRI_model, here::here(model_dir, paste(species, "model.rds", sep = "_")))
# FRI_model <- readRDS(here::here(model_dir, paste(species, "model.rds", sep = "_")))

# Show the relative importance of each of the predictors
summary(FRI_model)

# Number of trees
FRI_model$n.trees

# Printing AUCs
FRI_model$self.statistics$discrimination # Training AUC Score
FRI_model$cv.statistics$discrimination.mean # Validation AUC Score

# Predict to the testing dataset
preds <- gbm::predict.gbm(FRI_model, test, n.trees = FRI_model$gbm.call$best.trees, type = "response")
dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
get_testAUC(test$abundance_presence, preds) # Print testing AUC

#### Plotting maps ####

train_tmp <- train %>% 
  dplyr::mutate(model = FRI_model$fitted)
test_tmp <- test %>% 
  dplyr::mutate(model = preds)
limits = c(0, 1)

# January-March
gg_obj <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                            test_tmp, # testing object with model column (predictions)
                            "jan-mar", # season
                            FRI_predict_season1, # rest of the ocean cells
                            FRI_model, # BRT model
                            `grid_FRI_jan-mar` # grid of species for specific season
)

gg <- plotModel(gg_obj, limits) # plot extrapolations for Jan-Mar
ggsave(plot = gg, filename = here::here(figure_dir, paste(species, "jan-mar", "base.png", sep = "_")), 
       width = 14, height = 5, dpi = 600)

# April-June
gg_obj <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                            test_tmp, # testing object with model column (predictions)
                            "apr-jun", # season
                            FRI_predict_season2, # rest of the ocean cells
                            FRI_model, # BRT model
                            `grid_FRI_apr-jun` # grid of species for specific season
)

gg <- plotModel(gg_obj, limits) # plot extrapolations for Apr-Jun
ggsave(plot = gg, filename = here::here(figure_dir, paste(species, "apr-jun", "base.png", sep = "_")), 
       width = 14, height = 5, dpi = 600)

# July-September
gg_obj <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                            test_tmp, # testing object with model column (predictions)
                            "jul-sept", # season
                            FRI_predict_season3, # rest of the ocean cells
                            FRI_model, # BRT model
                            `grid_FRI_jul-sept` # grid of species for specific season
)

gg <- plotModel(gg_obj, limits) # plot extrapolations for Jul-Sept
ggsave(plot = gg, filename = here::here(figure_dir, paste(species, "jul-sept", "base.png", sep = "_")), 
       width = 14, height = 5, dpi = 600)

# October-December
gg_obj <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                            test_tmp, # testing object with model column (predictions)
                            "oct-dec", # season
                            FRI_predict_season4, # rest of the ocean cells
                            FRI_model, # BRT model
                            `grid_FRI_oct-dec` # grid of species for specific season
)

gg <- plotModel(gg_obj, limits) # plot extrapolations for Oct-Dec
ggsave(plot = gg, filename = here::here(figure_dir, paste(species, "oct-dec", "base.png", sep = "_")), 
       width = 14, height = 5, dpi = 600)
