# DESCRIPTION: Building optimal model for blue marlin

# Load preliminaries
source("06a_BLUM_Data.R") # Load BLUM data

#### Create model ####

# With longitude and latitude
# 5-fold grid search
CVGrid <- CVgridSearch(train, test, tc = c(1, 2), bf = c(0.5, 0.75), lr = seq(0.005, 0.01, 0.001), pred_in = c(7:23, 25), resp_in = 5)

print(CVGrid %>% dplyr::arrange(desc(test_AUC)), n = 1) # BEST TEST AUC

# Building most optimal model
BLUM_model <- dismo::gbm.step(data = train, gbm.x = c(7:23, 25),
                              gbm.y = 5, family = "bernoulli", n.folds = 5,
                              tree.complexity = 2, bag.fraction = 0.5, learning.rate = 0.009
)
saveRDS(BLUM_model, here::here(model_dir, paste(species, "model.rds", sep = "_")))
# BLUM_model <- readRDS(here::here(model_dir, paste(species, "model.rds", sep = "_")))

# Show the relative importance of each of the predictors
summary(BLUM_model)

# Number of trees
BLUM_model$n.trees

# Printing AUCs
BLUM_model$self.statistics$discrimination # Training AUC Score
BLUM_model$cv.statistics$discrimination.mean # Validation AUC Score

# Predict to the testing dataset
preds <- gbm::predict.gbm(BLUM_model, test, n.trees = BLUM_model$gbm.call$best.trees, type = "response")
dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
get_testAUC(test$abundance_presence, preds) # Print testing AUC

# Plot maps
train_tmp <- train %>% 
  dplyr::mutate(model = BLUM_model$fitted)
test_tmp <- test %>% 
  dplyr::mutate(model = preds)
limits = c(0, 0.85)

# January-March
gg_obj <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                            test_tmp, # testing object with model column (predictions)
                            "jan-mar", # season
                            BLUM_predict_season1, # rest of the ocean cells
                            BLUM_model, # BRT model
                            `grid_BLUM_jan-mar` # grid of species for specific season
)

gg <- plotModel(gg_obj, limits) # plot extrapolations for Jan-Mar
ggsave(plot = gg, filename = here::here(figure_dir, paste(species, "jan-mar", "base.png", sep = "_")), 
       width = 14, height = 5, dpi = 600)

# April-June
gg_obj <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                            test_tmp, # testing object with model column (predictions)
                            "apr-jun", # season
                            BLUM_predict_season2, # rest of the ocean cells
                            BLUM_model, # BRT model
                            `grid_BLUM_apr-jun` # grid of species for specific season
)

gg <- plotModel(gg_obj, limits) # plot extrapolations for Apr-Jun
ggsave(plot = gg, filename = here::here(figure_dir, paste(species, "apr-jun", "base.png", sep = "_")), 
       width = 14, height = 5, dpi = 600)

# July-September
gg_obj <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                            test_tmp, # testing object with model column (predictions)
                            "jul-sept", # season
                            BLUM_predict_season3, # rest of the ocean cells
                            BLUM_model, # BRT model
                            `grid_BLUM_jul-sept` # grid of species for specific season
)

gg <- plotModel(gg_obj, limits) # plot extrapolations for Jul-Sept
ggsave(plot = gg, filename = here::here(figure_dir, paste(species, "jul-sept", "base.png", sep = "_")), 
       width = 14, height = 5, dpi = 600)

# October-December
gg_obj <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                            test_tmp, # testing object with model column (predictions)
                            "oct-dec", # season
                            BLUM_predict_season4, # rest of the ocean cells
                            BLUM_model, # BRT model
                            `grid_BLUM_oct-dec` # grid of species for specific season
)

gg <- plotModel(gg_obj, limits) # plot extrapolations for Oct-Dec
ggsave(plot = gg, filename = here::here(figure_dir, paste(species, "oct-dec", "base.png", sep = "_")), 
       width = 14, height = 5, dpi = 600)
