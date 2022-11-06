# DESCRIPTION: Plotting potential models for YFT

###########################
## Load preliminaries ##
###########################
# Load preliminaries
source("00_Utils.R")

# Load YFT data
source("02a_YFTData.R")

###########################################################################
## Model 7: Least overfit; moderate complexity (original predictors) ##
###########################################################################
# Load the model 7
YFT_model7 <- readRDS("Output/Models/YFT_model7.rds")

# What are the min and max latitudes?
min(train$latitude)
max(train$latitude)

train_tmp <- train %>% 
  dplyr::mutate(model = YFT_model7$fitted)
preds <- gbm::predict.gbm(YFT_model7, test, n.trees = YFT_model7$gbm.call$best.trees, type = "response") # predict to test

test_tmp <- test %>% 
  dplyr::mutate(model = preds)

#### January-March
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jan-mar", # season
                        YFT_predict_season1 %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)), # rest of the ocean cells
                        YFT_model7, # BRT model
                        `grid_YFT_jan-mar` %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)) # grid of species for specific season with restricted ranges
)

ggseason1 <- plotModel(gg[[1]], gg[[2]]) + # Plot the model
  ggtitle("January-March")

#### April-June
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "apr-jun", # season
                        YFT_predict_season2 %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)), # rest of the ocean cells
                        YFT_model7, # BRT model
                        `grid_YFT_apr-jun` %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude))# grid of species for specific season
)

ggseason2 <- plotModel(gg[[1]], gg[[2]]) + # Plot the model
  ggtitle("April-June")

#### July-September
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jul-sept", # season
                        YFT_predict_season3 %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)), # rest of the ocean cells
                        YFT_model7, # BRT model
                        `grid_YFT_jul-sept` %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)) # grid of species for specific season
)

ggseason3 <- plotModel(gg[[1]], gg[[2]]) + # Plot the model
  ggtitle("July-September")

#### October-December
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "oct-dec", # season
                        YFT_predict_season4 %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)), # rest of the ocean cells
                        YFT_model7, # BRT model
                        `grid_YFT_oct-dec` %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude))# grid of species for specific season
)

ggseason4 <- plotModel(gg[[1]], gg[[2]]) + # Plot the model
  ggtitle("October-December")

ggseasons <- (ggseason1 + ggseason2) / (ggseason3 + ggseason4) + 
  plot_layout(guides = "collect") +
  plot_annotation("Model 7: Original predictors with moderate complexity (AUC: 0.80)")

ggsave(plot = ggseasons, filename = "Figures/YFT/YFT_Model7.png", width = 27, height = 15, dpi = 600)

###########################################################################
## Model 10: Best test AUC (original predictors) ##
###########################################################################
# Load the model 10
YFT_model10 <- readRDS("Output/Models/YFT_model10.rds")

# What are the min and max latitudes?
min(train$latitude)
max(train$latitude)

train_tmp <- train %>% 
  dplyr::mutate(model = YFT_model10$fitted)
preds <- gbm::predict.gbm(YFT_model10, test, n.trees = YFT_model10$gbm.call$best.trees, type = "response") # predict to test

test_tmp <- test %>% 
  dplyr::mutate(model = preds)

#### January-March
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jan-mar", # season
                        YFT_predict_season1 %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)), # rest of the ocean cells
                        YFT_model10, # BRT model
                        `grid_YFT_jan-mar` %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)) # grid of species for specific season with restricted ranges
)

ggseason1 <- plotModel(gg[[1]], gg[[2]]) + # Plot the model
  ggtitle("January-March")

#### April-June
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "apr-jun", # season
                        YFT_predict_season2 %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)), # rest of the ocean cells
                        YFT_model10, # BRT model
                        `grid_YFT_apr-jun` %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude))# grid of species for specific season
)

ggseason2 <- plotModel(gg[[1]], gg[[2]]) + # Plot the model
  ggtitle("April-June")

#### July-September
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jul-sept", # season
                        YFT_predict_season3 %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)), # rest of the ocean cells
                        YFT_model10, # BRT model
                        `grid_YFT_jul-sept` %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)) # grid of species for specific season
)

ggseason3 <- plotModel(gg[[1]], gg[[2]]) + # Plot the model
  ggtitle("July-September")

#### October-December
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "oct-dec", # season
                        YFT_predict_season4 %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)), # rest of the ocean cells
                        YFT_model10, # BRT model
                        `grid_YFT_oct-dec` %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude))# grid of species for specific season
)

ggseason4 <- plotModel(gg[[1]], gg[[2]]) + # Plot the model
  ggtitle("October-December")

ggseasons <- (ggseason1 + ggseason2) / (ggseason3 + ggseason4) + 
  plot_layout(guides = "collect") +
  plot_annotation("Model 10: Original predictors with best test AUC (AUC: 0.81)")

ggsave(plot = ggseasons, filename = "Figures/YFT/YFT_Model10.png", width = 27, height = 15, dpi = 600)

###########################################################################
## Model 12: Best test AUC; moderate complexity (additional predictors) ##
###########################################################################
# Load the model 12
YFT_model12 <- readRDS("Output/Models/YFT_model12.rds")

# What are the min and max latitudes?
min(train$latitude)
max(train$latitude)

train_tmp <- train %>% 
  dplyr::mutate(model = YFT_model12$fitted)
preds <- gbm::predict.gbm(YFT_model12, test, n.trees = YFT_model12$gbm.call$best.trees, type = "response") # predict to test

test_tmp <- test %>% 
  dplyr::mutate(model = preds)

#### January-March
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jan-mar", # season
                        YFT_predict_season1 %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)), # rest of the ocean cells
                        YFT_model12, # BRT model
                        `grid_YFT_jan-mar` %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)) # grid of species for specific season with restricted ranges
)

ggseason1 <- plotModel(gg[[1]], gg[[2]]) + # Plot the model
  ggtitle("January-March")

#### April-June
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "apr-jun", # season
                        YFT_predict_season2 %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)), # rest of the ocean cells
                        YFT_model12, # BRT model
                        `grid_YFT_apr-jun` %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude))# grid of species for specific season
)

ggseason2 <- plotModel(gg[[1]], gg[[2]]) + # Plot the model
  ggtitle("April-June")

#### July-September
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jul-sept", # season
                        YFT_predict_season3 %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)), # rest of the ocean cells
                        YFT_model12, # BRT model
                        `grid_YFT_jul-sept` %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)) # grid of species for specific season
)

ggseason3 <- plotModel(gg[[1]], gg[[2]]) + # Plot the model
  ggtitle("July-September")

#### October-December
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "oct-dec", # season
                        YFT_predict_season4 %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)), # rest of the ocean cells
                        YFT_model12, # BRT model
                        `grid_YFT_oct-dec` %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude))# grid of species for specific season
)

ggseason4 <- plotModel(gg[[1]], gg[[2]]) + # Plot the model
  ggtitle("October-December")

ggseasons <- (ggseason1 + ggseason2) / (ggseason3 + ggseason4) + 
  plot_layout(guides = "collect") +
  plot_annotation("Model 12: Additional predictors with moderate complexity (AUC: 0.82)")

ggsave(plot = ggseasons, filename = "Figures/YFT/YFT_Model12.png", width = 27, height = 15, dpi = 600)

###########################################################################
## Model 13: Least overfit; moderate complexity (additional predictors) ##
###########################################################################
# Load the model 13
YFT_model13 <- readRDS("Output/Models/YFT_model13.rds")

# What are the min and max latitudes?
min(train$latitude)
max(train$latitude)

train_tmp <- train %>% 
  dplyr::mutate(model = YFT_model13$fitted)
preds <- gbm::predict.gbm(YFT_model13, test, n.trees = YFT_model13$gbm.call$best.trees, type = "response") # predict to test

test_tmp <- test %>% 
  dplyr::mutate(model = preds)

#### January-March
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jan-mar", # season
                        YFT_predict_season1 %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)), # rest of the ocean cells
                        YFT_model13, # BRT model
                        `grid_YFT_jan-mar` %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)) # grid of species for specific season with restricted ranges
)

ggseason1 <- plotModel(gg[[1]], gg[[2]]) + # Plot the model
  ggtitle("January-March")

#### April-June
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "apr-jun", # season
                        YFT_predict_season2 %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)), # rest of the ocean cells
                        YFT_model13, # BRT model
                        `grid_YFT_apr-jun` %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude))# grid of species for specific season
)

ggseason2 <- plotModel(gg[[1]], gg[[2]]) + # Plot the model
  ggtitle("April-June")

#### July-September
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jul-sept", # season
                        YFT_predict_season3 %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)), # rest of the ocean cells
                        YFT_model13, # BRT model
                        `grid_YFT_jul-sept` %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)) # grid of species for specific season
)

ggseason3 <- plotModel(gg[[1]], gg[[2]]) + # Plot the model
  ggtitle("July-September")

#### October-December
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "oct-dec", # season
                        YFT_predict_season4 %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)), # rest of the ocean cells
                        YFT_model13, # BRT model
                        `grid_YFT_oct-dec` %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude))# grid of species for specific season
)

ggseason4 <- plotModel(gg[[1]], gg[[2]]) + # Plot the model
  ggtitle("October-December")

ggseasons <- (ggseason1 + ggseason2) / (ggseason3 + ggseason4) + 
  plot_layout(guides = "collect") +
  plot_annotation("Model 13: Additional predictors; least overfit (AUC: 0.81)")

ggsave(plot = ggseasons, filename = "Figures/YFT/YFT_Model13.png", width = 27, height = 15, dpi = 600)
