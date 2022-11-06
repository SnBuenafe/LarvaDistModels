###########################
## Load preliminaries ##
###########################
# Load SKP Data
source("03a_SKPData.R")

#####################################################
## Model 1: Restricted plots (Original predictors) ##
#####################################################

SKP_model1 <- dismo::gbm.step(data = train, gbm.x = c(7:13, 19:20),
                              gbm.y = 5, family = "bernoulli", n.folds = 5)
saveRDS(SKP_model1, "Output/Models/SKP_model1.rds") # save the model
#SKP_model1 <- readRDS("Output/Models/SKP_model1.rds") # load the model

summary(SKP_model1) # get the relative importance of each of the predictors

# AUCs
SKP_model1$self.statistics$discrimination # Training AUC Score
SKP_model1$cv.statistics$discrimination.mean # Validation AUC Score

##### Plotting ##### 
train_tmp <- train %>% 
  dplyr::mutate(model = SKP_model1$fitted)
preds <- gbm::predict.gbm(SKP_model1, test, n.trees = SKP_model1$gbm.call$best.trees, type = "response") # predict to test

dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
.roc(test$abundance_presence, preds) # Get testing AUC

test_tmp <- test %>% 
  dplyr::mutate(model = preds)

#### January-March
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jan-mar", # season
                        SKP_predict_season1 %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)), # rest of the ocean cells
                        SKP_model1, # BRT model
                        `grid_SKP_jan-mar` %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)) # grid of species for specific season
)

ggseason1 <- plotModel(gg[[1]], gg[[2]]) + # Plot the model
  ggtitle("January-March")

#### April-June
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "apr-jun", # season
                        SKP_predict_season2 %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)), # rest of the ocean cells
                        SKP_model1, # BRT model
                        `grid_SKP_apr-jun` %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)) # grid of species for specific season
)

ggseason2 <- plotModel(gg[[1]], gg[[2]]) + # Plot the model
  ggtitle("April-June")

#### July-September
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jul-sept", # season
                        SKP_predict_season3 %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)), # rest of the ocean cells
                        SKP_model1, # BRT model
                        `grid_SKP_jul-sept` %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)) # grid of species for specific season
)

ggseason3 <- plotModel(gg[[1]], gg[[2]]) + # Plot the model
  ggtitle("July-September")

#### October-December
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "oct-dec", # season
                        SKP_predict_season4 %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)), # rest of the ocean cells
                        SKP_model1, # BRT model
                        `grid_SKP_oct-dec` %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)) # grid of species for specific season
)

ggseason4 <- plotModel(gg[[1]], gg[[2]]) + # Plot the model
  ggtitle("October-December")

#### Arrange seasonal plots
ggseasons <- (ggseason1 + ggseason2) / (ggseason3 + ggseason4) + 
  plot_layout(guides = "collect") +
  plot_annotation("Model 1: Original Predictors (AUC: 0.74)")

ggsave(plot = ggseasons, filename = "Figures/SKP/SKP_Model1.png", width = 27, height = 15, dpi = 600)

#######################################################
## Model 2: Restricted plots (Additional predictors) ##
#######################################################

SKP_model2 <- dismo::gbm.step(data = train, gbm.x = c(7:20),
                              gbm.y = 5, family = "bernoulli", n.folds = 5)
saveRDS(SKP_model2, "Output/Models/SKP_model2.rds") # save the model
#SKP_model2 <- readRDS("Output/Models/SKP_model2.rds") # load the model

summary(SKP_model2) # get the relative importance of each of the predictors

# AUCs
SKP_model2$self.statistics$discrimination # Training AUC Score
SKP_model2$cv.statistics$discrimination.mean # Validation AUC Score

##### Plotting ##### 
train_tmp <- train %>% 
  dplyr::mutate(model = SKP_model2$fitted)
preds <- gbm::predict.gbm(SKP_model2, test, n.trees = SKP_model2$gbm.call$best.trees, type = "response") # predict to test

dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
.roc(test$abundance_presence, preds) # Get testing AUC

test_tmp <- test %>% 
  dplyr::mutate(model = preds)

#### January-March
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jan-mar", # season
                        SKP_predict_season1 %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)), # rest of the ocean cells
                        SKP_model2, # BRT model
                        `grid_SKP_jan-mar` %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)) # grid of species for specific season
)

ggseason1 <- plotModel(gg[[1]], gg[[2]]) + # Plot the model
  ggtitle("January-March")

#### April-June
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "apr-jun", # season
                        SKP_predict_season2 %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)), # rest of the ocean cells
                        SKP_model2, # BRT model
                        `grid_SKP_apr-jun` %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)) # grid of species for specific season
)

ggseason2 <- plotModel(gg[[1]], gg[[2]]) + # Plot the model
  ggtitle("April-June")

#### July-September
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jul-sept", # season
                        SKP_predict_season3 %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)), # rest of the ocean cells
                        SKP_model2, # BRT model
                        `grid_SKP_jul-sept` %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)) # grid of species for specific season
)

ggseason3 <- plotModel(gg[[1]], gg[[2]]) + # Plot the model
  ggtitle("July-September")

#### October-December
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "oct-dec", # season
                        SKP_predict_season4 %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)), # rest of the ocean cells
                        SKP_model2, # BRT model
                        `grid_SKP_oct-dec` %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)) # grid of species for specific season
)

ggseason4 <- plotModel(gg[[1]], gg[[2]]) + # Plot the model
  ggtitle("October-December")

#### Arrange seasonal plots
ggseasons <- (ggseason1 + ggseason2) / (ggseason3 + ggseason4) + 
  plot_layout(guides = "collect") +
  plot_annotation("Model 2: Additional Predictors (AUC: 0.77)")

ggsave(plot = ggseasons, filename = "Figures/SKP/SKP_Model2.png", width = 27, height = 15, dpi = 600)
