# DESCRIPTION: These models are solely for determining how we want to include geographical location.

###########################
## Load preliminaries ##
###########################
# Load YFT data
source("02a_YFT_Data.R")

###########################
## Model 0: None ##
###########################

# Building models
# check the index numbers of the columns
colnames(train)

# higher AUC better. A model with AUC values closer to 0 have more wrong predictions.
# see: https://rspatial.org/raster/sdm/9_sdm_brt.html for interpreting results
YFT_model0 <- dismo::gbm.step(data = train, gbm.x = c(9:20),
                              gbm.y = 5, family = "bernoulli", n.folds = 5)
saveRDS(YFT_model0, "Output/Models/YFT_model0.rds") # save the model
#YFT_model0 <- readRDS("Output/Models/YFT_model0.rds") # load the model

summary(YFT_model0) # get the relative importance of each of the predictors

# Check this for ROC metrics: https://stackoverflow.com/questions/22391547/roc-score-in-gbm-package
# CV AUC of ROC (Receiver Operating Characteristic Curve)
YFT_model0$self.statistics$discrimination # Training AUC Score
YFT_model0$cv.statistics$discrimination.mean # Validation AUC Score
# To calculate R2: https://stats.stackexchange.com/questions/76997/measure-the-goodness-of-fit-in-boosted-regression-tree

##### Plotting ##### 
train_tmp <- train %>% 
  dplyr::mutate(model = YFT_model0$fitted)
preds <- gbm::predict.gbm(YFT_model0, test, n.trees = YFT_model0$gbm.call$best.trees, type = "response") # predict to test

dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
.roc(test$abundance_presence, preds) # Get testing AUC

test_tmp <- test %>% 
  dplyr::mutate(model = preds)

#### January-March
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jan-mar", # season
                        YFT_predict_season1, # rest of the ocean cells
                        YFT_model0, # BRT model
                        `grid_YFT_jan-mar` # grid of species for specific season
                        )

ggseason1 <- plotModel(gg[[1]]) + # Plot the model
  ggtitle("January-March")

#### April-June
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "apr-jun", # season
                        YFT_predict_season2, # rest of the ocean cells
                        YFT_model0, # BRT model
                        `grid_YFT_apr-jun` # grid of species for specific season
)

ggseason2 <- plotModel(gg[[1]]) + # Plot the model
  ggtitle("April-June")

#### July-September
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jul-sept", # season
                        YFT_predict_season3, # rest of the ocean cells
                        YFT_model0, # BRT model
                        `grid_YFT_jul-sept` # grid of species for specific season
)

ggseason3 <- plotModel(gg[[1]]) + # Plot the model
  ggtitle("July-September")

#### October-December
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "oct-dec", # season
                        YFT_predict_season4, # rest of the ocean cells
                        YFT_model0, # BRT model
                        `grid_YFT_oct-dec` # grid of species for specific season
)

ggseason4 <- plotModel(gg[[1]]) + # Plot the model
  ggtitle("October-December")

#### Arrange seasonal plots
ggseasons <- (ggseason1 + ggseason2) / (ggseason3 + ggseason4) + 
  plot_layout(guides = "collect") +
  plot_annotation("Model 0: No geographical location (AUC: 0.74)")

ggsave(plot = ggseasons, filename = "Figures/YFT/YFT_Model0.png", width = 27, height = 15, dpi = 600)

###########################
## Model 1: Oceans ##
###########################

YFT_model1 <- dismo::gbm.step(data = train, gbm.x = c(6, 9:20),
                              gbm.y = 5, family = "bernoulli", n.folds = 5)
saveRDS(YFT_model1, "Output/Models/YFT_model1.rds") # save the model
#YFT_model1 <- readRDS("Output/Models/YFT_model1.rds") # load the model

summary(YFT_model1) # get the relative importance of each of the predictors

# AUCs
YFT_model1$self.statistics$discrimination # Training AUC
YFT_model1$cv.statistics$discrimination.mean # Validation AUC Score

##### Plotting ##### 
train_tmp <- train %>% 
  dplyr::mutate(model = YFT_model1$fitted)
preds <- gbm::predict.gbm(YFT_model1, test, n.trees = YFT_model1$gbm.call$best.trees, type = "response") # predict to test

dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
.roc(test$abundance_presence, preds) # Get testing AUC

test_tmp <- test %>% 
  dplyr::mutate(model = preds)

#### January-March
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jan-mar", # season
                        YFT_predict_season1, # rest of the ocean cells
                        YFT_model1, # BRT model
                        `grid_YFT_jan-mar` # grid of species for specific season
)

ggseason1 <- plotModel(gg[[1]]) + # Plot the model
  ggtitle("January-March")

#### April-June
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "apr-jun", # season
                        YFT_predict_season2, # rest of the ocean cells
                        YFT_model1, # BRT model
                        `grid_YFT_apr-jun` # grid of species for specific season
)

ggseason2 <- plotModel(gg[[1]]) + # Plot the model
  ggtitle("April-June")

#### July-September
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jul-sept", # season
                        YFT_predict_season3, # rest of the ocean cells
                        YFT_model1, # BRT model
                        `grid_YFT_jul-sept` # grid of species for specific season
)

ggseason3 <- plotModel(gg[[1]]) + # Plot the model
  ggtitle("July-September")

#### October-December
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "oct-dec", # season
                        YFT_predict_season4, # rest of the ocean cells
                        YFT_model1, # BRT model
                        `grid_YFT_oct-dec` # grid of species for specific season
)

ggseason4 <- plotModel(gg[[1]]) + # Plot the model
  ggtitle("October-December")

#### Arrange seasonal plots
ggseasons <- (ggseason1 + ggseason2) / (ggseason3 + ggseason4) + 
  plot_layout(guides = "collect") +
  plot_annotation("Model 1: Ocean basins (AUC: 0.75)")

ggsave(plot = ggseasons, filename = "Figures/YFT/YFT_Model1.png", width = 27, height = 15, dpi = 600)
###########################
## Model 2: Longitude ##
###########################

YFT_model2 <- dismo::gbm.step(data = train, gbm.x = c(7, 9:20),
                              gbm.y = 5, family = "bernoulli", n.folds = 5)
saveRDS(YFT_model2, "Output/Models/YFT_model2.rds") # save the model
#YFT_model2 <- readRDS("Output/Models/YFT_model2.rds") # load the model

summary(YFT_model2) # get the relative importance of each of the predictors

# AUCs
YFT_model2$self.statistics$discrimination # Training AUC Score
YFT_model2$cv.statistics$discrimination.mean # Validation AUC Score

##### Plotting ##### 
train_tmp <- train %>% 
  dplyr::mutate(model = YFT_model2$fitted)
preds <- gbm::predict.gbm(YFT_model2, test, n.trees = YFT_model2$gbm.call$best.trees, type = "response") # predict to test

dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
.roc(test$abundance_presence, preds) # Get testing AUC

test_tmp <- test %>% 
  dplyr::mutate(model = preds)

#### January-March
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jan-mar", # season
                        YFT_predict_season1, # rest of the ocean cells
                        YFT_model2, # BRT model
                        `grid_YFT_jan-mar` # grid of species for specific season
)

ggseason1 <- plotModel(gg[[1]]) + # Plot the model
  ggtitle("January-March")

#### April-June
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "apr-jun", # season
                        YFT_predict_season2, # rest of the ocean cells
                        YFT_model2, # BRT model
                        `grid_YFT_apr-jun` # grid of species for specific season
)

ggseason2 <- plotModel(gg[[1]]) + # Plot the model
  ggtitle("April-June")

#### July-September
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jul-sept", # season
                        YFT_predict_season3, # rest of the ocean cells
                        YFT_model2, # BRT model
                        `grid_YFT_jul-sept` # grid of species for specific season
)

ggseason3 <- plotModel(gg[[1]]) + # Plot the model
  ggtitle("July-September")

#### October-December
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "oct-dec", # season
                        YFT_predict_season4, # rest of the ocean cells
                        YFT_model2, # BRT model
                        `grid_YFT_oct-dec` # grid of species for specific season
)

ggseason4 <- plotModel(gg[[1]]) + # Plot the model
  ggtitle("October-December")

#### Arrange seasonal plots
ggseasons <- (ggseason1 + ggseason2) / (ggseason3 + ggseason4) + 
  plot_layout(guides = "collect") +
  plot_annotation("Model 2: Longitude (AUC: 0.75)")

ggsave(plot = ggseasons, filename = "Figures/YFT/YFT_Model2.png", width = 27, height = 15, dpi = 600)

###########################
## Model 3: Latitude ##
###########################

YFT_model3 <- dismo::gbm.step(data = train, gbm.x = c(8:20),
                              gbm.y = 5, family = "bernoulli", n.folds = 5)
saveRDS(YFT_model3, "Output/Models/YFT_model3.rds") # save the model
#YFT_model3 <- readRDS("Output/Models/YFT_model3.rds") # load the model

summary(YFT_model3) # get the relative importance of each of the predictors

# AUC Scores
YFT_model3$self.statistics$discrimination # Training AUC Score
YFT_model3$cv.statistics$discrimination.mean # Validation AUC Score

##### Plotting ##### 
train_tmp <- train %>% 
  dplyr::mutate(model = YFT_model3$fitted)
preds <- gbm::predict.gbm(YFT_model3, test, n.trees = YFT_model3$gbm.call$best.trees, type = "response") # predict to test

dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
.roc(test$abundance_presence, preds) # Get testing AUC

test_tmp <- test %>% 
  dplyr::mutate(model = preds)

#### January-March
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jan-mar", # season
                        YFT_predict_season1, # rest of the ocean cells
                        YFT_model3, # BRT model
                        `grid_YFT_jan-mar` # grid of species for specific season
)

ggseason1 <- plotModel(gg[[1]]) + # Plot the model
  ggtitle("January-March")

#### April-June
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "apr-jun", # season
                        YFT_predict_season2, # rest of the ocean cells
                        YFT_model3, # BRT model
                        `grid_YFT_apr-jun` # grid of species for specific season
)

ggseason2 <- plotModel(gg[[1]]) + # Plot the model
  ggtitle("April-June")

#### July-September
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jul-sept", # season
                        YFT_predict_season3, # rest of the ocean cells
                        YFT_model3, # BRT model
                        `grid_YFT_jul-sept` # grid of species for specific season
)

ggseason3 <- plotModel(gg[[1]]) + # Plot the model
  ggtitle("July-September")

#### October-December
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "oct-dec", # season
                        YFT_predict_season4, # rest of the ocean cells
                        YFT_model3, # BRT model
                        `grid_YFT_oct-dec` # grid of species for specific season
)

ggseason4 <- plotModel(gg[[1]]) + # Plot the model
  ggtitle("October-December")

#### Arrange seasonal plots
ggseasons <- (ggseason1 + ggseason2) / (ggseason3 + ggseason4) + 
  plot_layout(guides = "collect") +
  plot_annotation("Model 3: Latitude (AUC: 0.76)")

ggsave(plot = ggseasons, filename = "Figures/YFT/YFT_Model3.png", width = 27, height = 15, dpi = 600)

#####################################
## Model 4: Longitude and Latitude ##
#####################################
YFT_model4 <- dismo::gbm.step(data = train, gbm.x = c(7:20),
                              gbm.y = 5, family = "bernoulli", n.folds = 5)
saveRDS(YFT_model4, "Output/Models/YFT_model4.rds") # save the model
#YFT_model4 <- readRDS("Output/Models/YFT_model4.rds") # load the model

summary(YFT_model4) # get the relative importance of each of the predictors

# AUC Scores
YFT_model4$self.statistics$discrimination # Testing AUC Score
YFT_model4$cv.statistics$discrimination.mean # Validation AUC Score

##### Plotting ##### 
train_tmp <- train %>% 
  dplyr::mutate(model = YFT_model4$fitted)
preds <- gbm::predict.gbm(YFT_model4, test, n.trees = YFT_model4$gbm.call$best.trees, type = "response") # predict to test

dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
.roc(test$abundance_presence, preds) # Get testing AUC

test_tmp <- test %>% 
  dplyr::mutate(model = preds)

#### January-March
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jan-mar", # season
                        YFT_predict_season1, # rest of the ocean cells
                        YFT_model4, # BRT model
                        `grid_YFT_jan-mar` # grid of species for specific season
)

ggseason1 <- plotModel(gg[[1]]) + # Plot the model
  ggtitle("January-March")

#### April-June
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "apr-jun", # season
                        YFT_predict_season2, # rest of the ocean cells
                        YFT_model4, # BRT model
                        `grid_YFT_apr-jun` # grid of species for specific season
)

ggseason2 <- plotModel(gg[[1]]) + # Plot the model
  ggtitle("April-June")

#### July-September
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jul-sept", # season
                        YFT_predict_season3, # rest of the ocean cells
                        YFT_model4, # BRT model
                        `grid_YFT_jul-sept` # grid of species for specific season
)

ggseason3 <- plotModel(gg[[1]]) + # Plot the model
  ggtitle("July-September")

#### October-December
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "oct-dec", # season
                        YFT_predict_season4, # rest of the ocean cells
                        YFT_model4, # BRT model
                        `grid_YFT_oct-dec` # grid of species for specific season
)

ggseason4 <- plotModel(gg[[1]]) + # Plot the model
  ggtitle("October-December")

#### Arrange seasonal plots
ggseasons <- (ggseason1 + ggseason2) / (ggseason3 + ggseason4) + 
  plot_layout(guides = "collect") +
  plot_annotation("Model 4: Longitude and Latitude (AUC: 0.77)")

ggsave(plot = ggseasons, filename = "Figures/YFT/YFT_Model4.png", width = 27, height = 15, dpi = 600)
