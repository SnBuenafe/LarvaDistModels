# DESCRIPTION: Feature selection

###########################
## Load preliminaries ##
###########################
# Load dfs
source("04a_ALBData.R")

#####################################################
## Model 1: Restricted plots (Original predictors) ##
#####################################################

ALB_model1 <- dismo::gbm.step(data = train, gbm.x = c(7:13, 19:20),
                              gbm.y = 5, family = "bernoulli", n.folds = 5)
saveRDS(ALB_model1, "Output/Models/ALB_model1.rds") # save the model
#ALB_model1 <- readRDS("Output/Models/ALB_model1.rds") # load the model

summary(ALB_model1) # get the relative importance of each of the predictors

# AUCs
ALB_model1$self.statistics$discrimination # Training AUC Score
ALB_model1$cv.statistics$discrimination.mean # Validation AUC Score

##### Plotting ##### 
train_tmp <- train %>% 
  dplyr::mutate(model = ALB_model1$fitted)
preds <- gbm::predict.gbm(ALB_model1, test, n.trees = ALB_model1$gbm.call$best.trees, type = "response") # predict to test

dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
.roc(test$abundance_presence, preds) # Get testing AUC

test_tmp <- test %>% 
  dplyr::mutate(model = preds)

#### January-March
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jan-mar", # season
                        ALB_predict_season1 %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)), # rest of the ocean cells
                        ALB_model1, # BRT model
                        `grid_ALB_jan-mar` %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)) # grid of species for specific season
)

ggseason1 <- plotModel(gg[[1]], gg[[2]]) + # Plot the model
  ggtitle("January-March")

#### April-June
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "apr-jun", # season
                        ALB_predict_season2 %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)), # rest of the ocean cells
                        ALB_model1, # BRT model
                        `grid_ALB_apr-jun` %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)) # grid of species for specific season
)

ggseason2 <- plotModel(gg[[1]], gg[[2]]) + # Plot the model
  ggtitle("April-June")

#### July-September
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jul-sept", # season
                        ALB_predict_season3 %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)), # rest of the ocean cells
                        ALB_model1, # BRT model
                        `grid_ALB_jul-sept` %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)) # grid of species for specific season
)

ggseason3 <- plotModel(gg[[1]], gg[[2]]) + # Plot the model
  ggtitle("July-September")

#### October-December
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "oct-dec", # season
                        ALB_predict_season4 %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)), # rest of the ocean cells
                        ALB_model1, # BRT model
                        `grid_ALB_oct-dec` %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)) # grid of species for specific season
)

ggseason4 <- plotModel(gg[[1]], gg[[2]]) + # Plot the model
  ggtitle("October-December")

#### Arrange seasonal plots
ggseasons <- (ggseason1 + ggseason2) / (ggseason3 + ggseason4) + 
  plot_layout(guides = "collect") +
  plot_annotation("Model 1: Original Predictors (AUC: 0.87)")

ggsave(plot = ggseasons, filename = "Figures/ALB/ALB_Model1.png", width = 27, height = 15, dpi = 600)

#######################################################
## Model 2: Restricted plots (Additional predictors) ##
#######################################################

ALB_model2 <- dismo::gbm.step(data = train, gbm.x = c(7:20),
                              gbm.y = 5, family = "bernoulli", n.folds = 5)
saveRDS(ALB_model2, "Output/Models/ALB_model2.rds") # save the model
#ALB_model2 <- readRDS("Output/Models/ALB_model2.rds") # load the model

summary(ALB_model2) # get the relative importance of each of the predictors

# AUCs
ALB_model2$self.statistics$discrimination # Training AUC Score
ALB_model2$cv.statistics$discrimination.mean # Validation AUC Score

##### Plotting ##### 
train_tmp <- train %>% 
  dplyr::mutate(model = ALB_model2$fitted)
preds <- gbm::predict.gbm(ALB_model2, test, n.trees = ALB_model2$gbm.call$best.trees, type = "response") # predict to test

dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
.roc(test$abundance_presence, preds) # Get testing AUC

test_tmp <- test %>% 
  dplyr::mutate(model = preds)

#### January-March
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jan-mar", # season
                        ALB_predict_season1 %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)), # rest of the ocean cells
                        ALB_model2, # BRT model
                        `grid_ALB_jan-mar` %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)) # grid of species for specific season
)

ggseason1 <- plotModel(gg[[1]], gg[[2]]) + # Plot the model
  ggtitle("January-March")

#### April-June
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "apr-jun", # season
                        ALB_predict_season2 %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)), # rest of the ocean cells
                        ALB_model2, # BRT model
                        `grid_ALB_apr-jun` %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)) # grid of species for specific season
)

ggseason2 <- plotModel(gg[[1]], gg[[2]]) + # Plot the model
  ggtitle("April-June")

#### July-September
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jul-sept", # season
                        ALB_predict_season3 %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)), # rest of the ocean cells
                        ALB_model2, # BRT model
                        `grid_ALB_jul-sept` %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)) # grid of species for specific season
)

ggseason3 <- plotModel(gg[[1]], gg[[2]]) + # Plot the model
  ggtitle("July-September")

#### October-December
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "oct-dec", # season
                        ALB_predict_season4 %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)), # rest of the ocean cells
                        ALB_model2, # BRT model
                        `grid_ALB_oct-dec` %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)) # grid of species for specific season
)

ggseason4 <- plotModel(gg[[1]], gg[[2]]) + # Plot the model
  ggtitle("October-December")

#### Arrange seasonal plots
ggseasons <- (ggseason1 + ggseason2) / (ggseason3 + ggseason4) + 
  plot_layout(guides = "collect") +
  plot_annotation("Model 2: Additional Predictors (AUC: 0.87)")

ggsave(plot = ggseasons, filename = "Figures/ALB/ALB_Model2.png", width = 27, height = 15, dpi = 600)
