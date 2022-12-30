# DESCRIPTION: Feature selection

###########################
## Load preliminaries ##
###########################
# Load dfs
source("07a_SHOSData.R")

#######################################################
## Model 1: Restricted plots (Additional predictors) ##
#######################################################

SHOS_model1 <- dismo::gbm.step(data = train, gbm.x = c(7:20),
                               gbm.y = 5, family = "bernoulli", n.folds = 5)
saveRDS(SHOS_model1, "Output/Models/SHOS_model1.rds") # save the model
#SHOS_model1 <- readRDS("Output/Models/SHOS_model1.rds") # load the model

summary(SHOS_model1) # get the relative importance of each of the predictors

# AUCs
SHOS_model1$self.statistics$discrimination # Training AUC Score
SHOS_model1$cv.statistics$discrimination.mean # Validation AUC Score

##### Plotting ##### 
train_tmp <- train %>% 
  dplyr::mutate(model = SHOS_model1$fitted)
preds <- gbm::predict.gbm(SHOS_model1, test, n.trees = SHOS_model1$gbm.call$best.trees, type = "response") # predict to test

dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
.roc(test$abundance_presence, preds) # Get testing AUC

test_tmp <- test %>% 
  dplyr::mutate(model = preds)

#### January-March
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jan-mar", # season
                        SHOS_predict_season1 %>% dplyr::filter(latitude >= min(SHOS_build$latitude) & latitude <= max(SHOS_build$latitude)), # rest of the ocean cells
                        SHOS_model1, # BRT model
                        `grid_SHOS_jan-mar` %>% dplyr::filter(latitude >= min(SHOS_build$latitude) & latitude <= max(SHOS_build$latitude)) # grid of species for specific season
)

ggseason1 <- plotModel(gg[[1]]) + # Plot the model
  ggtitle("January-March")

#### April-June
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "apr-jun", # season
                        SHOS_predict_season2 %>% dplyr::filter(latitude >= min(SHOS_build$latitude) & latitude <= max(SHOS_build$latitude)), # rest of the ocean cells
                        SHOS_model1, # BRT model
                        `grid_SHOS_apr-jun` %>% dplyr::filter(latitude >= min(SHOS_build$latitude) & latitude <= max(SHOS_build$latitude)) # grid of species for specific season
)

ggseason2 <- plotModel(gg[[1]]) + # Plot the model
  ggtitle("April-June")

#### July-September
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jul-sept", # season
                        SHOS_predict_season3 %>% dplyr::filter(latitude >= min(SHOS_build$latitude) & latitude <= max(SHOS_build$latitude)), # rest of the ocean cells
                        SHOS_model1, # BRT model
                        `grid_SHOS_jul-sept` %>% dplyr::filter(latitude >= min(SHOS_build$latitude) & latitude <= max(SHOS_build$latitude)) # grid of species for specific season
)

ggseason3 <- plotModel(gg[[1]]) + # Plot the model
  ggtitle("July-September")

#### October-December
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "oct-dec", # season
                        SHOS_predict_season4 %>% dplyr::filter(latitude >= min(SHOS_build$latitude) & latitude <= max(SHOS_build$latitude)), # rest of the ocean cells
                        SHOS_model1, # BRT model
                        `grid_SHOS_oct-dec` %>% dplyr::filter(latitude >= min(SHOS_build$latitude) & latitude <= max(SHOS_build$latitude)) # grid of species for specific season
)

ggseason4 <- plotModel(gg[[1]]) + # Plot the model
  ggtitle("October-December")

#### Arrange seasonal plots
ggseasons <- (ggseason1 + ggseason2) / (ggseason3 + ggseason4) + 
  plot_layout(guides = "collect") +
  plot_annotation("Model 1: Additional Predictors (AUC: 0.82)")

ggsave(plot = ggseasons, filename = "Figures/SHOS/SHOS_model1.png", width = 27, height = 15, dpi = 600)
