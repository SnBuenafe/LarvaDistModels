###########################
## Load preliminaries ##
###########################
# Load SAIL Data
source("12a_SAIL_Data.R")

#######################################################
## Model 1: Restricted plots (Additional predictors) ##
#######################################################

SAIL_model1 <- dismo::gbm.step(data = train, gbm.x = c(7:20),
                              gbm.y = 5, family = "bernoulli", n.folds = 5)
saveRDS(SAIL_model1, "Output/Models/SAIL_model1.rds") # save the model
#SAIL_model1 <- readRDS("Output/Models/SAIL_model1.rds") # load the model

summary(SAIL_model1) # get the relative importance of each of the predictors

# AUCs
SAIL_model1$self.statistics$discrimination # Training AUC Score
SAIL_model1$cv.statistics$discrimination.mean # Validation AUC Score

##### Plotting ##### 
train_tmp <- train %>% 
  dplyr::mutate(model = SAIL_model1$fitted)
preds <- gbm::predict.gbm(SAIL_model1, test, n.trees = SAIL_model1$gbm.call$best.trees, type = "response") # predict to test

dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
.roc(test$abundance_presence, preds) # Get testing AUC

test_tmp <- test %>% 
  dplyr::mutate(model = preds)

#### January-March
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jan-mar", # season
                        SAIL_predict_season1 %>% dplyr::filter(latitude >= min(SAIL_build$latitude) & latitude <= max(SAIL_build$latitude)), # rest of the ocean cells
                        SAIL_model1, # BRT model
                        `grid_SAIL_jan-mar` %>% dplyr::filter(latitude >= min(SAIL_build$latitude) & latitude <= max(SAIL_build$latitude)) # grid of species for specific season
)

ggseason1 <- plotModel(gg[[1]]) + # Plot the model
  ggtitle("January-March")

#### April-June
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "apr-jun", # season
                        SAIL_predict_season2 %>% dplyr::filter(latitude >= min(SAIL_build$latitude) & latitude <= max(SAIL_build$latitude)), # rest of the ocean cells
                        SAIL_model1, # BRT model
                        `grid_SAIL_apr-jun` %>% dplyr::filter(latitude >= min(SAIL_build$latitude) & latitude <= max(SAIL_build$latitude)) # grid of species for specific season
)

ggseason2 <- plotModel(gg[[1]]) + # Plot the model
  ggtitle("April-June")

#### July-September
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jul-sept", # season
                        SAIL_predict_season3 %>% dplyr::filter(latitude >= min(SAIL_build$latitude) & latitude <= max(SAIL_build$latitude)), # rest of the ocean cells
                        SAIL_model1, # BRT model
                        `grid_SAIL_jul-sept` %>% dplyr::filter(latitude >= min(SAIL_build$latitude) & latitude <= max(SAIL_build$latitude)) # grid of species for specific season
)

ggseason3 <- plotModel(gg[[1]]) + # Plot the model
  ggtitle("July-September")

#### October-December
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "oct-dec", # season
                        SAIL_predict_season4 %>% dplyr::filter(latitude >= min(SAIL_build$latitude) & latitude <= max(SAIL_build$latitude)), # rest of the ocean cells
                        SAIL_model1, # BRT model
                        `grid_SAIL_oct-dec` %>% dplyr::filter(latitude >= min(SAIL_build$latitude) & latitude <= max(SAIL_build$latitude)) # grid of species for specific season
)

ggseason4 <- plotModel(gg[[1]]) + # Plot the model
  ggtitle("October-December")

#### Arrange seasonal plots
ggseasons <- (ggseason1 + ggseason2) / (ggseason3 + ggseason4) + 
  plot_layout(guides = "collect") +
  plot_annotation("Model 1: Additional Predictors (AUC: 0.87)")

ggsave(plot = ggseasons, filename = "Figures/SAIL/SAIL_model1.png", width = 27, height = 15, dpi = 600)
