# DESCRIPTION: Feature selection

###########################
## Load preliminaries ##
###########################
# Load dfs
source("08a_FRI_Data.R")

###############################
## Model 1: Restricted plots ##
###############################

FRI_model1 <- dismo::gbm.step(data = train, gbm.x = c(7:21, 23:24),
                               gbm.y = 5, family = "bernoulli", n.folds = 5)
saveRDS(FRI_model1, "Output/Models/FRI_model1.rds") # save the model
#FRI_model1 <- readRDS("Output/Models/FRI_model1.rds") # load the model

summary(FRI_model1) # get the relative importance of each of the predictors

# AUCs
FRI_model1$self.statistics$discrimination # Training AUC Score
FRI_model1$cv.statistics$discrimination.mean # Validation AUC Score

##### Plotting ##### 
train_tmp <- train %>% 
  dplyr::mutate(model = FRI_model1$fitted)
preds <- gbm::predict.gbm(FRI_model1, test, n.trees = FRI_model1$gbm.call$best.trees, type = "response") # predict to test

dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
.roc(test$abundance_presence, preds) # Get testing AUC

test_tmp <- test %>% 
  dplyr::mutate(model = preds)

#### January-March
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jan-mar", # season
                        FRI_predict_season1 %>% dplyr::filter(latitude >= min(FRI_build$latitude) & latitude <= max(FRI_build$latitude)), # rest of the ocean cells
                        FRI_model1, # BRT model
                        `grid_FRI_jan-mar` %>% dplyr::filter(latitude >= min(FRI_build$latitude) & latitude <= max(FRI_build$latitude)) # grid of species for specific season
)

ggseason1 <- plotModel(gg[[1]]) + # Plot the model
  ggtitle("January-March")

#### April-June
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "apr-jun", # season
                        FRI_predict_season2 %>% dplyr::filter(latitude >= min(FRI_build$latitude) & latitude <= max(FRI_build$latitude)), # rest of the ocean cells
                        FRI_model1, # BRT model
                        `grid_FRI_apr-jun` %>% dplyr::filter(latitude >= min(FRI_build$latitude) & latitude <= max(FRI_build$latitude)) # grid of species for specific season
)

ggseason2 <- plotModel(gg[[1]]) + # Plot the model
  ggtitle("April-June")

#### July-September
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jul-sept", # season
                        FRI_predict_season3 %>% dplyr::filter(latitude >= min(FRI_build$latitude) & latitude <= max(FRI_build$latitude)), # rest of the ocean cells
                        FRI_model1, # BRT model
                        `grid_FRI_jul-sept` %>% dplyr::filter(latitude >= min(FRI_build$latitude) & latitude <= max(FRI_build$latitude)) # grid of species for specific season
)

ggseason3 <- plotModel(gg[[1]]) + # Plot the model
  ggtitle("July-September")

#### October-December
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "oct-dec", # season
                        FRI_predict_season4 %>% dplyr::filter(latitude >= min(FRI_build$latitude) & latitude <= max(FRI_build$latitude)), # rest of the ocean cells
                        FRI_model1, # BRT model
                        `grid_FRI_oct-dec` %>% dplyr::filter(latitude >= min(FRI_build$latitude) & latitude <= max(FRI_build$latitude)) # grid of species for specific season
)

ggseason4 <- plotModel(gg[[1]]) + # Plot the model
  ggtitle("October-December")

#### Arrange seasonal plots
ggseasons <- (ggseason1 + ggseason2) / (ggseason3 + ggseason4) + 
  plot_annotation("Model 1: Additional Predictors (AUC: 0.91)")

ggsave(plot = ggseasons, filename = "Figures/FRI/FRI_model1.png", width = 27, height = 15, dpi = 600)
