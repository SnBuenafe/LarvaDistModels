# DESCRIPTION: Feature Selection with a restricted range (min and max latitudes)

###########################
## Load preliminaries ##
###########################
# Load YFT data
source("02a_YFTData.R")

#################################################
## Model 4: Restricted plots (All predictors) ##
#################################################
# Load the model 4
YFT_model4 <- readRDS("Output/Models/YFT_model4.rds")

# What are the min and max latitudes?
min(YFT_build$latitude)
max(YFT_build$latitude)

train_tmp <- train %>% 
  dplyr::mutate(model = YFT_model4$fitted)
preds <- gbm::predict.gbm(YFT_model4, test, n.trees = YFT_model4$gbm.call$best.trees, type = "response") # predict to test
test_tmp <- test %>% 
  dplyr::mutate(model = preds)

#### January-March
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jan-mar", # season
                        YFT_predict_season1 %>% dplyr::filter(latitude >= min(YFT_build$latitude) & latitude <= max(train$latitude)), # rest of the ocean cells
                        YFT_model4, # BRT model
                        `grid_YFT_jan-mar` %>% dplyr::filter(latitude >= min(YFT_build$latitude) & latitude <= max(train$latitude)) # grid of species for specific season with restricted ranges
)

ggseason1 <- plotModel(gg[[1]]) + # Plot the model
  ggtitle("January-March")

#### April-June
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "apr-jun", # season
                        YFT_predict_season2 %>% dplyr::filter(latitude >= min(YFT_build$latitude) & latitude <= max(YFT_build$latitude)), # rest of the ocean cells
                        YFT_model4, # BRT model
                        `grid_YFT_apr-jun` %>% dplyr::filter(latitude >= min(YFT_build$latitude) & latitude <= max(YFT_build$latitude))# grid of species for specific season
)

ggseason2 <- plotModel(gg[[1]]) + # Plot the model
  ggtitle("April-June")

#### July-September
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jul-sept", # season
                        YFT_predict_season3 %>% dplyr::filter(latitude >= min(YFT_build$latitude) & latitude <= max(YFT_build$latitude)), # rest of the ocean cells
                        YFT_model4, # BRT model
                        `grid_YFT_jul-sept` %>% dplyr::filter(latitude >= min(YFT_build$latitude) & latitude <= max(YFT_build$latitude)) # grid of species for specific season
)

ggseason3 <- plotModel(gg[[1]]) + # Plot the model
  ggtitle("July-September")

#### October-December
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "oct-dec", # season
                        YFT_predict_season4 %>% dplyr::filter(latitude >= min(YFT_build$latitude) & latitude <= max(YFT_build$latitude)), # rest of the ocean cells
                        YFT_model4, # BRT model
                        `grid_YFT_oct-dec` %>% dplyr::filter(latitude >= min(YFT_build$latitude) & latitude <= max(YFT_build$latitude))# grid of species for specific season
)

ggseason4 <- plotModel(gg[[1]]) + # Plot the model
  ggtitle("October-December")

ggseasons <- (ggseason1 + ggseason2) / (ggseason3 + ggseason4) + 
  plot_layout(guides = "collect") +
  plot_annotation("Model 4: New Predictors (AUC: 0.77)")

ggsave(plot = ggseasons, filename = "Figures/YFT/YFT_Model4a.png", width = 27, height = 15, dpi = 600)

#########################
## Model 5: Restricted ##
#########################

YFT_model5 <- dismo::gbm.step(data = train, gbm.x = c(7:13, 19:20),
                              gbm.y = 5, family = "bernoulli", n.folds = 5)
saveRDS(YFT_model5, "Output/Models/YFT_model5.rds") # save the model
#YFT_model5 <- readRDS("Output/Models/YFT_model5.rds") # load the model

summary(YFT_model5) # get the relative importance of each of the predictors

# AUCs
YFT_model5$self.statistics$discrimination # Training AUC Score
YFT_model5$cv.statistics$discrimination.mean # Validation AUC Score

##### Plotting ##### 
train_tmp <- train %>% 
  dplyr::mutate(model = YFT_model5$fitted)
preds <- gbm::predict.gbm(YFT_model5, test, n.trees = YFT_model5$gbm.call$best.trees, type = "response") # predict to test

dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
.roc(test$abundance_presence, preds) # Get testing AUC

test_tmp <- test %>% 
  dplyr::mutate(model = preds)

#### January-March
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jan-mar", # season
                        YFT_predict_season1 %>% dplyr::filter(latitude >= min(YFT_build$latitude) & latitude <= max(YFT_build$latitude)), # rest of the ocean cells
                        YFT_model5, # BRT model
                        `grid_YFT_jan-mar` %>% dplyr::filter(latitude >= min(YFT_build$latitude) & latitude <= max(YFT_build$latitude)) # grid of species for specific season
)

ggseason1 <- plotModel(gg[[1]]) + # Plot the model
  ggtitle("January-March")

#### April-June
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "apr-jun", # season
                        YFT_predict_season2 %>% dplyr::filter(latitude >= min(YFT_build$latitude) & latitude <= max(YFT_build$latitude)), # rest of the ocean cells
                        YFT_model5, # BRT model
                        `grid_YFT_apr-jun` %>% dplyr::filter(latitude >= min(YFT_build$latitude) & latitude <= max(YFT_build$latitude)) # grid of species for specific season
)

ggseason2 <- plotModel(gg[[1]]) + # Plot the model
  ggtitle("April-June")

#### July-September
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jul-sept", # season
                        YFT_predict_season3 %>% dplyr::filter(latitude >= min(YFT_build$latitude) & latitude <= max(YFT_build$latitude)), # rest of the ocean cells
                        YFT_model5, # BRT model
                        `grid_YFT_jul-sept` %>% dplyr::filter(latitude >= min(YFT_build$latitude) & latitude <= max(YFT_build$latitude)) # grid of species for specific season
)

ggseason3 <- plotModel(gg[[1]]) + # Plot the model
  ggtitle("July-September")

#### October-December
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "oct-dec", # season
                        YFT_predict_season4 %>% dplyr::filter(latitude >= min(YFT_build$latitude) & latitude <= max(YFT_build$latitude)), # rest of the ocean cells
                        YFT_model5, # BRT model
                        `grid_YFT_oct-dec` %>% dplyr::filter(latitude >= min(YFT_build$latitude) & latitude <= max(YFT_build$latitude)) # grid of species for specific season
)

ggseason4 <- plotModel(gg[[1]]) + # Plot the model
  ggtitle("October-December")

#### Arrange seasonal plots
ggseasons <- (ggseason1 + ggseason2) / (ggseason3 + ggseason4) + 
  plot_layout(guides = "collect") +
  plot_annotation("Model 5: Original Predictors (AUC: 0.77)")

ggsave(plot = ggseasons, filename = "Figures/YFT/YFT_Model5.png", width = 27, height = 15, dpi = 600)
