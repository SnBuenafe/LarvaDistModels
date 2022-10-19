# Load preliminaries
source("00_Utils.R")
# Load dfs
source("01a_DataLayers_Assembling.R")

# Load albacore full dataset
SWO_ds <- read_csv("Output/SWO_full.csv", show_col_types = FALSE)

# Always build models with known data only
SWO_filtered <- SWO_ds %>% 
  dplyr::filter(!is.na(abundance)) %>% # filter those with data!
  dplyr::mutate(across(where(is.character), ~factor(.))) %>% # convert all characters to factors
  dplyr::mutate(abundance_presence = case_when(abundance > 0 ~ 1,
                                               abundance == 0 ~ 0)) %>%  # mutate the abundance data into 1s and 0s
  dplyr::select(-geometry) %>% 
  as.data.frame() #gbm.step doesn't work if it's a tibble...

# Data.frame for predictions
SWO_predict <- SWO_ds %>% 
  dplyr::filter(is.na(abundance)) %>% # filter those with data!
  dplyr::mutate(across(where(is.character), ~factor(.))) %>% # convert all characters to factors
  dplyr::mutate(abundance_presence = case_when(abundance > 0 ~ 1,
                                               abundance == 0 ~ 0)) %>%  # mutate the abundance data into 1s and 0s
  dplyr::select(-geometry) %>% 
  as.data.frame() #gbm.step doesn't work if it's a tibble...

#### Build the models ####
# check the index numbers of the columns
colnames(SWO_filtered)

#### Model 0 ####
# Using defaults for hyperparameters but using 5-fold cross-validation
SWO_model0 <- dismo::gbm.step(data = SWO_filtered, gbm.x = 4:12,
                              gbm.y = 13, family = "bernoulli", tree.complexity = 1,
                              learning.rate = 0.01, bag.fraction = 0.75, n.folds = 5)
saveRDS(SWO_model0, "Output/SWO_model0.rds") # save the model
SWO_model0 <- readRDS("Output/SWO_model0.rds") # load the model

summary(SWO_model0) # get the relative importance of each of the predictors

# Plot predictors
pdf(file = "Figures/SWO_Model0_SmoothPredictors.pdf", width = 10, height = 8)
gbm.plot(SWO_model0, n.plots=11, plot.layout=c(3, 3), write.title = FALSE)
dev.off()

pdf(file = "Figures/SWO_Model0_Predictors.pdf", width = 10, height = 8)
gbm.plot.fits(SWO_model0)
dev.off()

# Check this for ROC metrics: https://stackoverflow.com/questions/22391547/roc-score-in-gbm-package
# CV AUC of ROC (Receiver Operating Characteristic Curve)
SWO_model0$cv.statistics$discrimination.mean # AUC Score

# Calculate R2: https://stats.stackexchange.com/questions/76997/measure-the-goodness-of-fit-in-boosted-regression-tree
y_new <- SWO_filtered$abundance_presence
num <- var((SWO_model0$fitted)-y_new)
den <- var(y_new)
R2 <- 1-(num/den)
R2

# Check the plot
SWO_filtered$model <- SWO_model0$fitted
SWO_sf <- grid %>% # convert to sf so we can plot
  dplyr::left_join(SWO_filtered, ., by = "cellID") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

plotModel(SWO_sf, "Figures/SWO_Model0.png") # Plot the model
plotAbundance(SWO_sf, "Figures/SWO_abundance.png") # Plot raw
plotPA(SWO_sf, "Figures/SWO_presabs.png") # Plot presence absence

### Predict for the other points
preds <- dismo::predict(SWO_model0, SWO_predict, n.trees = SWO_model0$gbm.call$best.trees, type = "response")
SWO_predict$predictions <- preds

SWO_predict_sf <- grid_SWO %>% # convert to sf so we can plot
  dplyr::select(-species, -abundance, -season, -longitude, -latitude) %>% 
  dplyr::left_join(SWO_predict, ., by = "cellID") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

plotPredictions(SWO_predict_sf, "Figures/SWO_Model0preds.png") # Plot predictions

#### Model 1 ####
# model 1: matching hyperparameters of Cerasoli et al. (2017)
SWO_model1 <- dismo::gbm.step(data = SWO_filtered, gbm.x = 4:12,
                              gbm.y = 13, family = "bernoulli",
                              learning.rate = 0.001,
                              tree.complexity = 5,
                              n.folds = 5)
saveRDS(SWO_model1, "Output/SWO_model1.rds") # save the model
SWO_model1 <- readRDS("Output/SWO_model1.rds") # load the model

summary(SWO_model1) # get the relative importance of each of the predictors

# Plot predictors
pdf(file = "Figures/SWO_Model1_SmoothPredictors.pdf", width = 10, height = 8)
gbm.plot(SWO_model1, n.plots=9, plot.layout=c(3, 3), write.title = FALSE)
dev.off()

pdf(file = "Figures/SWO_Model1_Predictors.pdf", width = 10, height = 8)
gbm.plot.fits(SWO_model1)
dev.off()

# Check this for ROC metrics: https://stackoverflow.com/questions/22391547/roc-score-in-gbm-package
# CV AUC of ROC (Receiver Operating Characteristic Curve)
SWO_model1$cv.statistics$discrimination.mean # AUC Score

# Calculate R2: https://stats.stackexchange.com/questions/76997/measure-the-goodness-of-fit-in-boosted-regression-tree
y_new <- SWO_filtered$abundance_presence
num <- var((SWO_model1$fitted)-y_new)
den <- var(y_new)
R2 <- 1-(num/den)
R2

# Check the plot
SWO_filtered$model <- SWO_model1$fitted
SWO_sf <- grid %>% # convert to sf so we can plot
  dplyr::left_join(SWO_filtered, ., by = "cellID") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

plotModel(SWO_sf, "Figures/SWO_Model1.png") # Plot the model

### Predict for the other points
preds <- dismo::predict(SWO_model1, SWO_predict, n.trees = SWO_model1$gbm.call$best.trees, type = "response")
SWO_predict$predictions <- preds

SWO_predict_sf <- grid_SWO %>% # convert to sf so we can plot
  dplyr::select(-species, -abundance, -season, -longitude, -latitude) %>% 
  dplyr::left_join(SWO_predict, ., by = "cellID") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

plotPredictions(SWO_predict_sf, "Figures/SWO_Model1preds.png") # Plot predictions

#### Model 2 ####
# model 2: Try to match python:
# Learning rate: 0.1; max.trees = 1000; n.trees = 1
# Tree complexity: 3 (trying 3-way interactions)
SWO_model2 <- dismo::gbm.step(data = SWO_filtered, gbm.x = 4:12,
                              gbm.y = 13, family = "bernoulli",
                              learning.rate = 0.1,
                              tree.complexity = 3,
                              max.trees = 1000,
                              n.trees = 1,
                              n.folds = 5)
saveRDS(SWO_model2, "Output/SWO_model2.rds") # save the model
SWO_model2 <- readRDS("Output/SWO_model2.rds") # load the model

# Plot predictors
pdf(file = "Figures/SWO_Model2_SmoothPredictors.pdf", width = 10, height = 8)
gbm.plot(SWO_model2, n.plots=9, plot.layout=c(3, 3), write.title = FALSE)
dev.off()

pdf(file = "Figures/SWO_Model2_Predictors.pdf", width = 10, height = 8)
gbm.plot.fits(SWO_model2)
dev.off()

# Check this for ROC metrics: https://stackoverflow.com/questions/22391547/roc-score-in-gbm-package
# CV AUC of ROC (Receiver Operating Characteristic Curve)
SWO_model2$cv.statistics$discrimination.mean # AUC Score

# Calculate R2: https://stats.stackexchange.com/questions/76997/measure-the-goodness-of-fit-in-boosted-regression-tree
y_new <- SWO_filtered$abundance_presence
num <- var((SWO_model2$fitted)-y_new)
den <- var(y_new)
R2 <- 1-(num/den)
R2

# Check the plot
SWO_filtered$model <- SWO_model2$fitted
SWO_sf <- grid %>% # convert to sf so we can plot
  dplyr::left_join(SWO_filtered, ., by = "cellID") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

plotModel(SWO_sf, "Figures/SWO_Model2.png") # Plot the model

### Predict for the other points
preds <- dismo::predict(SWO_model2, SWO_predict, n.trees = SWO_model2$gbm.call$best.trees, type = "response")
SWO_predict$predictions <- preds

SWO_predict_sf <- grid_SWO %>% # convert to sf so we can plot
  dplyr::select(-species, -abundance, -season, -longitude, -latitude) %>% 
  dplyr::left_join(SWO_predict, ., by = "cellID") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

plotPredictions(SWO_predict_sf, "Figures/SWO_Model2preds.png") # Plot predictions

