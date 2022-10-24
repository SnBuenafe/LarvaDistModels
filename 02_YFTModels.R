###########################
## Load preliminaries ##
###########################
# Load preliminaries
source("00_Utils.R")
# Load dfs
source("01a_DataLayers_Assembling.R")

###########################
## Load dataset ##
###########################

# Load yellowfin tuna full dataset
YFT_ds <- read_csv("Output/YFT_full.csv", show_col_types = FALSE)

# Always build models with known data only
YFT_filtered <- YFT_ds %>% 
  dplyr::filter(!is.na(abundance)) %>% # filter those with data!
  dplyr::mutate(across(where(is.character), ~factor(.))) %>% # convert all characters to factors
  dplyr::mutate(abundance_presence = case_when(abundance > 0 ~ 1,
                                      abundance == 0 ~ 0)) %>%  # mutate the abundance data into 1s and 0s
  dplyr::select(-geometry) %>% 
  as.data.frame() #gbm.step doesn't work if it's a tibble...

# Data.frame for predictions
YFT_predict <- YFT_ds %>% 
  dplyr::filter(is.na(abundance)) %>% # filter those with data!
  dplyr::mutate(across(where(is.character), ~factor(.))) %>% # convert all characters to factors
  dplyr::mutate(abundance_presence = case_when(abundance > 0 ~ 1,
                                               abundance == 0 ~ 0)) %>%  # mutate the abundance data into 1s and 0s
  dplyr::select(-geometry) %>% 
  as.data.frame() #gbm.step doesn't work if it's a tibble...

###########################
## Model 0 ##
###########################
# Building models
# check the index numbers of the columns
#colnames(YFT_filtered)

# higher AUC better. A model with AUC values closer to 0 have more wrong predictions.
# see: https://rspatial.org/raster/sdm/9_sdm_brt.html for interpreting results

# Using ocean basins
YFT_model0 <- dismo::gbm.step(data = YFT_filtered, gbm.x = c(4, 7:13),
                              gbm.y = 14, family = "bernoulli", tree.complexity = 1,
                              learning.rate = 0.001, bag.fraction = 0.75, n.folds = 5)
saveRDS(YFT_model0, "Output/YFT_model0.rds") # save the model
YFT_model0 <- readRDS("Output/YFT_model0.rds") # load the model

summary(YFT_model0) # get the relative importance of each of the predictors

# Plot predictors
pdf(file = "Figures/YFT_Model0_SmoothPredictors.pdf", width = 10, height = 8)
gbm.plot(YFT_model0, n.plots=9, plot.layout=c(3, 3), write.title = FALSE)
dev.off()

pdf(file = "Figures/YFT_Model0_Predictors.pdf", width = 10, height = 8)
gbm.plot.fits(YFT_model0)
dev.off()

# Check this for ROC metrics: https://stackoverflow.com/questions/22391547/roc-score-in-gbm-package
# CV AUC of ROC (Receiver Operating Characteristic Curve)
YFT_model0$cv.statistics$discrimination.mean # AUC Score

# Calculate R2: https://stats.stackexchange.com/questions/76997/measure-the-goodness-of-fit-in-boosted-regression-tree
y_new <- YFT_filtered$abundance_presence
num <- var((YFT_model0$fitted)-y_new)
den <- var(y_new)
R2 <- 1-(num/den)
R2

# Check the plot
YFT_filtered$model <- YFT_model0$fitted
YFT_sf <- grid %>% # convert to sf so we can plot
  dplyr::left_join(YFT_filtered, ., by = "cellID") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

plotModel(YFT_sf, "Figures/YFT_Model0.png") # Plot the model
plotAbundance(YFT_sf, "Figures/YFT_abundance.png") # Plot raw
plotPA(YFT_sf, "Figures/YFT_presabs.png") # Plot presence absence

### Predict for the other points
preds <- dismo::predict(YFT_model0, YFT_predict, n.trees = YFT_model0$gbm.call$best.trees, type = "response")
YFT_predict$predictions <- preds

YFT_predict_sf <- grid_YFT %>% # convert to sf so we can plot
  dplyr::select(-species, -abundance, -season, -longitude, -latitude) %>% 
  dplyr::left_join(YFT_predict, ., by = "cellID") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

plotPredictions(YFT_predict_sf, "Figures/YFT_Model0preds.png") # Plot predictions

#### Model 1 ####
# model 1: no geographic location
YFT_model1 <- dismo::gbm.step(data = YFT_filtered, gbm.x = c(4,8:13),
                              gbm.y = 14, family = "bernoulli",
                              learning.rate = 0.001,
                              tree.complexity = 5,
                              n.folds = 5)
saveRDS(YFT_model1, "Output/YFT_model1.rds") # save the model
YFT_model1 <- readRDS("Output/YFT_model1.rds") # load the model

summary(YFT_model1) # get the relative importance of each of the predictors

# Plot predictors
pdf(file = "Figures/YFT_Model1_SmoothPredictors.pdf", width = 10, height = 8)
gbm.plot(YFT_model1, n.plots=7, plot.layout=c(3, 3), write.title = FALSE)
dev.off()

pdf(file = "Figures/YFT_Model1_Predictors.pdf", width = 10, height = 8)
gbm.plot.fits(YFT_model1)
dev.off()

# Check this for ROC metrics: https://stackoverflow.com/questions/22391547/roc-score-in-gbm-package
# CV AUC of ROC (Receiver Operating Characteristic Curve)
YFT_model1$cv.statistics$discrimination.mean # AUC Score

# Calculate R2: https://stats.stackexchange.com/questions/76997/measure-the-goodness-of-fit-in-boosted-regression-tree
y_new <- YFT_filtered$abundance_presence
num <- var((YFT_model1$fitted)-y_new)
den <- var(y_new)
R2 <- 1-(num/den)
R2

# Check the plot
YFT_filtered$model <- YFT_model1$fitted
YFT_sf <- grid %>% # convert to sf so we can plot
  dplyr::left_join(YFT_filtered, ., by = "cellID") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

plotModel(YFT_sf, "Figures/YFT_Model1.png") # Plot the model

### Predict for the other points
preds <- dismo::predict(YFT_model1, YFT_predict, n.trees = YFT_model1$gbm.call$best.trees, type = "response")
YFT_predict$predictions <- preds

YFT_predict_sf <- grid_YFT %>% # convert to sf so we can plot
  dplyr::select(-species, -abundance, -season, -longitude, -latitude) %>% 
  dplyr::left_join(YFT_predict, ., by = "cellID") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

plotPredictions(YFT_predict_sf, "Figures/YFT_Model1preds.png") # Plot predictions

#### Model 2 ####
# model 2: just latitude
YFT_model2 <- dismo::gbm.step(data = YFT_filtered, gbm.x = c(4, 6, 8:13),
                              gbm.y = 14, family = "bernoulli",
                              learning.rate = 0.001,
                              tree.complexity = 5,
                              n.folds = 5)
saveRDS(YFT_model2, "Output/YFT_model2.rds") # save the model
YFT_model2 <- readRDS("Output/YFT_model2.rds") # load the model

summary(YFT_model2) # get the relative importance of each of the predictors

# Plot predictors
pdf(file = "Figures/YFT_Model2_SmoothPredictors.pdf", width = 10, height = 8)
gbm.plot(YFT_model2, n.plots=8, plot.layout=c(3, 3), write.title = FALSE)
dev.off()

pdf(file = "Figures/YFT_Model2_Predictors.pdf", width = 10, height = 8)
gbm.plot.fits(YFT_model2)
dev.off()

# Check this for ROC metrics: https://stackoverflow.com/questions/22391547/roc-score-in-gbm-package
# CV AUC of ROC (Receiver Operating Characteristic Curve)
YFT_model2$cv.statistics$discrimination.mean # AUC Score

# Calculate R2: https://stats.stackexchange.com/questions/76997/measure-the-goodness-of-fit-in-boosted-regression-tree
y_new <- YFT_filtered$abundance_presence
num <- var((YFT_model2$fitted)-y_new)
den <- var(y_new)
R2 <- 1-(num/den)
R2

# Check the plot
YFT_filtered$model <- YFT_model2$fitted
YFT_sf <- grid %>% # convert to sf so we can plot
  dplyr::left_join(YFT_filtered, ., by = "cellID") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

plotModel(YFT_sf, "Figures/YFT_Model2.png") # Plot the model

### Predict for the other points
preds <- dismo::predict(YFT_model2, YFT_predict, n.trees = YFT_model2$gbm.call$best.trees, type = "response")
YFT_predict$predictions <- preds

YFT_predict_sf <- grid_YFT %>% # convert to sf so we can plot
  dplyr::select(-species, -abundance, -season, -longitude, -latitude) %>% 
  dplyr::left_join(YFT_predict, ., by = "cellID") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

plotPredictions(YFT_predict_sf, "Figures/YFT_Model2preds.png") # Plot predictions

### Model 3 ####
# model 3: just longitude
YFT_model3 <- dismo::gbm.step(data = YFT_filtered, gbm.x = c(4:5, 8:13),
                              gbm.y = 14, family = "bernoulli",
                              learning.rate = 0.001,
                              tree.complexity = 5,
                              n.folds = 5)
saveRDS(YFT_model3, "Output/YFT_model3.rds") # save the model
YFT_model3 <- readRDS("Output/YFT_model3.rds") # load the model

summary(YFT_model3) # get the relative importance of each of the predictors

# Plot predictors
pdf(file = "Figures/YFT_Model3_SmoothPredictors.pdf", width = 10, height = 8)
gbm.plot(YFT_model3, n.plots=8, plot.layout=c(3, 3), write.title = FALSE)
dev.off()

pdf(file = "Figures/YFT_Model3_Predictors.pdf", width = 10, height = 8)
gbm.plot.fits(YFT_model3)
dev.off()

# Check this for ROC metrics: https://stackoverflow.com/questions/22391547/roc-score-in-gbm-package
# CV AUC of ROC (Receiver Operating Characteristic Curve)
YFT_model3$cv.statistics$discrimination.mean # AUC Score

# Calculate R2: https://stats.stackexchange.com/questions/76997/measure-the-goodness-of-fit-in-boosted-regression-tree
y_new <- YFT_filtered$abundance_presence
num <- var((YFT_model3$fitted)-y_new)
den <- var(y_new)
R2 <- 1-(num/den)
R2

# Check the plot
YFT_filtered$model <- YFT_model3$fitted
YFT_sf <- grid %>% # convert to sf so we can plot
  dplyr::left_join(YFT_filtered, ., by = "cellID") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

plotModel(YFT_sf, "Figures/YFT_Model3.png") # Plot the model

### Predict for the other points
preds <- dismo::predict(YFT_model3, YFT_predict, n.trees = YFT_model3$gbm.call$best.trees, type = "response")
YFT_predict$predictions <- preds

YFT_predict_sf <- grid_YFT %>% # convert to sf so we can plot
  dplyr::select(-species, -abundance, -season, -longitude, -latitude) %>% 
  dplyr::left_join(YFT_predict, ., by = "cellID") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

plotPredictions(YFT_predict_sf, "Figures/YFT_Model3preds.png") # Plot predictions

### Model 4 ####
# model 4: longitude and latitude
YFT_model4 <- dismo::gbm.step(data = YFT_filtered, gbm.x = c(4:6, 8:13),
                              gbm.y = 14, family = "bernoulli",
                              learning.rate = 0.001,
                              tree.complexity = 5,
                              n.folds = 5)
saveRDS(YFT_model4, "Output/YFT_model4.rds") # save the model
YFT_model4 <- readRDS("Output/YFT_model4.rds") # load the model

summary(YFT_model4) # get the relative importance of each of the predictors

# Plot predictors
pdf(file = "Figures/YFT_Model4_SmoothPredictors.pdf", width = 10, height = 8)
gbm.plot(YFT_model4, n.plots=9, plot.layout=c(3, 3), write.title = FALSE)
dev.off()

pdf(file = "Figures/YFT_Model4_Predictors.pdf", width = 10, height = 8)
gbm.plot.fits(YFT_model4)
dev.off()

# Check this for ROC metrics: https://stackoverflow.com/questions/22391547/roc-score-in-gbm-package
# CV AUC of ROC (Receiver Operating Characteristic Curve)
YFT_model4$cv.statistics$discrimination.mean # AUC Score

# Calculate R2: https://stats.stackexchange.com/questions/76997/measure-the-goodness-of-fit-in-boosted-regression-tree
y_new <- YFT_filtered$abundance_presence
num <- var((YFT_model4$fitted)-y_new)
den <- var(y_new)
R2 <- 1-(num/den)
R2

# Check the plot
YFT_filtered$model <- YFT_model4$fitted
YFT_sf <- grid %>% # convert to sf so we can plot
  dplyr::left_join(YFT_filtered, ., by = "cellID") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

plotModel(YFT_sf, "Figures/YFT_Model4.png") # Plot the model

### Predict for the other points
preds <- dismo::predict(YFT_model4, YFT_predict, n.trees = YFT_model4$gbm.call$best.trees, type = "response")
YFT_predict$predictions <- preds

YFT_predict_sf <- grid_YFT %>% # convert to sf so we can plot
  dplyr::select(-species, -abundance, -season, -longitude, -latitude) %>% 
  dplyr::left_join(YFT_predict, ., by = "cellID") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

plotPredictions(YFT_predict_sf, "Figures/YFT_Model4preds.png") # Plot predictions

###########################
## Model 5 ##
###########################
# Do a cross-validated grid search using gbm.fixed
# define a 5-fold cross-validation
CVGrid <- CVgridSearch(YFT_filtered, 5, tc = c(1, 2, 3, 5), bf = c(0.5, 0.75), lr = 0.005)

print(CVGrid %>% dplyr::arrange(cv_deviance))
# constant learning rate of 0.005; we want >= 1000 trees (Cerasoli et al., 2017)

# Now build the model; we want to have a lot of trees: preferably at least 1,000 trees
YFT_model5 <- gbm.step(YFT_filtered, gbm.x = c(4, 6, 8:13), gbm.y = 14, 
                  learning.rate = 0.005,
                  bag.fraction = 0.5,
                  tree.complexity = 5
                  )
saveRDS(YFT_model5, "Output/YFT_model5.rds") # save the model
YFT_model5 <- readRDS("Output/YFT_model5.rds") # load the model

summary(YFT_model5) # get the relative importance of each of the predictors

# Plot predictors
pdf(file = "Figures/YFT_Model5_SmoothPredictors.pdf", width = 10, height = 8)
gbm.plot(YFT_model5, n.plots=8, plot.layout=c(3, 3), write.title = FALSE)
dev.off()

pdf(file = "Figures/YFT_Model5_Predictors.pdf", width = 10, height = 8)
gbm.plot.fits(YFT_model5)
dev.off()

# Check this for ROC metrics: https://stackoverflow.com/questions/22391547/roc-score-in-gbm-package
# CV AUC of ROC (Receiver Operating Characteristic Curve)
YFT_model5$cv.statistics$discrimination.mean # AUC Score

# Check the plot
YFT_filtered$model <- YFT_model5$fitted
YFT_sf <- grid %>% # convert to sf so we can plot
  dplyr::left_join(YFT_filtered, ., by = "cellID") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

plotModel(YFT_sf, "Figures/YFT_Model5.png") # Plot the model

### Predict for the other points
preds <- dismo::predict(YFT_model5, YFT_predict, n.trees = YFT_model5$gbm.call$best.trees, type = "response")
YFT_predict$predictions <- preds

YFT_predict_sf <- grid_YFT %>% # convert to sf so we can plot
  dplyr::select(-species, -abundance, -season, -longitude, -latitude) %>% 
  dplyr::left_join(YFT_predict, ., by = "cellID") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

plotPredictions(YFT_predict_sf, "Figures/YFT_Model5preds.png") # Plot predictions
