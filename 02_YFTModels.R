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
YFT_ds <- read_csv("Output/CSV/YFT_historical_full.csv", show_col_types = FALSE)

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
## Model 0: None ##
###########################
# Building models
# check the index numbers of the columns
colnames(YFT_filtered)

# higher AUC better. A model with AUC values closer to 0 have more wrong predictions.
# see: https://rspatial.org/raster/sdm/9_sdm_brt.html for interpreting results

YFT_model0 <- dismo::gbm.step(data = YFT_filtered, gbm.x = c(4, 8:13),
                              gbm.y = 14, family = "bernoulli", n.folds = 5)
saveRDS(YFT_model0, "Output/Models/YFT_model0.rds") # save the model
#YFT_model0 <- readRDS("Output/YFT_model0.rds") # load the model

summary(YFT_model0) # get the relative importance of each of the predictors

# Plot predictors
pdf(file = "Figures/YFT/YFT_Model0_SmoothPredictors.pdf", width = 10, height = 8)
gbm.plot(YFT_model0, n.plots=7, plot.layout=c(3, 3), write.title = FALSE)
dev.off()

pdf(file = "Figures/YFT/YFT_Model0_Predictors.pdf", width = 10, height = 8)
gbm.plot.fits(YFT_model0)
dev.off()

# Check this for ROC metrics: https://stackoverflow.com/questions/22391547/roc-score-in-gbm-package
# CV AUC of ROC (Receiver Operating Characteristic Curve)
YFT_model0$cv.statistics$discrimination.mean # AUC Score
# To calculate R2: https://stats.stackexchange.com/questions/76997/measure-the-goodness-of-fit-in-boosted-regression-tree

# Plot
YFT_filtered$model <- YFT_model0$fitted # fitted values for the model
preds <- dismo::predict(YFT_model0, YFT_predict, n.trees = YFT_model0$gbm.call$best.trees, type = "response") # Predicted values
YFT_predict$model <- preds

model <- dplyr::bind_rows(YFT_filtered, YFT_predict) %>% 
  dplyr::arrange(cellID) %>%  # Make sure everything is arranged by cellID
  dplyr::select(model) %>% 
  pull()

ggYFT <- grid_YFT %>% 
  dplyr::bind_cols(., model = model) %>% 
  dplyr::select(cellID, ocean, model, geometry) %>% 
  sf::st_as_sf(sf_column_name = "geometry")

ggYFT_abun <- grid_YFT %>%
  dplyr::mutate(abundance_presence = ifelse(abundance > 0, yes = as.factor(1), no = NA)) %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  sf::st_centroid()

ggmodel <- plotModel(ggYFT, ggYFT_abun) + # Plot the model
  ggtitle("Yellowfin tuna (1956-1981): No geographic location (AUC: 0.71)")
ggsave(plot = ggmodel, filename = "Figures/YFT/YFT_Model0.png", width = 15, height = 8, dpi = 300)

###########################
## Model 1: Oceans ##
###########################
YFT_model1 <- dismo::gbm.step(data = YFT_filtered, gbm.x = c(4,7:13),
                              gbm.y = 14, family = "bernoulli",
                              n.folds = 5)
saveRDS(YFT_model1, "Output/Models/YFT_model1.rds") # save the model
#YFT_model1 <- readRDS("Output/YFT_model1.rds") # load the model

summary(YFT_model1) # get the relative importance of each of the predictors

# Plot predictors
pdf(file = "Figures/YFT/YFT_Model1_SmoothPredictors.pdf", width = 10, height = 8)
gbm.plot(YFT_model1, n.plots=8, plot.layout=c(3, 3), write.title = FALSE)
dev.off()

pdf(file = "Figures/YFT/YFT_Model1_Predictors.pdf", width = 10, height = 8)
gbm.plot.fits(YFT_model1)
dev.off()

# Check this for ROC metrics: https://stackoverflow.com/questions/22391547/roc-score-in-gbm-package
# CV AUC of ROC (Receiver Operating Characteristic Curve)
YFT_model1$cv.statistics$discrimination.mean # AUC Score

YFT_filtered$model <- YFT_model1$fitted # fitted values for the model
preds <- dismo::predict(YFT_model1, YFT_predict, n.trees = YFT_model1$gbm.call$best.trees, type = "response") # Predicted values
YFT_predict$model <- preds

model <- dplyr::bind_rows(YFT_filtered, YFT_predict) %>% 
  dplyr::arrange(cellID) %>%  # Make sure everything is arranged by cellID
  dplyr::select(model) %>% 
  pull()

ggYFT <- grid_YFT %>% 
  dplyr::bind_cols(., model = model) %>% 
  dplyr::select(cellID, ocean, model, geometry) %>% 
  sf::st_as_sf(sf_column_name = "geometry")

ggmodel <- plotModel(ggYFT, ggYFT_abun) + # Plot the model
  ggtitle("Yellowfin tuna (1956-1981): With ocean basins (AUC: 0.72)")
ggsave(plot = ggmodel, filename = "Figures/YFT/YFT_Model1.png", width = 15, height = 8, dpi = 300)

###########################
## Model 2: Latitude ##
###########################
YFT_model2 <- dismo::gbm.step(data = YFT_filtered, gbm.x = c(4, 6, 8:13),
                              gbm.y = 14, family = "bernoulli",
                              learning.rate = 0.005,
                              bag.fraction = 0.5,
                              tree.complexity = 5,
                              n.folds = 5)
saveRDS(YFT_model2, "Output/Models/YFT_model2.rds") # save the model
#YFT_model2 <- readRDS("Output/Models/YFT_model2.rds") # load the model

summary(YFT_model2) # get the relative importance of each of the predictors

# Plot predictors
pdf(file = "Figures/YFT/YFT_Model2_SmoothPredictors.pdf", width = 10, height = 8)
gbm.plot(YFT_model2, n.plots=8, plot.layout=c(3, 3), write.title = FALSE)
dev.off()

pdf(file = "Figures/YFT/YFT_Model2_Predictors.pdf", width = 10, height = 8)
gbm.plot.fits(YFT_model2)
dev.off()

# Check this for ROC metrics: https://stackoverflow.com/questions/22391547/roc-score-in-gbm-package
# CV AUC of ROC (Receiver Operating Characteristic Curve)
YFT_model2$cv.statistics$discrimination.mean # AUC Score

YFT_filtered$model <- YFT_model2$fitted # fitted values for the model
preds <- dismo::predict(YFT_model2, YFT_predict, n.trees = YFT_model2$gbm.call$best.trees, type = "response") # Predicted values
YFT_predict$model <- preds

model <- dplyr::bind_rows(YFT_filtered, YFT_predict) %>% 
  dplyr::arrange(cellID) %>%  # Make sure everything is arranged by cellID
  dplyr::select(model) %>% 
  pull()

ggYFT <- grid_YFT %>% 
  dplyr::bind_cols(., model = model) %>% 
  dplyr::select(cellID, ocean, model, geometry) %>% 
  sf::st_as_sf(sf_column_name = "geometry")

ggmodel <- plotModel(ggYFT, ggYFT_abun) + # Plot the model
  ggtitle("Yellowfin tuna (1956-1981): Latitude (AUC: 0.78)")
ggsave(plot = ggmodel, filename = "Figures/YFT/YFT_Model2.png", width = 15, height = 8, dpi = 300)

#################################################################################
## Model 3: Restricting range to just the max and min latitudes of Nishikawa ##
#################################################################################
# What are the min and max latitudes?
min(YFT_filtered$latitude)
max(YFT_filtered$latitude)

# Let's restrict the predictions there
YFT_restrict <- YFT_predict %>% 
  dplyr::filter(latitude >= min(YFT_filtered$latitude) & latitude <= max(YFT_filtered$latitude))

# Model should still be Model 2
YFT_filtered %<>% dplyr::select(-model) # clear the model column
YFT_filtered$model <- YFT_model2$fitted # fitted values for the model
preds <- dismo::predict(YFT_model2, YFT_restrict, n.trees = YFT_model2$gbm.call$best.trees, type = "response") # Predicted values
YFT_restrict$model <- preds

YFT_restrict_joined <- YFT_predict %>% 
 # dplyr::select(-model) %>% 
 # dplyr::filter(latitude >= min(YFT_filtered$latitude) & latitude <= max(YFT_filtered$latitude)) %>% 
  dplyr::left_join(., YFT_restrict)

model <- dplyr::bind_rows(YFT_filtered, YFT_restrict_joined) %>% 
  dplyr::arrange(cellID) %>%  # Make sure everything is arranged by cellID
  dplyr::select(model) %>% 
  pull()

ggYFT <- grid_YFT %>% 
  dplyr::arrange(cellID) %>% 
  dplyr::bind_cols(., model = model) %>% 
  dplyr::select(cellID, ocean, latitude, model, geometry) %>% 
  sf::st_as_sf(sf_column_name = "geometry")# %>% 
  dplyr::mutate(model = ifelse(latitude == 49.5, yes = 100, no = model))

ggmodel <- plotModel(ggYFT, ggYFT_abun) + # Plot the model
  ggtitle("Yellowfin tuna (1956-1981): Latitude (restricted range)") +
  geom_hline(yintercept = 5502172, color = "red") +
  geom_hline(yintercept = -5502172, color = "red")
ggsave(plot = ggmodel, filename = "Figures/YFT/YFT_Model3.png", width = 15, height = 8, dpi = 300)

# Try and use Model 2 to predict for present...
YFT_present <- read_csv(file = "Output/CSV/YFT_present_full.csv") %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry)

YFT_present_restricted <- YFT_present %>% 
  dplyr::filter(latitude >= min(YFT_filtered$latitude) & latitude <= max(YFT_filtered$latitude))

YFT_present_left <- YFT_present %>% 
  dplyr::filter(!(latitude >= min(YFT_filtered$latitude) & latitude <= max(YFT_filtered$latitude)))

preds <- dismo::predict(YFT_model2, YFT_present_restricted, n.trees = YFT_model2$gbm.call$best.trees, type = "response") # Predicted values
YFT_present_restricted$model <- preds

model <- dplyr::bind_rows(YFT_present_left, YFT_present_restricted) %>% 
  dplyr::arrange(cellID) %>%  # Make sure everything is arranged by cellID
  dplyr::select(model) %>% 
  pull()

ggYFT <- grid_YFT %>% 
  dplyr::bind_cols(., model = model) %>% 
  dplyr::select(cellID, ocean, model, geometry) %>% 
  sf::st_as_sf(sf_column_name = "geometry")

ggmodel <- plotModel(ggYFT, ggYFT_abun) + # Plot the model
  ggtitle("Yellowfin tuna (2017-2026)") +
  geom_hline(yintercept = 5502172, color = "red") +
  geom_hline(yintercept = -5502172, color = "red")
ggsave(plot = ggmodel, filename = "Figures/YFT/YFT_Model3_Present.png", width = 15, height = 8, dpi = 300)

# Try and use model 2 to predict mid-century
YFT_mid <- read_csv(file = "Output/CSV/YFT_midCentury_full.csv") %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry)

YFT_mid_restricted <- YFT_mid %>% 
  dplyr::filter(latitude >= min(YFT_filtered$latitude) & latitude <= max(YFT_filtered$latitude))

YFT_mid_left <- YFT_mid %>% 
  dplyr::filter(!(latitude >= min(YFT_filtered$latitude) & latitude <= max(YFT_filtered$latitude)))

preds <- dismo::predict(YFT_model2, YFT_mid_restricted, n.trees = YFT_model2$gbm.call$best.trees, type = "response") # Predicted values
YFT_mid_restricted$model <- preds

model <- dplyr::bind_rows(YFT_mid_left, YFT_mid_restricted) %>% 
  dplyr::arrange(cellID) %>%  # Make sure everything is arranged by cellID
  dplyr::select(model) %>% 
  pull()

ggYFT <- grid_YFT %>% 
  dplyr::bind_cols(., model = model) %>% 
  dplyr::select(cellID, ocean, model, geometry) %>% 
  sf::st_as_sf(sf_column_name = "geometry")

ggmodel <- plotModel(ggYFT, ggYFT_abun) + # Plot the model
  ggtitle("Yellowfin tuna (2046-2055)") +
  geom_hline(yintercept = 5502172, color = "red") +
  geom_hline(yintercept = -5502172, color = "red")
ggsave(plot = ggmodel, filename = "Figures/YFT/YFT_Model3_MidCentury.png", width = 15, height = 8, dpi = 300)

# Try and use model 2 to predict end of the century
YFT_end <- read_csv(file = "Output/CSV/YFT_endCentury_full.csv") %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry)

YFT_end_restricted <- YFT_end %>% 
  dplyr::filter(latitude >= min(YFT_filtered$latitude) & latitude <= max(YFT_filtered$latitude))

YFT_end_left <- YFT_end %>% 
  dplyr::filter(!(latitude >= min(YFT_filtered$latitude) & latitude <= max(YFT_filtered$latitude)))

preds <- dismo::predict(YFT_model2, YFT_end_restricted, n.trees = YFT_model2$gbm.call$best.trees, type = "response") # Predicted values
YFT_end_restricted$model <- preds

model <- dplyr::bind_rows(YFT_end_left, YFT_end_restricted) %>% 
  dplyr::arrange(cellID) %>%  # Make sure everything is arranged by cellID
  dplyr::select(model) %>% 
  pull()

ggYFT <- grid_YFT %>% 
  dplyr::bind_cols(., model = model) %>% 
  dplyr::select(cellID, ocean, model, geometry) %>% 
  sf::st_as_sf(sf_column_name = "geometry")

ggmodel <- plotModel(ggYFT, ggYFT_abun) + # Plot the model
  ggtitle("Yellowfin tuna (2091-2100)") +
  geom_hline(yintercept = 5502172, color = "red") +
  geom_hline(yintercept = -5502172, color = "red")
ggsave(plot = ggmodel, filename = "Figures/YFT/YFT_Model3_EndCentury.png", width = 15, height = 8, dpi = 300)

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
