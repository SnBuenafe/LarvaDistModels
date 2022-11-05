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
                                      abundance == 0 ~ 0), 
                row = row_number()) %>%  # mutate the abundance data into 1s and 0s
  dplyr::select(-geometry) %>% 
  dplyr::select(row, cellID, species, abundance, abundance_presence, ocean, longitude, latitude, season, everything()) %>% # arrange columns
  as.data.frame() #gbm.step doesn't work if it's a tibble...

# We divide the data into train (training and validation) and test
nrow(YFT_filtered) * 0.9 # = 11890

train <- slice_sample(YFT_filtered, n = 11890, replace = FALSE) # 90% training set
test <- YFT_filtered[!YFT_filtered$row %in% train$row, ] # 10% testing set

# Data.frame for predictions
YFT_predict <- YFT_ds %>% 
  dplyr::filter(is.na(abundance)) %>% # filter those with data!
  dplyr::mutate(across(where(is.character), ~factor(.))) %>% # convert all characters to factors
  dplyr::mutate(abundance_presence = case_when(abundance > 0 ~ 1,
                                               abundance == 0 ~ 0)) %>%  # mutate the abundance data into 1s and 0s
  dplyr::select(-geometry) %>% 
  dplyr::select(cellID, species, abundance, abundance_presence, ocean, longitude, latitude, season, everything()) %>% # arrange columns
  as.data.frame() #gbm.step doesn't work if it's a tibble...

###########################
## Model 0: None ##
###########################
# Building models
# check the index numbers of the columns
colnames(YFT_filtered)

# higher AUC better. A model with AUC values closer to 0 have more wrong predictions.
# see: https://rspatial.org/raster/sdm/9_sdm_brt.html for interpreting results

YFT_model0 <- dismo::gbm.step(data = YFT_filtered, gbm.x = c(4, 8:11, 17:18),
                              gbm.y = 14, family = "bernoulli", n.folds = 5)
saveRDS(YFT_model0, "Output/Models/YFT_model0.rds") # save the model
#YFT_model0 <- readRDS("Output/Models/YFT_model0.rds") # load the model

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
YFT_filtered %<>% dplyr::select(-model) # Deselect models on both data.frames
YFT_predict %<>% dplyr::select(-model)

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
YFT_model1 <- dismo::gbm.step(data = YFT_filtered, gbm.x = c(4, 7:11, 17:18),
                              gbm.y = 14, family = "bernoulli",
                              n.folds = 5)
saveRDS(YFT_model1, "Output/Models/YFT_model1.rds") # save the model
#YFT_model1 <- readRDS("Output/Models/YFT_model1.rds") # load the model

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

YFT_filtered %<>% dplyr::select(-model) # Deselect models on both data.frames
YFT_predict %<>% dplyr::select(-model)

# Plot
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
## Model 2: Lat & Lon ##
###########################
YFT_model2 <- dismo::gbm.step(data = YFT_filtered, gbm.x = c(4:6, 8:11, 17:18),
                              gbm.y = 14, family = "bernoulli",
                              n.folds = 5)
saveRDS(YFT_model2, "Output/Models/YFT_model2.rds") # save the model
#YFT_model2 <- readRDS("Output/Models/YFT_model2.rds") # load the model

summary(YFT_model2) # get the relative importance of each of the predictors

# Plot predictors
pdf(file = "Figures/YFT/YFT_Model2_SmoothPredictors.pdf", width = 10, height = 8)
gbm.plot(YFT_model2, n.plots=9, plot.layout=c(3, 3), write.title = FALSE)
dev.off()

pdf(file = "Figures/YFT/YFT_Model2_Predictors.pdf", width = 10, height = 8)
gbm.plot.fits(YFT_model2)
dev.off()

# Check this for ROC metrics: https://stackoverflow.com/questions/22391547/roc-score-in-gbm-package
# CV AUC of ROC (Receiver Operating Characteristic Curve)
YFT_model2$cv.statistics$discrimination.mean # AUC Score

YFT_filtered %<>% dplyr::select(-model) # Deselect models on both data.frames
YFT_predict %<>% dplyr::select(-model)

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
  ggtitle("Yellowfin tuna (1956-1981): With longitude and latitude (AUC: 0.74)")
ggsave(plot = ggmodel, filename = "Figures/YFT/YFT_Model2.png", width = 15, height = 8, dpi = 300)

#############################################
## Model 2a: Restricted (historical) ##
#############################################
# What are the min and max latitudes?
min(YFT_filtered$latitude)
max(YFT_filtered$latitude)

# Let's restrict the predictions there
YFT_restrict <- YFT_predict %>% 
  dplyr::filter(latitude >= min(YFT_filtered$latitude) & latitude <= max(YFT_filtered$latitude))

# Model should still be Model 2
YFT_filtered %<>% dplyr::select(-model) # clear the model column
YFT_predict %<>% dplyr::select(-model)
YFT_filtered$model <- YFT_model2$fitted # fitted values for the model
preds <- dismo::predict(YFT_model2, YFT_restrict, n.trees = YFT_model2$gbm.call$best.trees, type = "response") # Predicted values
YFT_restrict$model <- preds

YFT_restrict_joined <- YFT_predict %>% 
  dplyr::left_join(., YFT_restrict)

model <- dplyr::bind_rows(YFT_filtered, YFT_restrict_joined) %>% 
  dplyr::arrange(cellID) %>%  # Make sure everything is arranged by cellID
  dplyr::select(model) %>% 
  pull()

ggYFT <- grid_YFT %>% 
  dplyr::arrange(cellID) %>% 
  dplyr::bind_cols(., model = model) %>% 
  dplyr::select(cellID, ocean, latitude, model, geometry) %>% 
  sf::st_as_sf(sf_column_name = "geometry")

ggmodel <- plotModel(ggYFT, ggYFT_abun) + # Plot the model
  ggtitle("Yellowfin tuna (1956-1981): Latitude and latitude (restricted range)") +
  geom_hline(yintercept = 5821011, color = "red") +
  geom_hline(yintercept = -5821011, color = "red")
ggsave(plot = ggmodel, filename = "Figures/YFT/YFT_Model2a.png", width = 15, height = 8, dpi = 300)

#############################################
## Model 2b: Restricted (present) ##
#############################################
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
  ggtitle("Yellowfin tuna (2017-2026): Latitude and longitude (restricted range)") +
  geom_hline(yintercept = 5821011, color = "red") +
  geom_hline(yintercept = -5821011, color = "red")
ggsave(plot = ggmodel, filename = "Figures/YFT/YFT_Model2b.png", width = 15, height = 8, dpi = 300)

#############################################
## Model 2c: Restricted (mid-century) ##
#############################################
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
  ggtitle("Yellowfin tuna (2046-2055): Latitude and longitude (restricted range)") +
  geom_hline(yintercept = 5821011, color = "red") +
  geom_hline(yintercept = -5821011, color = "red")
ggsave(plot = ggmodel, filename = "Figures/YFT/YFT_Model2c.png", width = 15, height = 8, dpi = 300)

################################################
## Model 2d: Restricted (end of the century) ##
################################################
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
  ggtitle("Yellowfin tuna (2091-2100): Latitude and longitude (restricted range)") +
  geom_hline(yintercept = 5821011, color = "red") +
  geom_hline(yintercept = -5821011, color = "red")
ggsave(plot = ggmodel, filename = "Figures/YFT/YFT_Model2d.png", width = 15, height = 8, dpi = 300)

###########################
## Model 3: Latitude ##
###########################
YFT_model3 <- dismo::gbm.step(data = YFT_filtered, gbm.x = c(4, 6, 8:11, 17:18),
                              gbm.y = 14, family = "bernoulli",
                              n.folds = 5)
saveRDS(YFT_model3, "Output/Models/YFT_model3.rds") # save the model
#YFT_model3 <- readRDS("Output/Models/YFT_model3.rds") # load the model

summary(YFT_model3) # get the relative importance of each of the predictors

# Plot predictors
pdf(file = "Figures/YFT/YFT_Model3_SmoothPredictors.pdf", width = 10, height = 8)
gbm.plot(YFT_model3, n.plots=8, plot.layout=c(3, 3), write.title = FALSE)
dev.off()

pdf(file = "Figures/YFT/YFT_Model3_Predictors.pdf", width = 10, height = 8)
gbm.plot.fits(YFT_model3)
dev.off()

# Check this for ROC metrics: https://stackoverflow.com/questions/22391547/roc-score-in-gbm-package
# CV AUC of ROC (Receiver Operating Characteristic Curve)
YFT_model3$cv.statistics$discrimination.mean # AUC Score

YFT_filtered %<>% dplyr::select(-model) # Deselect models on both data.frames
YFT_predict %<>% dplyr::select(-model)

YFT_filtered$model <- YFT_model3$fitted # fitted values for the model
preds <- dismo::predict(YFT_model3, YFT_predict, n.trees = YFT_model3$gbm.call$best.trees, type = "response") # Predicted values
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
  ggtitle("Yellowfin tuna (1956-1981): Latitude (AUC: 0.72)")
ggsave(plot = ggmodel, filename = "Figures/YFT/YFT_Model3.png", width = 15, height = 8, dpi = 300)

#############################################
## Model 3a: Restricted (historical) ##
#############################################
# What are the min and max latitudes?
min(YFT_filtered$latitude)
max(YFT_filtered$latitude)

# Let's restrict the predictions there
YFT_restrict <- YFT_predict %>% 
  dplyr::filter(latitude >= min(YFT_filtered$latitude) & latitude <= max(YFT_filtered$latitude))

# Model should still be Model 3
YFT_filtered %<>% dplyr::select(-model) # clear the model column
YFT_predict %<>% dplyr::select(-model)
YFT_filtered$model <- YFT_model3$fitted # fitted values for the model
preds <- dismo::predict(YFT_model3, YFT_restrict, n.trees = YFT_model3$gbm.call$best.trees, type = "response") # Predicted values
YFT_restrict$model <- preds

YFT_restrict_joined <- YFT_predict %>% 
  dplyr::left_join(., YFT_restrict)

model <- dplyr::bind_rows(YFT_filtered, YFT_restrict_joined) %>% 
  dplyr::arrange(cellID) %>%  # Make sure everything is arranged by cellID
  dplyr::select(model) %>% 
  pull()

ggYFT <- grid_YFT %>% 
  dplyr::arrange(cellID) %>% 
  dplyr::bind_cols(., model = model) %>% 
  dplyr::select(cellID, ocean, latitude, model, geometry) %>% 
  sf::st_as_sf(sf_column_name = "geometry")

ggmodel <- plotModel(ggYFT, ggYFT_abun) + # Plot the model
  ggtitle("Yellowfin tuna (1956-1981): Latitude (restricted range)") +
  geom_hline(yintercept = 5821011, color = "red") +
  geom_hline(yintercept = -5821011, color = "red")
ggsave(plot = ggmodel, filename = "Figures/YFT/YFT_Model3a.png", width = 15, height = 8, dpi = 300)

#############################################
## Model 3b: Restricted (present) ##
#############################################
YFT_present <- read_csv(file = "Output/CSV/YFT_present_full.csv") %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry)

YFT_present_restricted <- YFT_present %>% 
  dplyr::filter(latitude >= min(YFT_filtered$latitude) & latitude <= max(YFT_filtered$latitude))

YFT_present_left <- YFT_present %>% 
  dplyr::filter(!(latitude >= min(YFT_filtered$latitude) & latitude <= max(YFT_filtered$latitude)))

preds <- dismo::predict(YFT_model3, YFT_present_restricted, n.trees = YFT_model3$gbm.call$best.trees, type = "response") # Predicted values
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
  ggtitle("Yellowfin tuna (2017-2026): Latitude (restricted range)") +
  geom_hline(yintercept = 5821011, color = "red") +
  geom_hline(yintercept = -5821011, color = "red")
ggsave(plot = ggmodel, filename = "Figures/YFT/YFT_Model3b.png", width = 15, height = 8, dpi = 300)


#############################################
## Model 3c: Restricted (mid-century) ##
#############################################
# Try and use model 3 to predict mid-century
YFT_mid <- read_csv(file = "Output/CSV/YFT_midCentury_full.csv") %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry)

YFT_mid_restricted <- YFT_mid %>% 
  dplyr::filter(latitude >= min(YFT_filtered$latitude) & latitude <= max(YFT_filtered$latitude))

YFT_mid_left <- YFT_mid %>% 
  dplyr::filter(!(latitude >= min(YFT_filtered$latitude) & latitude <= max(YFT_filtered$latitude)))

preds <- dismo::predict(YFT_model3, YFT_mid_restricted, n.trees = YFT_model3$gbm.call$best.trees, type = "response") # Predicted values
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
  ggtitle("Yellowfin tuna (2046-2055): Latitude (restricted range)") +
  geom_hline(yintercept = 5821011, color = "red") +
  geom_hline(yintercept = -5821011, color = "red")
ggsave(plot = ggmodel, filename = "Figures/YFT/YFT_Model3c.png", width = 15, height = 8, dpi = 300)

################################################
## Model 3d: Restricted (end of the century) ##
################################################
# Try and use model 3 to predict end of the century
YFT_end <- read_csv(file = "Output/CSV/YFT_endCentury_full.csv") %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry)

YFT_end_restricted <- YFT_end %>% 
  dplyr::filter(latitude >= min(YFT_filtered$latitude) & latitude <= max(YFT_filtered$latitude))

YFT_end_left <- YFT_end %>% 
  dplyr::filter(!(latitude >= min(YFT_filtered$latitude) & latitude <= max(YFT_filtered$latitude)))

preds <- dismo::predict(YFT_model3, YFT_end_restricted, n.trees = YFT_model3$gbm.call$best.trees, type = "response") # Predicted values
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
  ggtitle("Yellowfin tuna (2091-2100): Latitude (restricted range)") +
  geom_hline(yintercept = 5821011, color = "red") +
  geom_hline(yintercept = -5821011, color = "red")
ggsave(plot = ggmodel, filename = "Figures/YFT/YFT_Model3d.png", width = 15, height = 8, dpi = 300)

###########################
## Model 4: Longitude ##
###########################
YFT_model4 <- dismo::gbm.step(data = YFT_filtered, gbm.x = c(4:5, 8:11, 17:18),
                              gbm.y = 14, family = "bernoulli",
                              n.folds = 5)
saveRDS(YFT_model4, "Output/Models/YFT_model4.rds") # save the model
#YFT_model3 <- readRDS("Output/Models/YFT_model4.rds") # load the model

summary(YFT_model4) # get the relative importance of each of the predictors

# Plot predictors
pdf(file = "Figures/YFT/YFT_Model4_SmoothPredictors.pdf", width = 10, height = 8)
gbm.plot(YFT_model4, n.plots=8, plot.layout=c(3, 3), write.title = FALSE)
dev.off()

pdf(file = "Figures/YFT/YFT_Model4_Predictors.pdf", width = 10, height = 8)
gbm.plot.fits(YFT_model4)
dev.off()

# Check this for ROC metrics: https://stackoverflow.com/questions/22391547/roc-score-in-gbm-package
# CV AUC of ROC (Receiver Operating Characteristic Curve)
YFT_model4$cv.statistics$discrimination.mean # AUC Score

YFT_filtered %<>% dplyr::select(-model) # Deselect models on both data.frames
YFT_predict %<>% dplyr::select(-model)

YFT_filtered$model <- YFT_model4$fitted # fitted values for the model
preds <- dismo::predict(YFT_model4, YFT_predict, n.trees = YFT_model4$gbm.call$best.trees, type = "response") # Predicted values
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
  ggtitle("Yellowfin tuna (1956-1981): Longitude (AUC: 0.74)")
ggsave(plot = ggmodel, filename = "Figures/YFT/YFT_Model4.png", width = 15, height = 8, dpi = 300)


#######################################################################
## Model 5: Grid search on longitude and latitude (restricted range) ##
#######################################################################
# Do a cross-validated grid search using gbm.fixed
# define a 5-fold cross-validation
# Setting learning rate to be 
CVGrid <- CVgridSearch(YFT_filtered, 5, tc = c(2, 3, 5), bf = c(0.5, 0.75), lr = 0.005, pred_in = c(4:6, 8:11, 17:18), resp_in = 19)

print(CVGrid %>% dplyr::arrange(cv_deviance))
# constant learning rate of 0.005; we want >= 1000 trees (Cerasoli et al., 2017)

# Now build the model; we want to have a lot of trees: preferably at least 1,000 trees
YFT_model5 <- gbm.step(YFT_filtered, gbm.x = c(4:6, 8:11, 17:18), gbm.y = 19, 
                  learning.rate = 0.005,
                  bag.fraction = 0.5,
                  tree.complexity = 5,
                  n.folds = 5 # 5-fold CV
                  )
saveRDS(YFT_model5, "Output/Models/YFT_model5.rds") # save the model
#YFT_model5 <- readRDS("Output/Models/YFT_model5.rds") # load the model

summary(YFT_model5) # get the relative importance of each of the predictors

# Plot predictors
pdf(file = "Figures/YFT/YFT_Model5_SmoothPredictors.pdf", width = 10, height = 8)
gbm.plot(YFT_model5, n.plots=9, plot.layout=c(3, 3), write.title = FALSE)
dev.off()

pdf(file = "Figures/YFT/YFT_Model5_Predictors.pdf", width = 10, height = 8)
gbm.plot.fits(YFT_model5)
dev.off()

# Check this for ROC metrics: https://stackoverflow.com/questions/22391547/roc-score-in-gbm-package
# CV AUC of ROC (Receiver Operating Characteristic Curve)
YFT_model5$cv.statistics$discrimination.mean # AUC Score

#############################################
## Model 5a: Restricted (historical) ##
#############################################
# What are the min and max latitudes?
min(YFT_filtered$latitude)
max(YFT_filtered$latitude)

# Let's restrict the predictions there
YFT_restrict <- YFT_predict %>% 
  dplyr::filter(latitude >= min(YFT_filtered$latitude) & latitude <= max(YFT_filtered$latitude))

# Model should still be Model 3
YFT_filtered %<>% dplyr::select(-model) # clear the model column
YFT_predict %<>% dplyr::select(-model)
YFT_filtered$model <- YFT_model5$fitted # fitted values for the model
preds <- dismo::predict(YFT_model5, YFT_restrict, n.trees = YFT_model5$gbm.call$best.trees, type = "response") # Predicted values
YFT_restrict$model <- preds

YFT_restrict_joined <- YFT_predict %>% 
  dplyr::left_join(., YFT_restrict)

model <- dplyr::bind_rows(YFT_filtered, YFT_restrict_joined) %>% 
  dplyr::arrange(cellID) %>%  # Make sure everything is arranged by cellID
  dplyr::select(model) %>% 
  pull()

ggYFT <- grid_YFT %>% 
  dplyr::arrange(cellID) %>% 
  dplyr::bind_cols(., model = model) %>% 
  dplyr::select(cellID, ocean, latitude, model, geometry) %>% 
  sf::st_as_sf(sf_column_name = "geometry")

ggmodel <- plotModel(ggYFT, ggYFT_abun) + # Plot the model
  ggtitle("Yellowfin tuna (1956-1981): Latitude and longitude (restricted range + grid search); AUC: 0.81") +
  geom_hline(yintercept = 5821011, color = "red") +
  geom_hline(yintercept = -5821011, color = "red")
ggsave(plot = ggmodel, filename = "Figures/YFT/YFT_Model5a.png", width = 15, height = 8, dpi = 300)

#############################################
## Model 5b: Restricted (present) ##
#############################################
YFT_present <- read_csv(file = "Output/CSV/YFT_present_full.csv") %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry)

YFT_present_restricted <- YFT_present %>% 
  dplyr::filter(latitude >= min(YFT_filtered$latitude) & latitude <= max(YFT_filtered$latitude))

YFT_present_left <- YFT_present %>% 
  dplyr::filter(!(latitude >= min(YFT_filtered$latitude) & latitude <= max(YFT_filtered$latitude)))

preds <- dismo::predict(YFT_model5, YFT_present_restricted, n.trees = YFT_model5$gbm.call$best.trees, type = "response") # Predicted values
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
  ggtitle("Yellowfin tuna (2017-2026): Latitude and longitude (restricted range + grid search); AUC: 0.81") +
  geom_hline(yintercept = 5821011, color = "red") +
  geom_hline(yintercept = -5821011, color = "red")
ggsave(plot = ggmodel, filename = "Figures/YFT/YFT_Model5b.png", width = 15, height = 8, dpi = 300)

#############################################
## Model 5c: Restricted (mid-century) ##
#############################################
# Try and use model 5 to predict mid-century
YFT_mid <- read_csv(file = "Output/CSV/YFT_midCentury_full.csv") %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry)

YFT_mid_restricted <- YFT_mid %>% 
  dplyr::filter(latitude >= min(YFT_filtered$latitude) & latitude <= max(YFT_filtered$latitude))

YFT_mid_left <- YFT_mid %>% 
  dplyr::filter(!(latitude >= min(YFT_filtered$latitude) & latitude <= max(YFT_filtered$latitude)))

preds <- dismo::predict(YFT_model5, YFT_mid_restricted, n.trees = YFT_model5$gbm.call$best.trees, type = "response") # Predicted values
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
  ggtitle("Yellowfin tuna (2046-2055): Latitude and longitude (restricted range + grid search); AUC: 0.81") +
  geom_hline(yintercept = 5821011, color = "red") +
  geom_hline(yintercept = -5821011, color = "red")
ggsave(plot = ggmodel, filename = "Figures/YFT/YFT_Model5c.png", width = 15, height = 8, dpi = 300)

################################################
## Model 5d: Restricted (end of the century) ##
################################################
# Try and use model 5 to predict end of the century
YFT_end <- read_csv(file = "Output/CSV/YFT_endCentury_full.csv") %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry)

YFT_end_restricted <- YFT_end %>% 
  dplyr::filter(latitude >= min(YFT_filtered$latitude) & latitude <= max(YFT_filtered$latitude))

YFT_end_left <- YFT_end %>% 
  dplyr::filter(!(latitude >= min(YFT_filtered$latitude) & latitude <= max(YFT_filtered$latitude)))

preds <- dismo::predict(YFT_model5, YFT_end_restricted, n.trees = YFT_model5$gbm.call$best.trees, type = "response") # Predicted values
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
  ggtitle("Yellowfin tuna (2091-2100): Latitude and longitude (restricted range + grid search); AUC: 0.81") +
  geom_hline(yintercept = 5821011, color = "red") +
  geom_hline(yintercept = -5821011, color = "red")
ggsave(plot = ggmodel, filename = "Figures/YFT/YFT_Model5d.png", width = 15, height = 8, dpi = 300)


#######################################################################
## Trying with additional predictors and accounting for overfitting ##
#######################################################################
# Do a more stringent cross-validated grid search using gbm.fixed
# define a 5-fold cross-validation
CVGrid <- CVgridSearch(YFT_filtered, 5, tc = c(1, 2, 3), bf = c(0.5, 0.75), lr = seq(0.005, 0.01, 0.001), pred_in = c(4:6, 8:18), resp_in = 19)

print(CVGrid %>% dplyr::arrange(cv_deviance), n = 36) # Get the parameters with the lowest deviance?

##################
## Trial models ##
##################
# Addt'l parameters: sos, mlotst, no3os, po4os, nh4os

# Now build the model; we want to have a lot of trees: preferably at least 1,000 trees
YFT_trialModel <- gbm.step(train, gbm.x = c(4:6, 8:18), gbm.y = 19, 
                       learning.rate = 0.01,
                       bag.fraction = 0.5,
                       tree.complexity = 1,
                       n.folds = 5 # 5-fold CV
)

# Check this for ROC metrics: https://stackoverflow.com/questions/22391547/roc-score-in-gbm-package
# CV AUC of ROC (Receiver Operating Characteristic Curve)
YFT_trialModel$self.statistics$discrimination # Training AUC
YFT_trialModel$cv.statistics$discrimination.mean # Validating AUC

.roc <-function (obsdat, preddat) {
  # code adapted from Ferrier, Pearce and Watson's code, by J.Elith
  #
  # see:
  # Hanley, J.A. & McNeil, B.J. (1982) The meaning and use of the area
  # under a Receiver Operating Characteristic (ROC) curve.
  # Radiology, 143, 29-36
  #
  # Pearce, J. & Ferrier, S. (2000) Evaluating the predictive performance
  # of habitat models developed using logistic regression.
  # Ecological Modelling, 133, 225-245.
  # this is the non-parametric calculation for area under the ROC curve, 
  # using the fact that a MannWhitney U statistic is closely related to
  # the area
  #
  
  # in dismo, this is used in the gbm routines, but not elsewhere (see evaluate).
  
  if (length(obsdat) != length(preddat)) { 
    stop("obs and preds must be equal lengths")
  }
  n.x <- length(obsdat[obsdat == 0])
  n.y <- length(obsdat[obsdat == 1])
  xy <- c(preddat[obsdat == 0], preddat[obsdat == 1])
  rnk <- rank(xy)
  wilc <- ((n.x * n.y) + ((n.x * (n.x + 1))/2) - sum(rnk[1:n.x]))/(n.x * n.y)
  return(round(wilc, 4))
}

preds <- gbm::predict.gbm(YFT_trialModel, test, n.trees = YFT_trialModel$gbm.call$best.trees, type = "response")
.roc(test$abundance_presence, preds)
