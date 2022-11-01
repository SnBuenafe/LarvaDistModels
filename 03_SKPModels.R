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

# Load skipjack tuna full dataset
SKP_ds <- read_csv("Output/CSV/SKP_historical_full.csv", show_col_types = FALSE)

# Always build models with known data only
SKP_filtered <- SKP_ds %>% 
  dplyr::filter(!is.na(abundance)) %>% # filter those with data!
  dplyr::mutate(across(where(is.character), ~factor(.))) %>% # convert all characters to factors
  dplyr::mutate(abundance_presence = case_when(abundance > 0 ~ 1,
                                               abundance == 0 ~ 0)) %>%  # mutate the abundance data into 1s and 0s
  dplyr::select(-geometry) %>% 
  as.data.frame() #gbm.step doesn't work if it's a tibble...

# Data.frame for predictions
SKP_predict <- SKP_ds %>% 
  dplyr::filter(is.na(abundance)) %>% # filter those with data!
  dplyr::mutate(across(where(is.character), ~factor(.))) %>% # convert all characters to factors
  dplyr::mutate(abundance_presence = case_when(abundance > 0 ~ 1,
                                               abundance == 0 ~ 0)) %>%  # mutate the abundance data into 1s and 0s
  dplyr::select(-geometry) %>% 
  as.data.frame() #gbm.step doesn't work if it's a tibble...

# check the index numbers of the columns
colnames(SKP_filtered)

###########################
## Grid search ##
###########################
CVGrid <- CVgridSearch(SKP_filtered, 5, tc = c(2, 3, 5), bf = c(0.5, 0.75), lr = 0.005)

print(CVGrid %>% dplyr::arrange(cv_deviance))
# constant learning rate of 0.005

#########################################
## Build model: Lat & Lon (restricted) ##
#########################################
SKP_model1 <- dismo::gbm.step(data = SKP_filtered, gbm.x = c(4:6, 8:13),
                              gbm.y = 14, family = "bernoulli",
                              tree.complexity = 5, bag.fraction = 0.5, learning.rate = 0.005,
                              n.folds = 5)
saveRDS(SKP_model1, "Output/Models/SKP_model1.rds") # save the model
#SKP_model0 <- readRDS("Output/Models/SKP_model1.rds") # load the model

summary(SKP_model1) # get the relative importance of each of the predictors

# Plot predictors
pdf(file = "Figures/SKP/SKP_Model1_SmoothPredictors.pdf", width = 10, height = 8)
gbm.plot(SKP_model1, n.plots=9, plot.layout=c(3, 3), write.title = FALSE)
dev.off()

pdf(file = "Figures/SKP/SKP_Model1_Predictors.pdf", width = 10, height = 8)
gbm.plot.fits(SKP_model1)
dev.off()

# Check this for ROC metrics: https://stackoverflow.com/questions/22391547/roc-score-in-gbm-package
# CV AUC of ROC (Receiver Operating Characteristic Curve)
SKP_model1$cv.statistics$discrimination.mean # AUC Score

######################################
## Predict: Historical (restricted) ##
######################################
# What are the min and max latitudes?
min(SKP_filtered$latitude)
max(SKP_filtered$latitude)

# Let's restrict the predictions there
SKP_restrict <- SKP_predict %>% 
  dplyr::filter(latitude >= min(SKP_filtered$latitude) & latitude <= max(SKP_filtered$latitude))

SKP_filtered$model <- SKP_model1$fitted # fitted values for the model
preds <- dismo::predict(SKP_model1, SKP_restrict, n.trees = SKP_model1$gbm.call$best.trees, type = "response") # Predicted values
SKP_restrict$model <- preds

SKP_restrict_joined <- SKP_predict %>% 
  dplyr::left_join(., SKP_restrict)

model <- dplyr::bind_rows(SKP_filtered, SKP_restrict_joined) %>% 
  dplyr::arrange(cellID) %>%  # Make sure everything is arranged by cellID
  dplyr::select(model) %>% 
  pull()

ggSKP <- grid_SKP %>% 
  dplyr::arrange(cellID) %>% 
  dplyr::bind_cols(., model = model) %>% 
  dplyr::select(cellID, ocean, latitude, model, geometry) %>% 
  sf::st_as_sf(sf_column_name = "geometry")

ggSKP_abun <- grid_SKP %>%
  dplyr::mutate(abundance_presence = ifelse(abundance > 0, yes = as.factor(1), no = NA)) %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  sf::st_centroid()

ggmodel <- plotModel(ggSKP, ggSKP_abun) + # Plot the model
  ggtitle("Skipjack tuna (1956-1981): Latitude and longitude (restricted range + grid search); AUC: 0.81") +
  geom_hline(yintercept = 5821011, color = "red") +
  geom_hline(yintercept = -5821011, color = "red")
ggsave(plot = ggmodel, filename = "Figures/SKP/SKP_Model1a.png", width = 15, height = 8, dpi = 300)

######################################
## Predict: Present (restricted) ##
######################################
SKP_present <- read_csv(file = "Output/CSV/SKP_present_full.csv") %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry)

SKP_present_restricted <- SKP_present %>% 
  dplyr::filter(latitude >= min(SKP_filtered$latitude) & latitude <= max(SKP_filtered$latitude))

SKP_present_left <- SKP_present %>% 
  dplyr::filter(!(latitude >= min(SKP_filtered$latitude) & latitude <= max(SKP_filtered$latitude)))

preds <- dismo::predict(SKP_model1, SKP_present_restricted, n.trees = SKP_model1$gbm.call$best.trees, type = "response") # Predicted values
SKP_present_restricted$model <- preds

model <- dplyr::bind_rows(SKP_present_left, SKP_present_restricted) %>% 
  dplyr::arrange(cellID) %>%  # Make sure everything is arranged by cellID
  dplyr::select(model) %>% 
  pull()

ggSKP <- grid_SKP %>% 
  dplyr::bind_cols(., model = model) %>% 
  dplyr::select(cellID, ocean, model, geometry) %>% 
  sf::st_as_sf(sf_column_name = "geometry")

ggmodel <- plotModel(ggSKP, ggSKP_abun) + # Plot the model
  ggtitle("Skipjack tuna (2017-2026): Latitude and longitude (restricted range + grid search)") +
  geom_hline(yintercept = 5821011, color = "red") +
  geom_hline(yintercept = -5821011, color = "red")
ggsave(plot = ggmodel, filename = "Figures/SKP/SKP_Model1b.png", width = 15, height = 8, dpi = 300)

#############################################
## Predict: Mid-century (restricted) ##
#############################################
SKP_mid <- read_csv(file = "Output/CSV/SKP_midCentury_full.csv") %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry)

SKP_mid_restricted <- SKP_mid %>% 
  dplyr::filter(latitude >= min(SKP_filtered$latitude) & latitude <= max(SKP_filtered$latitude))

SKP_mid_left <- SKP_mid %>% 
  dplyr::filter(!(latitude >= min(SKP_filtered$latitude) & latitude <= max(SKP_filtered$latitude)))

preds <- dismo::predict(SKP_model1, SKP_mid_restricted, n.trees = SKP_model1$gbm.call$best.trees, type = "response") # Predicted values
SKP_mid_restricted$model <- preds

model <- dplyr::bind_rows(SKP_mid_left, SKP_mid_restricted) %>% 
  dplyr::arrange(cellID) %>%  # Make sure everything is arranged by cellID
  dplyr::select(model) %>% 
  pull()

ggSKP <- grid_SKP %>% 
  dplyr::bind_cols(., model = model) %>% 
  dplyr::select(cellID, ocean, model, geometry) %>% 
  sf::st_as_sf(sf_column_name = "geometry")

ggmodel <- plotModel(ggSKP, ggSKP_abun) + # Plot the model
  ggtitle("Skipjack tuna (2046-2055): Latitude and longitude (restricted range + grid search)") +
  geom_hline(yintercept = 5821011, color = "red") +
  geom_hline(yintercept = -5821011, color = "red")
ggsave(plot = ggmodel, filename = "Figures/SKP/SKP_Model1c.png", width = 15, height = 8, dpi = 300)

################################################
## Predict: End of the century (restricted) ##
################################################
SKP_end <- read_csv(file = "Output/CSV/SKP_endCentury_full.csv") %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry)

SKP_end_restricted <- SKP_end %>% 
  dplyr::filter(latitude >= min(SKP_filtered$latitude) & latitude <= max(SKP_filtered$latitude))

SKP_end_left <- SKP_end %>% 
  dplyr::filter(!(latitude >= min(SKP_filtered$latitude) & latitude <= max(SKP_filtered$latitude)))

preds <- dismo::predict(SKP_model1, SKP_end_restricted, n.trees = SKP_model1$gbm.call$best.trees, type = "response") # Predicted values
SKP_end_restricted$model <- preds

model <- dplyr::bind_rows(SKP_end_left, SKP_end_restricted) %>% 
  dplyr::arrange(cellID) %>%  # Make sure everything is arranged by cellID
  dplyr::select(model) %>% 
  pull()

ggSKP <- grid_SKP %>% 
  dplyr::bind_cols(., model = model) %>% 
  dplyr::select(cellID, ocean, model, geometry) %>% 
  sf::st_as_sf(sf_column_name = "geometry")

ggmodel <- plotModel(ggSKP, ggSKP_abun) + # Plot the model
  ggtitle("Skipjack tuna (2091-2100): Latitude and longitude (restricted range + grid search)") +
  geom_hline(yintercept = 5821011, color = "red") +
  geom_hline(yintercept = -5821011, color = "red")
ggsave(plot = ggmodel, filename = "Figures/SKP/SKP_Model1d.png", width = 15, height = 8, dpi = 300)
