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

# Load swordfish full dataset
SWO_ds <- read_csv("Output/CSV/SWO_historical_full.csv", show_col_types = FALSE)

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

# check the index numbers of the columns
colnames(SWO_filtered)

###########################
## Grid search ##
###########################
CVGrid <- CVgridSearch(SWO_filtered, 5, tc = c(2, 3, 5), bf = c(0.5, 0.75), lr = 0.005)

print(CVGrid %>% dplyr::arrange(cv_deviance))
# constant learning rate of 0.005

#########################################
## Build model: Lat & Lon (restricted) ##
#########################################
SWO_model1 <- dismo::gbm.step(data = SWO_filtered, gbm.x = c(4:6, 8:13),
                              gbm.y = 14, family = "bernoulli",
                              tree.complexity = 5, bag.fraction = 0.5, learning.rate = 0.005,
                              n.folds = 5)
saveRDS(SWO_model1, "Output/Models/SWO_model1.rds") # save the model
#SWO_model0 <- readRDS("Output/Models/SWO_model1.rds") # load the model

summary(SWO_model1) # get the relative importance of each of the predictors

# Plot predictors
pdf(file = "Figures/SWO/SWO_Model1_SmoothPredictors.pdf", width = 10, height = 8)
gbm.plot(SWO_model1, n.plots=9, plot.layout=c(3, 3), write.title = FALSE)
dev.off()

pdf(file = "Figures/SWO/SWO_Model1_Predictors.pdf", width = 10, height = 8)
gbm.plot.fits(SWO_model1)
dev.off()

# Check this for ROC metrics: https://stackoverflow.com/questions/22391547/roc-score-in-gbm-package
# CV AUC of ROC (Receiver Operating Characteristic Curve)
SWO_model1$cv.statistics$discrimination.mean # AUC Score

######################################
## Predict: Historical (restricted) ##
######################################
# What are the min and max latitudes?
min(SWO_filtered$latitude)
max(SWO_filtered$latitude)

# Let's restrict the predictions there
SWO_restrict <- SWO_predict %>% 
  dplyr::filter(latitude >= min(SWO_filtered$latitude) & latitude <= max(SWO_filtered$latitude))

SWO_filtered$model <- SWO_model1$fitted # fitted values for the model
preds <- dismo::predict(SWO_model1, SWO_restrict, n.trees = SWO_model1$gbm.call$best.trees, type = "response") # Predicted values
SWO_restrict$model <- preds

SWO_restrict_joined <- SWO_predict %>% 
  dplyr::left_join(., SWO_restrict)

model <- dplyr::bind_rows(SWO_filtered, SWO_restrict_joined) %>% 
  dplyr::arrange(cellID) %>%  # Make sure everything is arranged by cellID
  dplyr::select(model) %>% 
  pull()

ggSWO <- grid_SWO %>% 
  dplyr::arrange(cellID) %>% 
  dplyr::bind_cols(., model = model) %>% 
  dplyr::select(cellID, ocean, latitude, model, geometry) %>% 
  sf::st_as_sf(sf_column_name = "geometry")

ggSWO_abun <- grid_SWO %>%
  dplyr::mutate(abundance_presence = ifelse(abundance > 0, yes = as.factor(1), no = NA)) %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  sf::st_centroid()

ggmodel <- plotModel(ggSWO, ggSWO_abun) + # Plot the model
  ggtitle("Swordfish (1956-1981): Latitude and longitude (restricted range + grid search); AUC: 0.81") +
  geom_hline(yintercept = 5821011, color = "red") +
  geom_hline(yintercept = -5821011, color = "red")
ggsave(plot = ggmodel, filename = "Figures/SWO/SWO_Model1a.png", width = 15, height = 8, dpi = 300)

######################################
## Predict: Present (restricted) ##
######################################
SWO_present <- read_csv(file = "Output/CSV/SWO_present_full.csv") %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry)

SWO_present_restricted <- SWO_present %>% 
  dplyr::filter(latitude >= min(SWO_filtered$latitude) & latitude <= max(SWO_filtered$latitude))

SWO_present_left <- SWO_present %>% 
  dplyr::filter(!(latitude >= min(SWO_filtered$latitude) & latitude <= max(SWO_filtered$latitude)))

preds <- dismo::predict(SWO_model1, SWO_present_restricted, n.trees = SWO_model1$gbm.call$best.trees, type = "response") # Predicted values
SWO_present_restricted$model <- preds

model <- dplyr::bind_rows(SWO_present_left, SWO_present_restricted) %>% 
  dplyr::arrange(cellID) %>%  # Make sure everything is arranged by cellID
  dplyr::select(model) %>% 
  pull()

ggSWO <- grid_SWO %>% 
  dplyr::bind_cols(., model = model) %>% 
  dplyr::select(cellID, ocean, model, geometry) %>% 
  sf::st_as_sf(sf_column_name = "geometry")

ggmodel <- plotModel(ggSWO, ggSWO_abun) + # Plot the model
  ggtitle("Swordfish (2017-2026): Latitude and longitude (restricted range + grid search)") +
  geom_hline(yintercept = 5821011, color = "red") +
  geom_hline(yintercept = -5821011, color = "red")
ggsave(plot = ggmodel, filename = "Figures/SWO/SWO_Model1b.png", width = 15, height = 8, dpi = 300)

#############################################
## Predict: Mid-century (restricted) ##
#############################################
SWO_mid <- read_csv(file = "Output/CSV/SWO_midCentury_full.csv") %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry)

SWO_mid_restricted <- SWO_mid %>% 
  dplyr::filter(latitude >= min(SWO_filtered$latitude) & latitude <= max(SWO_filtered$latitude))

SWO_mid_left <- SWO_mid %>% 
  dplyr::filter(!(latitude >= min(SWO_filtered$latitude) & latitude <= max(SWO_filtered$latitude)))

preds <- dismo::predict(SWO_model1, SWO_mid_restricted, n.trees = SWO_model1$gbm.call$best.trees, type = "response") # Predicted values
SWO_mid_restricted$model <- preds

model <- dplyr::bind_rows(SWO_mid_left, SWO_mid_restricted) %>% 
  dplyr::arrange(cellID) %>%  # Make sure everything is arranged by cellID
  dplyr::select(model) %>% 
  pull()

ggSWO <- grid_SWO %>% 
  dplyr::bind_cols(., model = model) %>% 
  dplyr::select(cellID, ocean, model, geometry) %>% 
  sf::st_as_sf(sf_column_name = "geometry")

ggmodel <- plotModel(ggSWO, ggSWO_abun) + # Plot the model
  ggtitle("Swordfish (2046-2055): Latitude and longitude (restricted range + grid search)") +
  geom_hline(yintercept = 5821011, color = "red") +
  geom_hline(yintercept = -5821011, color = "red")
ggsave(plot = ggmodel, filename = "Figures/SWO/SWO_Model1c.png", width = 15, height = 8, dpi = 300)

################################################
## Predict: End of the century (restricted) ##
################################################
SWO_end <- read_csv(file = "Output/CSV/SWO_endCentury_full.csv") %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry)

SWO_end_restricted <- SWO_end %>% 
  dplyr::filter(latitude >= min(SWO_filtered$latitude) & latitude <= max(SWO_filtered$latitude))

SWO_end_left <- SWO_end %>% 
  dplyr::filter(!(latitude >= min(SWO_filtered$latitude) & latitude <= max(SWO_filtered$latitude)))

preds <- dismo::predict(SWO_model1, SWO_end_restricted, n.trees = SWO_model1$gbm.call$best.trees, type = "response") # Predicted values
SWO_end_restricted$model <- preds

model <- dplyr::bind_rows(SWO_end_left, SWO_end_restricted) %>% 
  dplyr::arrange(cellID) %>%  # Make sure everything is arranged by cellID
  dplyr::select(model) %>% 
  pull()

ggSWO <- grid_SWO %>% 
  dplyr::bind_cols(., model = model) %>% 
  dplyr::select(cellID, ocean, model, geometry) %>% 
  sf::st_as_sf(sf_column_name = "geometry")

ggmodel <- plotModel(ggSWO, ggSWO_abun) + # Plot the model
  ggtitle("Swordfish (2091-2100): Latitude and longitude (restricted range + grid search)") +
  geom_hline(yintercept = 5821011, color = "red") +
  geom_hline(yintercept = -5821011, color = "red")
ggsave(plot = ggmodel, filename = "Figures/SWO/SWO_Model1d.png", width = 15, height = 8, dpi = 300)
