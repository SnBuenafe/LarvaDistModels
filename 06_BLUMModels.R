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

# Load blue marlin full dataset
BLUM_ds <- read_csv("Output/CSV/BLUM_historical_full.csv", show_col_types = FALSE)

# Always build models with known data only
BLUM_filtered <- BLUM_ds %>% 
  dplyr::filter(!is.na(abundance)) %>% # filter those with data!
  dplyr::mutate(across(where(is.character), ~factor(.))) %>% # convert all characters to factors
  dplyr::mutate(abundance_presence = case_when(abundance > 0 ~ 1,
                                               abundance == 0 ~ 0)) %>%  # mutate the abundance data into 1s and 0s
  dplyr::select(-geometry) %>% 
  as.data.frame() #gbm.step doesn't work if it's a tibble...

# Data.frame for predictions
BLUM_predict <- BLUM_ds %>% 
  dplyr::filter(is.na(abundance)) %>% # filter those with data!
  dplyr::mutate(across(where(is.character), ~factor(.))) %>% # convert all characters to factors
  dplyr::mutate(abundance_presence = case_when(abundance > 0 ~ 1,
                                               abundance == 0 ~ 0)) %>%  # mutate the abundance data into 1s and 0s
  dplyr::select(-geometry) %>% 
  as.data.frame() #gbm.step doesn't work if it's a tibble...

# check the index numbers of the columns
colnames(BLUM_filtered)

###########################
## Grid search ##
###########################
CVGrid <- CVgridSearch(BLUM_filtered, 5, tc = c(2, 3, 5), bf = c(0.5, 0.75), lr = 0.005)

print(CVGrid %>% dplyr::arrange(cv_deviance))
# constant learning rate of 0.005

#########################################
## Build model: Lat & Lon (restricted) ##
#########################################
BLUM_model1 <- dismo::gbm.step(data = BLUM_filtered, gbm.x = c(4:6, 8:13),
                              gbm.y = 14, family = "bernoulli",
                              tree.complexity = 5, bag.fraction = 0.5, learning.rate = 0.005,
                              n.folds = 5)
saveRDS(BLUM_model1, "Output/Models/BLUM_model1.rds") # save the model
#BLUM_model0 <- readRDS("Output/Models/BLUM_model1.rds") # load the model

summary(BLUM_model1) # get the relative importance of each of the predictors

# Plot predictors
pdf(file = "Figures/BLUM/BLUM_Model1_SmoothPredictors.pdf", width = 10, height = 8)
gbm.plot(BLUM_model1, n.plots=9, plot.layout=c(3, 3), write.title = FALSE)
dev.off()

pdf(file = "Figures/BLUM/BLUM_Model1_Predictors.pdf", width = 10, height = 8)
gbm.plot.fits(BLUM_model1)
dev.off()

# Check this for ROC metrics: https://stackoverflow.com/questions/22391547/roc-score-in-gbm-package
# CV AUC of ROC (Receiver Operating Characteristic Curve)
BLUM_model1$cv.statistics$discrimination.mean # AUC Score

######################################
## Predict: Historical (restricted) ##
######################################
# What are the min and max latitudes?
min(BLUM_filtered$latitude)
max(BLUM_filtered$latitude)

# Let's restrict the predictions there
BLUM_restrict <- BLUM_predict %>% 
  dplyr::filter(latitude >= min(BLUM_filtered$latitude) & latitude <= max(BLUM_filtered$latitude))

BLUM_filtered$model <- BLUM_model1$fitted # fitted values for the model
preds <- dismo::predict(BLUM_model1, BLUM_restrict, n.trees = BLUM_model1$gbm.call$best.trees, type = "response") # Predicted values
BLUM_restrict$model <- preds

BLUM_restrict_joined <- BLUM_predict %>% 
  dplyr::left_join(., BLUM_restrict)

model <- dplyr::bind_rows(BLUM_filtered, BLUM_restrict_joined) %>% 
  dplyr::arrange(cellID) %>%  # Make sure everything is arranged by cellID
  dplyr::select(model) %>% 
  pull()

ggBLUM <- grid_BLUM %>% 
  dplyr::arrange(cellID) %>% 
  dplyr::bind_cols(., model = model) %>% 
  dplyr::select(cellID, ocean, latitude, model, geometry) %>% 
  sf::st_as_sf(sf_column_name = "geometry")

ggBLUM_abun <- grid_BLUM %>%
  dplyr::mutate(abundance_presence = ifelse(abundance > 0, yes = as.factor(1), no = NA)) %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  sf::st_centroid()

ggmodel <- plotModel(ggBLUM, ggBLUM_abun) + # Plot the model
  ggtitle("Blue marlin (1956-1981): Latitude and longitude (restricted range + grid search); AUC: 0.86") +
  geom_hline(yintercept = 5821011, color = "red") +
  geom_hline(yintercept = -5821011, color = "red")
ggsave(plot = ggmodel, filename = "Figures/BLUM/BLUM_Model1a.png", width = 15, height = 8, dpi = 300)

######################################
## Predict: Present (restricted) ##
######################################
BLUM_present <- read_csv(file = "Output/CSV/BLUM_present_full.csv") %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry)

BLUM_present_restricted <- BLUM_present %>% 
  dplyr::filter(latitude >= min(BLUM_filtered$latitude) & latitude <= max(BLUM_filtered$latitude))

BLUM_present_left <- BLUM_present %>% 
  dplyr::filter(!(latitude >= min(BLUM_filtered$latitude) & latitude <= max(BLUM_filtered$latitude)))

preds <- dismo::predict(BLUM_model1, BLUM_present_restricted, n.trees = BLUM_model1$gbm.call$best.trees, type = "response") # Predicted values
BLUM_present_restricted$model <- preds

model <- dplyr::bind_rows(BLUM_present_left, BLUM_present_restricted) %>% 
  dplyr::arrange(cellID) %>%  # Make sure everything is arranged by cellID
  dplyr::select(model) %>% 
  pull()

ggBLUM <- grid_BLUM %>% 
  dplyr::bind_cols(., model = model) %>% 
  dplyr::select(cellID, ocean, model, geometry) %>% 
  sf::st_as_sf(sf_column_name = "geometry")

ggmodel <- plotModel(ggBLUM, ggBLUM_abun) + # Plot the model
  ggtitle("Blue marlin (2017-2026): Latitude and longitude (restricted range + grid search)") +
  geom_hline(yintercept = 5821011, color = "red") +
  geom_hline(yintercept = -5821011, color = "red")
ggsave(plot = ggmodel, filename = "Figures/BLUM/BLUM_Model1b.png", width = 15, height = 8, dpi = 300)

#############################################
## Predict: Mid-century (restricted) ##
#############################################
BLUM_mid <- read_csv(file = "Output/CSV/BLUM_midCentury_full.csv") %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry)

BLUM_mid_restricted <- BLUM_mid %>% 
  dplyr::filter(latitude >= min(BLUM_filtered$latitude) & latitude <= max(BLUM_filtered$latitude))

BLUM_mid_left <- BLUM_mid %>% 
  dplyr::filter(!(latitude >= min(BLUM_filtered$latitude) & latitude <= max(BLUM_filtered$latitude)))

preds <- dismo::predict(BLUM_model1, BLUM_mid_restricted, n.trees = BLUM_model1$gbm.call$best.trees, type = "response") # Predicted values
BLUM_mid_restricted$model <- preds

model <- dplyr::bind_rows(BLUM_mid_left, BLUM_mid_restricted) %>% 
  dplyr::arrange(cellID) %>%  # Make sure everything is arranged by cellID
  dplyr::select(model) %>% 
  pull()

ggBLUM <- grid_BLUM %>% 
  dplyr::bind_cols(., model = model) %>% 
  dplyr::select(cellID, ocean, model, geometry) %>% 
  sf::st_as_sf(sf_column_name = "geometry")

ggmodel <- plotModel(ggBLUM, ggBLUM_abun) + # Plot the model
  ggtitle("Blue marlin (2046-2055): Latitude and longitude (restricted range + grid search)") +
  geom_hline(yintercept = 5821011, color = "red") +
  geom_hline(yintercept = -5821011, color = "red")
ggsave(plot = ggmodel, filename = "Figures/BLUM/BLUM_Model1c.png", width = 15, height = 8, dpi = 300)

################################################
## Predict: End of the century (restricted) ##
################################################
BLUM_end <- read_csv(file = "Output/CSV/BLUM_endCentury_full.csv") %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry)

BLUM_end_restricted <- BLUM_end %>% 
  dplyr::filter(latitude >= min(BLUM_filtered$latitude) & latitude <= max(BLUM_filtered$latitude))

BLUM_end_left <- BLUM_end %>% 
  dplyr::filter(!(latitude >= min(BLUM_filtered$latitude) & latitude <= max(BLUM_filtered$latitude)))

preds <- dismo::predict(BLUM_model1, BLUM_end_restricted, n.trees = BLUM_model1$gbm.call$best.trees, type = "response") # Predicted values
BLUM_end_restricted$model <- preds

model <- dplyr::bind_rows(BLUM_end_left, BLUM_end_restricted) %>% 
  dplyr::arrange(cellID) %>%  # Make sure everything is arranged by cellID
  dplyr::select(model) %>% 
  pull()

ggBLUM <- grid_BLUM %>% 
  dplyr::bind_cols(., model = model) %>% 
  dplyr::select(cellID, ocean, model, geometry) %>% 
  sf::st_as_sf(sf_column_name = "geometry")

ggmodel <- plotModel(ggBLUM, ggBLUM_abun) + # Plot the model
  ggtitle("Blue marlin (2091-2100): Latitude and longitude (restricted range + grid search)") +
  geom_hline(yintercept = 5821011, color = "red") +
  geom_hline(yintercept = -5821011, color = "red")
ggsave(plot = ggmodel, filename = "Figures/BLUM/BLUM_Model1d.png", width = 15, height = 8, dpi = 300)
