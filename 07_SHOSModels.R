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

# Load shortbill spearfish full dataset
SHOS_ds <- read_csv("Output/CSV/SHOS_historical_full.csv", show_col_types = FALSE)

# Always build models with known data only
SHOS_filtered <- SHOS_ds %>% 
  dplyr::filter(!is.na(abundance)) %>% # filter those with data!
  dplyr::mutate(across(where(is.character), ~factor(.))) %>% # convert all characters to factors
  dplyr::mutate(abundance_presence = case_when(abundance > 0 ~ 1,
                                               abundance == 0 ~ 0)) %>%  # mutate the abundance data into 1s and 0s
  dplyr::select(-geometry) %>% 
  as.data.frame() #gbm.step doesn't work if it's a tibble...

# Data.frame for predictions
SHOS_predict <- SHOS_ds %>% 
  dplyr::filter(is.na(abundance)) %>% # filter those with data!
  dplyr::mutate(across(where(is.character), ~factor(.))) %>% # convert all characters to factors
  dplyr::mutate(abundance_presence = case_when(abundance > 0 ~ 1,
                                               abundance == 0 ~ 0)) %>%  # mutate the abundance data into 1s and 0s
  dplyr::select(-geometry) %>% 
  as.data.frame() #gbm.step doesn't work if it's a tibble...

# check the index numbers of the columns
colnames(SHOS_filtered)

###########################
## Grid search ##
###########################
CVGrid <- CVgridSearch(SHOS_filtered, 5, tc = c(2, 3, 5), bf = c(0.5, 0.75), lr = 0.005)

print(CVGrid %>% dplyr::arrange(cv_deviance))
# constant learning rate of 0.005

#########################################
## Build model: Lat & Lon (restricted) ##
#########################################
SHOS_model1 <- dismo::gbm.step(data = SHOS_filtered, gbm.x = c(4:6, 8:13),
                               gbm.y = 14, family = "bernoulli",
                               tree.complexity = 5, bag.fraction = 0.5, learning.rate = 0.005,
                               n.folds = 5)
saveRDS(SHOS_model1, "Output/Models/SHOS_model1.rds") # save the model
#SHOS_model0 <- readRDS("Output/Models/SHOS_model1.rds") # load the model

summary(SHOS_model1) # get the relative importance of each of the predictors

# Plot predictors
pdf(file = "Figures/SHOS/SHOS_Model1_SmoothPredictors.pdf", width = 10, height = 8)
gbm.plot(SHOS_model1, n.plots=9, plot.layout=c(3, 3), write.title = FALSE)
dev.off()

pdf(file = "Figures/SHOS/SHOS_Model1_Predictors.pdf", width = 10, height = 8)
gbm.plot.fits(SHOS_model1)
dev.off()

# Check this for ROC metrics: https://stackoverflow.com/questions/22391547/roc-score-in-gbm-package
# CV AUC of ROC (Receiver Operating Characteristic Curve)
SHOS_model1$cv.statistics$discrimination.mean # AUC Score

######################################
## Predict: Historical (restricted) ##
######################################
# What are the min and max latitudes?
min(SHOS_filtered$latitude)
max(SHOS_filtered$latitude)

# Let's restrict the predictions there
SHOS_restrict <- SHOS_predict %>% 
  dplyr::filter(latitude >= min(SHOS_filtered$latitude) & latitude <= max(SHOS_filtered$latitude))

SHOS_filtered$model <- SHOS_model1$fitted # fitted values for the model
preds <- dismo::predict(SHOS_model1, SHOS_restrict, n.trees = SHOS_model1$gbm.call$best.trees, type = "response") # Predicted values
SHOS_restrict$model <- preds

SHOS_restrict_joined <- SHOS_predict %>% 
  dplyr::left_join(., SHOS_restrict)

model <- dplyr::bind_rows(SHOS_filtered, SHOS_restrict_joined) %>% 
  dplyr::arrange(cellID) %>%  # Make sure everything is arranged by cellID
  dplyr::select(model) %>% 
  pull()

ggSHOS <- grid_SHOS %>% 
  dplyr::arrange(cellID) %>% 
  dplyr::bind_cols(., model = model) %>% 
  dplyr::select(cellID, ocean, latitude, model, geometry) %>% 
  sf::st_as_sf(sf_column_name = "geometry")

ggSHOS_abun <- grid_SHOS %>%
  dplyr::mutate(abundance_presence = ifelse(abundance > 0, yes = as.factor(1), no = NA)) %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  sf::st_centroid()

ggmodel <- plotModel(ggSHOS, ggSHOS_abun) + # Plot the model
  ggtitle("Shortbill spearfish (1956-1981): Latitude and longitude (restricted range + grid search); AUC: 0.86") +
  geom_hline(yintercept = 5821011, color = "red") +
  geom_hline(yintercept = -5821011, color = "red")
ggsave(plot = ggmodel, filename = "Figures/SHOS/SHOS_Model1a.png", width = 15, height = 8, dpi = 300)

######################################
## Predict: Present (restricted) ##
######################################
SHOS_present <- read_csv(file = "Output/CSV/SHOS_present_full.csv") %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry)

SHOS_present_restricted <- SHOS_present %>% 
  dplyr::filter(latitude >= min(SHOS_filtered$latitude) & latitude <= max(SHOS_filtered$latitude))

SHOS_present_left <- SHOS_present %>% 
  dplyr::filter(!(latitude >= min(SHOS_filtered$latitude) & latitude <= max(SHOS_filtered$latitude)))

preds <- dismo::predict(SHOS_model1, SHOS_present_restricted, n.trees = SHOS_model1$gbm.call$best.trees, type = "response") # Predicted values
SHOS_present_restricted$model <- preds

model <- dplyr::bind_rows(SHOS_present_left, SHOS_present_restricted) %>% 
  dplyr::arrange(cellID) %>%  # Make sure everything is arranged by cellID
  dplyr::select(model) %>% 
  pull()

ggSHOS <- grid_SHOS %>% 
  dplyr::bind_cols(., model = model) %>% 
  dplyr::select(cellID, ocean, model, geometry) %>% 
  sf::st_as_sf(sf_column_name = "geometry")

ggmodel <- plotModel(ggSHOS, ggSHOS_abun) + # Plot the model
  ggtitle("Shortbill spearfish (2017-2026): Latitude and longitude (restricted range + grid search)") +
  geom_hline(yintercept = 5821011, color = "red") +
  geom_hline(yintercept = -5821011, color = "red")
ggsave(plot = ggmodel, filename = "Figures/SHOS/SHOS_Model1b.png", width = 15, height = 8, dpi = 300)

#############################################
## Predict: Mid-century (restricted) ##
#############################################
SHOS_mid <- read_csv(file = "Output/CSV/SHOS_midCentury_full.csv") %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry)

SHOS_mid_restricted <- SHOS_mid %>% 
  dplyr::filter(latitude >= min(SHOS_filtered$latitude) & latitude <= max(SHOS_filtered$latitude))

SHOS_mid_left <- SHOS_mid %>% 
  dplyr::filter(!(latitude >= min(SHOS_filtered$latitude) & latitude <= max(SHOS_filtered$latitude)))

preds <- dismo::predict(SHOS_model1, SHOS_mid_restricted, n.trees = SHOS_model1$gbm.call$best.trees, type = "response") # Predicted values
SHOS_mid_restricted$model <- preds

model <- dplyr::bind_rows(SHOS_mid_left, SHOS_mid_restricted) %>% 
  dplyr::arrange(cellID) %>%  # Make sure everything is arranged by cellID
  dplyr::select(model) %>% 
  pull()

ggSHOS <- grid_SHOS %>% 
  dplyr::bind_cols(., model = model) %>% 
  dplyr::select(cellID, ocean, model, geometry) %>% 
  sf::st_as_sf(sf_column_name = "geometry")

ggmodel <- plotModel(ggSHOS, ggSHOS_abun) + # Plot the model
  ggtitle("Shortbill spearfish (2046-2055): Latitude and longitude (restricted range + grid search)") +
  geom_hline(yintercept = 5821011, color = "red") +
  geom_hline(yintercept = -5821011, color = "red")
ggsave(plot = ggmodel, filename = "Figures/SHOS/SHOS_Model1c.png", width = 15, height = 8, dpi = 300)

################################################
## Predict: End of the century (restricted) ##
################################################
SHOS_end <- read_csv(file = "Output/CSV/SHOS_endCentury_full.csv") %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry)

SHOS_end_restricted <- SHOS_end %>% 
  dplyr::filter(latitude >= min(SHOS_filtered$latitude) & latitude <= max(SHOS_filtered$latitude))

SHOS_end_left <- SHOS_end %>% 
  dplyr::filter(!(latitude >= min(SHOS_filtered$latitude) & latitude <= max(SHOS_filtered$latitude)))

preds <- dismo::predict(SHOS_model1, SHOS_end_restricted, n.trees = SHOS_model1$gbm.call$best.trees, type = "response") # Predicted values
SHOS_end_restricted$model <- preds

model <- dplyr::bind_rows(SHOS_end_left, SHOS_end_restricted) %>% 
  dplyr::arrange(cellID) %>%  # Make sure everything is arranged by cellID
  dplyr::select(model) %>% 
  pull()

ggSHOS <- grid_SHOS %>% 
  dplyr::bind_cols(., model = model) %>% 
  dplyr::select(cellID, ocean, model, geometry) %>% 
  sf::st_as_sf(sf_column_name = "geometry")

ggmodel <- plotModel(ggSHOS, ggSHOS_abun) + # Plot the model
  ggtitle("Shortbill spearfish (2091-2100): Latitude and longitude (restricted range + grid search)") +
  geom_hline(yintercept = 5821011, color = "red") +
  geom_hline(yintercept = -5821011, color = "red")
ggsave(plot = ggmodel, filename = "Figures/SHOS/SHOS_Model1d.png", width = 15, height = 8, dpi = 300)
