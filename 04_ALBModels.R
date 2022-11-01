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

# Load albacore full dataset
ALB_ds <- read_csv("Output/CSV/ALB_historical_full.csv", show_col_types = FALSE)

# Always build models with known data only
ALB_filtered <- ALB_ds %>% 
  dplyr::filter(!is.na(abundance)) %>% # filter those with data!
  dplyr::mutate(across(where(is.character), ~factor(.))) %>% # convert all characters to factors
  dplyr::mutate(abundance_presence = case_when(abundance > 0 ~ 1,
                                               abundance == 0 ~ 0)) %>%  # mutate the abundance data into 1s and 0s
  dplyr::select(-geometry) %>% 
  as.data.frame() #gbm.step doesn't work if it's a tibble...

# Data.frame for predictions
ALB_predict <- ALB_ds %>% 
  dplyr::filter(is.na(abundance)) %>% # filter those with data!
  dplyr::mutate(across(where(is.character), ~factor(.))) %>% # convert all characters to factors
  dplyr::mutate(abundance_presence = case_when(abundance > 0 ~ 1,
                                               abundance == 0 ~ 0)) %>%  # mutate the abundance data into 1s and 0s
  dplyr::select(-geometry) %>% 
  as.data.frame() #gbm.step doesn't work if it's a tibble...

# check the index numbers of the columns
colnames(ALB_filtered)

###########################
## Grid search ##
###########################
CVGrid <- CVgridSearch(ALB_filtered, 5, tc = c(2, 3, 5), bf = c(0.5, 0.75), lr = 0.005)

print(CVGrid %>% dplyr::arrange(cv_deviance))
# constant learning rate of 0.005

#########################################
## Build model: Lat & Lon (restricted) ##
#########################################
ALB_model1 <- dismo::gbm.step(data = ALB_filtered, gbm.x = c(4:6, 8:13),
                              gbm.y = 14, family = "bernoulli",
                              tree.complexity = 5, bag.fraction = 0.5, learning.rate = 0.005,
                              n.folds = 5)
saveRDS(ALB_model1, "Output/Models/ALB_model1.rds") # save the model
#ALB_model0 <- readRDS("Output/Models/ALB_model1.rds") # load the model

summary(ALB_model1) # get the relative importance of each of the predictors

# Plot predictors
pdf(file = "Figures/ALB/ALB_Model1_SmoothPredictors.pdf", width = 10, height = 8)
gbm.plot(ALB_model1, n.plots=9, plot.layout=c(3, 3), write.title = FALSE)
dev.off()

pdf(file = "Figures/ALB/ALB_Model1_Predictors.pdf", width = 10, height = 8)
gbm.plot.fits(ALB_model1)
dev.off()

# Check this for ROC metrics: https://stackoverflow.com/questions/22391547/roc-score-in-gbm-package
# CV AUC of ROC (Receiver Operating Characteristic Curve)
ALB_model1$cv.statistics$discrimination.mean # AUC Score

######################################
## Predict: Historical (restricted) ##
######################################
# What are the min and max latitudes?
min(ALB_filtered$latitude)
max(ALB_filtered$latitude)

# Let's restrict the predictions there
ALB_restrict <- ALB_predict %>% 
  dplyr::filter(latitude >= min(ALB_filtered$latitude) & latitude <= max(ALB_filtered$latitude))

ALB_filtered$model <- ALB_model1$fitted # fitted values for the model
preds <- dismo::predict(ALB_model1, ALB_restrict, n.trees = ALB_model1$gbm.call$best.trees, type = "response") # Predicted values
ALB_restrict$model <- preds

ALB_restrict_joined <- ALB_predict %>% 
  dplyr::left_join(., ALB_restrict)

model <- dplyr::bind_rows(ALB_filtered, ALB_restrict_joined) %>% 
  dplyr::arrange(cellID) %>%  # Make sure everything is arranged by cellID
  dplyr::select(model) %>% 
  pull()

ggALB <- grid_ALB %>% 
  dplyr::arrange(cellID) %>% 
  dplyr::bind_cols(., model = model) %>% 
  dplyr::select(cellID, ocean, latitude, model, geometry) %>% 
  sf::st_as_sf(sf_column_name = "geometry")

ggALB_abun <- grid_ALB %>%
  dplyr::mutate(abundance_presence = ifelse(abundance > 0, yes = as.factor(1), no = NA)) %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  sf::st_centroid()

ggmodel <- plotModel(ggALB, ggALB_abun) + # Plot the model
  ggtitle("Albacore (1956-1981): Latitude and longitude (restricted range + grid search); AUC: 0.90") +
  geom_hline(yintercept = 5821011, color = "red") +
  geom_hline(yintercept = -5821011, color = "red")
ggsave(plot = ggmodel, filename = "Figures/ALB/ALB_Model1a.png", width = 15, height = 8, dpi = 300)

######################################
## Predict: Present (restricted) ##
######################################
ALB_present <- read_csv(file = "Output/CSV/ALB_present_full.csv") %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry)

ALB_present_restricted <- ALB_present %>% 
  dplyr::filter(latitude >= min(ALB_filtered$latitude) & latitude <= max(ALB_filtered$latitude))

ALB_present_left <- ALB_present %>% 
  dplyr::filter(!(latitude >= min(ALB_filtered$latitude) & latitude <= max(ALB_filtered$latitude)))

preds <- dismo::predict(ALB_model1, ALB_present_restricted, n.trees = ALB_model1$gbm.call$best.trees, type = "response") # Predicted values
ALB_present_restricted$model <- preds

model <- dplyr::bind_rows(ALB_present_left, ALB_present_restricted) %>% 
  dplyr::arrange(cellID) %>%  # Make sure everything is arranged by cellID
  dplyr::select(model) %>% 
  pull()

ggALB <- grid_ALB %>% 
  dplyr::bind_cols(., model = model) %>% 
  dplyr::select(cellID, ocean, model, geometry) %>% 
  sf::st_as_sf(sf_column_name = "geometry")

ggmodel <- plotModel(ggALB, ggALB_abun) + # Plot the model
  ggtitle("Albacore (2017-2026): Latitude and longitude (restricted range + grid search)") +
  geom_hline(yintercept = 5821011, color = "red") +
  geom_hline(yintercept = -5821011, color = "red")
ggsave(plot = ggmodel, filename = "Figures/ALB/ALB_Model1b.png", width = 15, height = 8, dpi = 300)

#############################################
## Predict: Mid-century (restricted) ##
#############################################
ALB_mid <- read_csv(file = "Output/CSV/ALB_midCentury_full.csv") %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry)

ALB_mid_restricted <- ALB_mid %>% 
  dplyr::filter(latitude >= min(ALB_filtered$latitude) & latitude <= max(ALB_filtered$latitude))

ALB_mid_left <- ALB_mid %>% 
  dplyr::filter(!(latitude >= min(ALB_filtered$latitude) & latitude <= max(ALB_filtered$latitude)))

preds <- dismo::predict(ALB_model1, ALB_mid_restricted, n.trees = ALB_model1$gbm.call$best.trees, type = "response") # Predicted values
ALB_mid_restricted$model <- preds

model <- dplyr::bind_rows(ALB_mid_left, ALB_mid_restricted) %>% 
  dplyr::arrange(cellID) %>%  # Make sure everything is arranged by cellID
  dplyr::select(model) %>% 
  pull()

ggALB <- grid_ALB %>% 
  dplyr::bind_cols(., model = model) %>% 
  dplyr::select(cellID, ocean, model, geometry) %>% 
  sf::st_as_sf(sf_column_name = "geometry")

ggmodel <- plotModel(ggALB, ggALB_abun) + # Plot the model
  ggtitle("Albacore (2046-2055): Latitude and longitude (restricted range + grid search)") +
  geom_hline(yintercept = 5821011, color = "red") +
  geom_hline(yintercept = -5821011, color = "red")
ggsave(plot = ggmodel, filename = "Figures/ALB/ALB_Model1c.png", width = 15, height = 8, dpi = 300)

################################################
## Predict: End of the century (restricted) ##
################################################
ALB_end <- read_csv(file = "Output/CSV/ALB_endCentury_full.csv") %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry)

ALB_end_restricted <- ALB_end %>% 
  dplyr::filter(latitude >= min(ALB_filtered$latitude) & latitude <= max(ALB_filtered$latitude))

ALB_end_left <- ALB_end %>% 
  dplyr::filter(!(latitude >= min(ALB_filtered$latitude) & latitude <= max(ALB_filtered$latitude)))

preds <- dismo::predict(ALB_model1, ALB_end_restricted, n.trees = ALB_model1$gbm.call$best.trees, type = "response") # Predicted values
ALB_end_restricted$model <- preds

model <- dplyr::bind_rows(ALB_end_left, ALB_end_restricted) %>% 
  dplyr::arrange(cellID) %>%  # Make sure everything is arranged by cellID
  dplyr::select(model) %>% 
  pull()

ggALB <- grid_ALB %>% 
  dplyr::bind_cols(., model = model) %>% 
  dplyr::select(cellID, ocean, model, geometry) %>% 
  sf::st_as_sf(sf_column_name = "geometry")

ggmodel <- plotModel(ggALB, ggALB_abun) + # Plot the model
  ggtitle("Albacore (2091-2100): Latitude and longitude (restricted range + grid search)") +
  geom_hline(yintercept = 5821011, color = "red") +
  geom_hline(yintercept = -5821011, color = "red")
ggsave(plot = ggmodel, filename = "Figures/ALB/ALB_Model1d.png", width = 15, height = 8, dpi = 300)
