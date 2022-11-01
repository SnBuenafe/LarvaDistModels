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

# Load frigate tuna full dataset
FRI_ds <- read_csv("Output/CSV/FRI_historical_full.csv", show_col_types = FALSE)

# Always build models with known data only
FRI_filtered <- FRI_ds %>% 
  dplyr::filter(!is.na(abundance)) %>% # filter those with data!
  dplyr::mutate(across(where(is.character), ~factor(.))) %>% # convert all characters to factors
  dplyr::mutate(abundance_presence = case_when(abundance > 0 ~ 1,
                                               abundance == 0 ~ 0)) %>%  # mutate the abundance data into 1s and 0s
  dplyr::select(-geometry) %>% 
  as.data.frame() #gbm.step doesn't work if it's a tibble...

# Data.frame for predictions
FRI_predict <- FRI_ds %>% 
  dplyr::filter(is.na(abundance)) %>% # filter those with data!
  dplyr::mutate(across(where(is.character), ~factor(.))) %>% # convert all characters to factors
  dplyr::mutate(abundance_presence = case_when(abundance > 0 ~ 1,
                                               abundance == 0 ~ 0)) %>%  # mutate the abundance data into 1s and 0s
  dplyr::select(-geometry) %>% 
  as.data.frame() #gbm.step doesn't work if it's a tibble...

# check the index numbers of the columns
colnames(FRI_filtered)

###########################
## Grid search ##
###########################
CVGrid <- CVgridSearch(FRI_filtered, 5, tc = c(2, 3, 5), bf = c(0.5, 0.75), lr = 0.005)

print(CVGrid %>% dplyr::arrange(cv_deviance))
# constant learning rate of 0.005

#########################################
## Build model: Lat & Lon (restricted) ##
#########################################
FRI_model1 <- dismo::gbm.step(data = FRI_filtered, gbm.x = c(4:6, 8:13),
                               gbm.y = 14, family = "bernoulli",
                               tree.complexity = 5, bag.fraction = 0.5, learning.rate = 0.005,
                               n.folds = 5)
saveRDS(FRI_model1, "Output/Models/FRI_model1.rds") # save the model
#FRI_model0 <- readRDS("Output/Models/FRI_model1.rds") # load the model

summary(FRI_model1) # get the relative importance of each of the predictors

# Plot predictors
pdf(file = "Figures/FRI/FRI_Model1_SmoothPredictors.pdf", width = 10, height = 8)
gbm.plot(FRI_model1, n.plots=9, plot.layout=c(3, 3), write.title = FALSE)
dev.off()

pdf(file = "Figures/FRI/FRI_Model1_Predictors.pdf", width = 10, height = 8)
gbm.plot.fits(FRI_model1)
dev.off()

# Check this for ROC metrics: https://stackoverflow.com/questions/22391547/roc-score-in-gbm-package
# CV AUC of ROC (Receiver Operating Characteristic Curve)
FRI_model1$cv.statistics$discrimination.mean # AUC Score

######################################
## Predict: Historical (restricted) ##
######################################
# What are the min and max latitudes?
min(FRI_filtered$latitude)
max(FRI_filtered$latitude)

# Let's restrict the predictions there
FRI_restrict <- FRI_predict %>% 
  dplyr::filter(latitude >= min(FRI_filtered$latitude) & latitude <= max(FRI_filtered$latitude))

FRI_filtered$model <- FRI_model1$fitted # fitted values for the model
preds <- dismo::predict(FRI_model1, FRI_restrict, n.trees = FRI_model1$gbm.call$best.trees, type = "response") # Predicted values
FRI_restrict$model <- preds

FRI_restrict_joined <- FRI_predict %>% 
  dplyr::left_join(., FRI_restrict)

model <- dplyr::bind_rows(FRI_filtered, FRI_restrict_joined) %>% 
  dplyr::arrange(cellID) %>%  # Make sure everything is arranged by cellID
  dplyr::select(model) %>% 
  pull()

ggFRI <- grid_FRI %>% 
  dplyr::arrange(cellID) %>% 
  dplyr::bind_cols(., model = model) %>% 
  dplyr::select(cellID, ocean, latitude, model, geometry) %>% 
  sf::st_as_sf(sf_column_name = "geometry")

ggFRI_abun <- grid_FRI %>%
  dplyr::mutate(abundance_presence = ifelse(abundance > 0, yes = as.factor(1), no = NA)) %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  sf::st_centroid()

ggmodel <- plotModel(ggFRI, ggFRI_abun) + # Plot the model
  ggtitle("Shortbill spearfish (1956-1981): Latitude and longitude (restricted range + grid search); AUC: 0.86") +
  geom_hline(yintercept = 5821011, color = "red") +
  geom_hline(yintercept = -5821011, color = "red")
ggsave(plot = ggmodel, filename = "Figures/FRI/FRI_Model1a.png", width = 15, height = 8, dpi = 300)

######################################
## Predict: Present (restricted) ##
######################################
FRI_present <- read_csv(file = "Output/CSV/FRI_present_full.csv") %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry)

FRI_present_restricted <- FRI_present %>% 
  dplyr::filter(latitude >= min(FRI_filtered$latitude) & latitude <= max(FRI_filtered$latitude))

FRI_present_left <- FRI_present %>% 
  dplyr::filter(!(latitude >= min(FRI_filtered$latitude) & latitude <= max(FRI_filtered$latitude)))

preds <- dismo::predict(FRI_model1, FRI_present_restricted, n.trees = FRI_model1$gbm.call$best.trees, type = "response") # Predicted values
FRI_present_restricted$model <- preds

model <- dplyr::bind_rows(FRI_present_left, FRI_present_restricted) %>% 
  dplyr::arrange(cellID) %>%  # Make sure everything is arranged by cellID
  dplyr::select(model) %>% 
  pull()

ggFRI <- grid_FRI %>% 
  dplyr::bind_cols(., model = model) %>% 
  dplyr::select(cellID, ocean, model, geometry) %>% 
  sf::st_as_sf(sf_column_name = "geometry")

ggmodel <- plotModel(ggFRI, ggFRI_abun) + # Plot the model
  ggtitle("Shortbill spearfish (2017-2026): Latitude and longitude (restricted range + grid search)") +
  geom_hline(yintercept = 5821011, color = "red") +
  geom_hline(yintercept = -5821011, color = "red")
ggsave(plot = ggmodel, filename = "Figures/FRI/FRI_Model1b.png", width = 15, height = 8, dpi = 300)

#############################################
## Predict: Mid-century (restricted) ##
#############################################
FRI_mid <- read_csv(file = "Output/CSV/FRI_midCentury_full.csv") %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry)

FRI_mid_restricted <- FRI_mid %>% 
  dplyr::filter(latitude >= min(FRI_filtered$latitude) & latitude <= max(FRI_filtered$latitude))

FRI_mid_left <- FRI_mid %>% 
  dplyr::filter(!(latitude >= min(FRI_filtered$latitude) & latitude <= max(FRI_filtered$latitude)))

preds <- dismo::predict(FRI_model1, FRI_mid_restricted, n.trees = FRI_model1$gbm.call$best.trees, type = "response") # Predicted values
FRI_mid_restricted$model <- preds

model <- dplyr::bind_rows(FRI_mid_left, FRI_mid_restricted) %>% 
  dplyr::arrange(cellID) %>%  # Make sure everything is arranged by cellID
  dplyr::select(model) %>% 
  pull()

ggFRI <- grid_FRI %>% 
  dplyr::bind_cols(., model = model) %>% 
  dplyr::select(cellID, ocean, model, geometry) %>% 
  sf::st_as_sf(sf_column_name = "geometry")

ggmodel <- plotModel(ggFRI, ggFRI_abun) + # Plot the model
  ggtitle("Shortbill spearfish (2046-2055): Latitude and longitude (restricted range + grid search)") +
  geom_hline(yintercept = 5821011, color = "red") +
  geom_hline(yintercept = -5821011, color = "red")
ggsave(plot = ggmodel, filename = "Figures/FRI/FRI_Model1c.png", width = 15, height = 8, dpi = 300)

################################################
## Predict: End of the century (restricted) ##
################################################
FRI_end <- read_csv(file = "Output/CSV/FRI_endCentury_full.csv") %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry)

FRI_end_restricted <- FRI_end %>% 
  dplyr::filter(latitude >= min(FRI_filtered$latitude) & latitude <= max(FRI_filtered$latitude))

FRI_end_left <- FRI_end %>% 
  dplyr::filter(!(latitude >= min(FRI_filtered$latitude) & latitude <= max(FRI_filtered$latitude)))

preds <- dismo::predict(FRI_model1, FRI_end_restricted, n.trees = FRI_model1$gbm.call$best.trees, type = "response") # Predicted values
FRI_end_restricted$model <- preds

model <- dplyr::bind_rows(FRI_end_left, FRI_end_restricted) %>% 
  dplyr::arrange(cellID) %>%  # Make sure everything is arranged by cellID
  dplyr::select(model) %>% 
  pull()

ggFRI <- grid_FRI %>% 
  dplyr::bind_cols(., model = model) %>% 
  dplyr::select(cellID, ocean, model, geometry) %>% 
  sf::st_as_sf(sf_column_name = "geometry")

ggmodel <- plotModel(ggFRI, ggFRI_abun) + # Plot the model
  ggtitle("Shortbill spearfish (2091-2100): Latitude and longitude (restricted range + grid search)") +
  geom_hline(yintercept = 5821011, color = "red") +
  geom_hline(yintercept = -5821011, color = "red")
ggsave(plot = ggmodel, filename = "Figures/FRI/FRI_Model1d.png", width = 15, height = 8, dpi = 300)
