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
# Load striped marlin and white marlin full dataset
STRM_ds <- read_csv("Output/STRM_full.csv", show_col_types = FALSE)

# Always build models with known data only
STRM_filtered <- STRM_ds %>% 
  dplyr::filter(!is.na(abundance)) %>% # filter those with data!
  dplyr::mutate(across(where(is.character), ~factor(.))) %>% # convert all characters to factors
  dplyr::mutate(abundance_presence = case_when(abundance > 0 ~ 1,
                                               abundance == 0 ~ 0)) %>%  # mutate the abundance data into 1s and 0s
  dplyr::select(-geometry) %>% 
  as.data.frame() #gbm.step doesn't work if it's a tibble...

# Data.frame for predictions
STRM_predict <- STRM_ds %>% 
  dplyr::filter(is.na(abundance)) %>% # filter those with data!
  dplyr::mutate(across(where(is.character), ~factor(.))) %>% # convert all characters to factors
  dplyr::mutate(abundance_presence = case_when(abundance > 0 ~ 1,
                                               abundance == 0 ~ 0)) %>%  # mutate the abundance data into 1s and 0s
  dplyr::select(-geometry) %>% 
  as.data.frame() #gbm.step doesn't work if it's a tibble...

# Build models
# check the index numbers of the columns
colnames(STRM_filtered)
###########################
## Grid search ##
###########################
# Do a cross-validated grid search using gbm.fixed
# define a 5-fold cross-validation
CVGrid <- CVgridSearch(STRM_filtered, 5, tc = c(1, 2, 3, 5), bf = c(0.5, 0.75), lr = 0.005)

print(CVGrid %>% dplyr::arrange(cv_deviance))
###########################
## Fit model ##
###########################
# Now build the model; we want to have a lot of trees: preferably at least 1,000 trees
STRM_model1 <- gbm.step(STRM_filtered, gbm.x = c(4, 6, 8:13), gbm.y = 14, 
                       learning.rate = 0.005,
                       bag.fraction = 0.5,
                       tree.complexity = 5
)
saveRDS(STRM_model1, "Output/STRM_model1.rds") # save the model
STRM_model1 <- readRDS("Output/STRM_model1.rds") # load the model

summary(STRM_model1) # get the relative importance of each of the predictors

# Plot predictors
pdf(file = "Figures/STRM_Model1_SmoothPredictors.pdf", width = 10, height = 8)
gbm.plot(STRM_model1, n.plots=8, plot.layout=c(3, 3), write.title = FALSE)
dev.off()

pdf(file = "Figures/STRM_Model1_Predictors.pdf", width = 10, height = 8)
gbm.plot.fits(STRM_model1)
dev.off()

# Check this for ROC metrics: https://stackoverflow.com/questions/22391547/roc-score-in-gbm-package
# CV AUC of ROC (Receiver Operating Characteristic Curve)
STRM_model1$cv.statistics$discrimination.mean # AUC Score

# Check the plot
STRM_filtered$model <- STRM_model1$fitted
STRM_sf <- grid %>% # convert to sf so we can plot
  dplyr::left_join(STRM_filtered, ., by = "cellID") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

plotModel(STRM_sf, "Figures/STRM_Model1.png") # Plot the model
plotAbundance(STRM_sf, "Figures/STRM_abundance.png") # Plot raw
pres <- round(sum(STRM_sf$abundance_presence)/nrow(STRM_sf) * 100, 2)
plotPA(STRM_sf, pres, "Figures/STRM_presabs.png") # Plot presence absence

###############################
## Predict for other points ##
###############################
preds <- dismo::predict(STRM_model1, STRM_predict, n.trees = STRM_model1$gbm.call$best.trees, type = "response")
STRM_predict$predictions <- preds

STRM_predict_sf <- grid_STRM %>% # convert to sf so we can plot
  dplyr::select(-species, -abundance, -season, -longitude, -latitude) %>% 
  dplyr::left_join(STRM_predict, ., by = "cellID") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

plotPredictions(STRM_predict_sf, "Figures/STRM_Model1preds.png") # Plot predictions
