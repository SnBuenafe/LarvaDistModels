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
# Load southern bluefin tuna full dataset
SBFT_ds <- read_csv("Output/SBFT_full.csv", show_col_types = FALSE)

# Always build models with known data only
SBFT_filtered <- SBFT_ds %>% 
  dplyr::filter(!is.na(abundance)) %>% # filter those with data!
  dplyr::mutate(across(where(is.character), ~factor(.))) %>% # convert all characters to factors
  dplyr::mutate(abundance_presence = case_when(abundance > 0 ~ 1,
                                               abundance == 0 ~ 0)) %>%  # mutate the abundance data into 1s and 0s
  dplyr::select(-geometry) %>% 
  as.data.frame() #gbm.step doesn't work if it's a tibble...

# Data.frame for predictions
SBFT_predict <- SBFT_ds %>% 
  dplyr::filter(is.na(abundance)) %>% # filter those with data!
  dplyr::mutate(across(where(is.character), ~factor(.))) %>% # convert all characters to factors
  dplyr::mutate(abundance_presence = case_when(abundance > 0 ~ 1,
                                               abundance == 0 ~ 0)) %>%  # mutate the abundance data into 1s and 0s
  dplyr::select(-geometry) %>% 
  as.data.frame() #gbm.step doesn't work if it's a tibble...

# Build models
# check the index numbers of the columns
colnames(SBFT_filtered)
###########################
## Grid search ##
###########################
# Do a cross-validated grid search using gbm.fixed
# define a 5-fold cross-validation
CVGrid <- CVgridSearch(SBFT_filtered, 5, tc = c(1, 2, 3, 5), bf = c(0.5, 0.75), lr = 0.005)

print(CVGrid %>% dplyr::arrange(cv_deviance))
###########################
## Fit model ##
###########################
# Now build the model; we want to have a lot of trees: preferably at least 1,000 trees
SBFT_model1 <- gbm.step(SBFT_filtered, gbm.x = c(4, 6, 8:13), gbm.y = 14, 
                       learning.rate = 0.005,
                       bag.fraction = 0.75,
                       tree.complexity = 5
)
saveRDS(SBFT_model1, "Output/SBFT_model1.rds") # save the model
SBFT_model1 <- readRDS("Output/SBFT_model1.rds") # load the model

summary(SBFT_model1) # get the relative importance of each of the predictors

# Plot predictors
pdf(file = "Figures/SBFT_Model1_SmoothPredictors.pdf", width = 10, height = 8)
gbm.plot(SBFT_model1, n.plots=8, plot.layout=c(3, 3), write.title = FALSE)
dev.off()

pdf(file = "Figures/SBFT_Model1_Predictors.pdf", width = 10, height = 8)
gbm.plot.fits(SBFT_model1)
dev.off()

# Check this for ROC metrics: https://stackoverflow.com/questions/22391547/roc-score-in-gbm-package
# CV AUC of ROC (Receiver Operating Characteristic Curve)
SBFT_model1$cv.statistics$discrimination.mean # AUC Score

# Check the plot
SBFT_filtered$model <- SBFT_model1$fitted
SBFT_sf <- grid %>% # convert to sf so we can plot
  dplyr::left_join(SBFT_filtered, ., by = "cellID") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

plotModel(SBFT_sf, "Figures/SBFT_Model1.png") # Plot the model
plotAbundance(SBFT_sf, "Figures/SBFT_abundance.png") # Plot raw
pres <- round(sum(SBFT_sf$abundance_presence)/nrow(SBFT_sf) * 100, 2)
plotPA(SBFT_sf, inset, pres, "Figures/SBFT_presabs.png") # Plot presence absence

###############################
## Predict for other points ##
###############################
preds <- dismo::predict(SBFT_model1, SBFT_predict, n.trees = SBFT_model1$gbm.call$best.trees, type = "response")
SBFT_predict$predictions <- preds

SBFT_predict_sf <- grid_SBFT %>% # convert to sf so we can plot
  dplyr::select(-species, -abundance, -season, -longitude, -latitude) %>% 
  dplyr::left_join(SBFT_predict, ., by = "cellID") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

plotPredictions(SBFT_predict_sf, "Figures/SBFT_Model1preds.png") # Plot predictions
