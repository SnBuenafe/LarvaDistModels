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
# Load bonitos full dataset
BON_ds <- read_csv("Output/BON_full.csv", show_col_types = FALSE)

# Always build models with known data only
BON_filtered <- BON_ds %>% 
  dplyr::filter(!is.na(abundance)) %>% # filter those with data!
  dplyr::mutate(across(where(is.character), ~factor(.))) %>% # convert all characters to factors
  dplyr::mutate(abundance_presence = case_when(abundance > 0 ~ 1,
                                               abundance == 0 ~ 0)) %>%  # mutate the abundance data into 1s and 0s
  dplyr::select(-geometry) %>% 
  as.data.frame() #gbm.step doesn't work if it's a tibble...

# Data.frame for predictions
BON_predict <- BON_ds %>% 
  dplyr::filter(is.na(abundance)) %>% # filter those with data!
  dplyr::mutate(across(where(is.character), ~factor(.))) %>% # convert all characters to factors
  dplyr::mutate(abundance_presence = case_when(abundance > 0 ~ 1,
                                               abundance == 0 ~ 0)) %>%  # mutate the abundance data into 1s and 0s
  dplyr::select(-geometry) %>% 
  as.data.frame() #gbm.step doesn't work if it's a tibble...

# Build models
# check the index numbers of the columns
colnames(BON_filtered)
###########################
## Grid search ##
###########################
# Do a cross-validated grid search using gbm.fixed
# define a 5-fold cross-validation
CVGrid <- CVgridSearch(BON_filtered, 5, tc = c(1, 2, 3, 5), bf = c(0.5, 0.75), lr = 0.005)

print(CVGrid %>% dplyr::arrange(cv_deviance))
###########################
## Fit model ##
###########################
# Now build the model; we want to have a lot of trees: preferably at least 1,000 trees
BON_model1 <- gbm.step(BON_filtered, gbm.x = c(4, 6, 8:13), gbm.y = 14, 
                       learning.rate = 0.005,
                       bag.fraction = 0.5,
                       tree.complexity = 3,
)
saveRDS(BON_model1, "Output/BON_model1.rds") # save the model
BON_model1 <- readRDS("Output/BON_model1.rds") # load the model

summary(BON_model1) # get the relative importance of each of the predictors

# Plot predictors
pdf(file = "Figures/BON_Model1_SmoothPredictors.pdf", width = 10, height = 8)
gbm.plot(BON_model1, n.plots=8, plot.layout=c(3, 3), write.title = FALSE)
dev.off()

pdf(file = "Figures/BON_Model1_Predictors.pdf", width = 10, height = 8)
gbm.plot.fits(BON_model1)
dev.off()

# Check this for ROC metrics: https://stackoverflow.com/questions/22391547/roc-score-in-gbm-package
# CV AUC of ROC (Receiver Operating Characteristic Curve)
BON_model1$cv.statistics$discrimination.mean # AUC Score

# Check the plot
BON_filtered$model <- BON_model1$fitted
BON_sf <- grid %>% # convert to sf so we can plot
  dplyr::left_join(BON_filtered, ., by = "cellID") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

plotModel(BON_sf, "Figures/BON_Model1.png") # Plot the model
plotAbundance(BON_sf, "Figures/BON_abundance.png") # Plot raw
pres <- round(sum(BON_sf$abundance_presence)/nrow(BON_sf) * 100, 2)
plotPA(BON_sf, pres, "Figures/BON_presabs.png") # Plot presence absence

###############################
## Predict for other points ##
###############################
preds <- dismo::predict(BON_model1, BON_predict, n.trees = BON_model1$gbm.call$best.trees, type = "response")
BON_predict$predictions <- preds

BON_predict_sf <- grid_BON %>% # convert to sf so we can plot
  dplyr::select(-species, -abundance, -season, -longitude, -latitude) %>% 
  dplyr::left_join(BON_predict, ., by = "cellID") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

plotPredictions(BON_predict_sf, "Figures/BON_Model1preds.png") # Plot predictions
