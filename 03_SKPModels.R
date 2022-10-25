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
SKP_ds <- read_csv("Output/SKP_full.csv", show_col_types = FALSE)

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
# define a 5-fold cross-validation
CVGrid <- CVgridSearch(SKP_filtered, 5, tc = c(1, 2, 3, 5), bf = c(0.5, 0.75), lr = 0.005)

print(CVGrid %>% dplyr::arrange(cv_deviance))

###########################
## Fit model ##
###########################
# Now build the model; we want to have a lot of trees: preferably at least 1,000 trees
SKP_model1 <- gbm.step(SKP_filtered, gbm.x = c(4, 6, 8:13), gbm.y = 14, 
                       learning.rate = 0.005,
                       bag.fraction = 0.5,
                       tree.complexity = 5
)
saveRDS(SKP_model1, "Output/SKP_model1.rds") # save the model
SKP_model1 <- readRDS("Output/SKP_model1.rds") # load the model

summary(SKP_model1) # get the relative importance of each of the predictors

# Plot predictors
pdf(file = "Figures/SKP_Model1_SmoothPredictors.pdf", width = 10, height = 8)
gbm.plot(SKP_model1, n.plots=8, plot.layout=c(3, 3), write.title = FALSE)
dev.off()

pdf(file = "Figures/SKP_Model1_Predictors.pdf", width = 10, height = 8)
gbm.plot.fits(SKP_model1)
dev.off()

# Check this for ROC metrics: https://stackoverflow.com/questions/22391547/roc-score-in-gbm-package
# CV AUC of ROC (Receiver Operating Characteristic Curve)
SKP_model1$cv.statistics$discrimination.mean # AUC Score

# Check the plot
SKP_filtered$model <- SKP_model1$fitted
SKP_sf <- grid %>% # convert to sf so we can plot
  dplyr::left_join(SKP_filtered, ., by = "cellID") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

plotModel(SKP_sf, "Figures/SKP_Model1.png") # Plot the model
plotAbundance(SKP_sf, "Figures/SKP_abundance.png") # Plot raw
pres <- round(sum(SKP_sf$abundance_presence)/nrow(SKP_sf) * 100, 2)
plotPA(SKP_sf, pres, "Figures/SKP_presabs.png") # Plot presence absence

###############################
## Predict for other points ##
###############################
preds <- dismo::predict(SKP_model1, SKP_predict, n.trees = SKP_model1$gbm.call$best.trees, type = "response")
SKP_predict$predictions <- preds

SKP_predict_sf <- grid_SKP %>% # convert to sf so we can plot
  dplyr::select(-species, -abundance, -season, -longitude, -latitude) %>% 
  dplyr::left_join(SKP_predict, ., by = "cellID") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

plotPredictions(SKP_predict_sf, "Figures/SKP_Model1preds.png") # Plot predictions
