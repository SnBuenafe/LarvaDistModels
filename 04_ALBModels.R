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
ALB_ds <- read_csv("Output/ALB_full.csv", show_col_types = FALSE)

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
# Do a cross-validated grid search using gbm.fixed
# define a 5-fold cross-validation
train <- dismo::kfold(ALB_filtered, k = 5)

ALB_filtered$fold <- train

gridTib <- tibble(tree_complexity = numeric(),
                  bag_fraction = numeric(),
                  learning_rate = numeric(),
                  cv_deviance = numeric(),
                  time = numeric())


tc <- c(1, 2, 3, 5) # tree complexity
bf <- c(0.5, 0.75)
lr <- c(0.005) # constant learning rate of 0.001 so we have >= 1000 trees (Cerasoli et al., 2017)

x = 1
for(i in 1:length(tc)) {
  for(j in 1:length(bf)) {
    for(k in 1:length(lr)) {
      
      time <- system.time({ 
        deviance <- c()
        for(l in 1:length(unique(train))) {
          subset <- dplyr::filter(ALB_filtered, fold == l)
          
          model <- dismo::gbm.fixed(data = subset, gbm.x = c(4, 6, 8:13), gbm.y = 14,
                                    tree.complexity = tc[i],
                                    learning.rate = lr[k],
                                    bag.fraction = bf[j],
                                    n.trees = 500, # just for the sake of grid search
                                    verbose = FALSE)
          
          deviance[l] <- model$self.statistics$resid.deviance
        }
        
        cv_deviance <- mean(deviance)
      }
      )
      
      # Populate the grid tibble
      gridTib[x, "tree_complexity"] = tc[i]
      gridTib[x, "bag_fraction"] = bf[j]
      gridTib[x, "learning_rate"] = lr[k]
      gridTib[x, "cv_deviance"] = cv_deviance
      gridTib[x, "time"] = time[[3]] # get the time elapsed
      print(paste0("Run: ", x, "; deviance = ", cv_deviance))
      
      
      x = x + 1
    }
  }
}

print(gridTib %>% dplyr::arrange(cv_deviance))

###########################
## Fit model ##
###########################
# Now build the model; we want to have a lot of trees: preferably at least 1,000 trees
ALB_model1 <- gbm.step(ALB_filtered, gbm.x = c(4, 6, 8:13), gbm.y = 14, 
                       learning.rate = 0.005,
                       bag.fraction = 0.5,
                       tree.complexity = 5
)
saveRDS(ALB_model1, "Output/ALB_model1.rds") # save the model
ALB_model1 <- readRDS("Output/ALB_model1.rds") # load the model

summary(ALB_model1) # get the relative importance of each of the predictors

# Plot predictors
pdf(file = "Figures/ALB_Model1_SmoothPredictors.pdf", width = 10, height = 8)
gbm.plot(ALB_model1, n.plots=8, plot.layout=c(3, 3), write.title = FALSE)
dev.off()

pdf(file = "Figures/ALB_Model1_Predictors.pdf", width = 10, height = 8)
gbm.plot.fits(ALB_model1)
dev.off()

# Check this for ROC metrics: https://stackoverflow.com/questions/22391547/roc-score-in-gbm-package
# CV AUC of ROC (Receiver Operating Characteristic Curve)
ALB_model1$cv.statistics$discrimination.mean # AUC Score

# Check the plot
ALB_filtered$model <- ALB_model1$fitted
ALB_sf <- grid %>% # convert to sf so we can plot
  dplyr::left_join(ALB_filtered, ., by = "cellID") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

plotModel(ALB_sf, "Figures/ALB_Model1.png") # Plot the model
plotAbundance(ALB_sf, "Figures/ALB_abundance.png") # Plot raw
plotPA(ALB_sf, "Figures/ALB_presabs.png") # Plot presence absence

###############################
## Predict for other points ##
###############################
preds <- dismo::predict(ALB_model1, ALB_predict, n.trees = ALB_model1$gbm.call$best.trees, type = "response")
ALB_predict$predictions <- preds

ALB_predict_sf <- grid_ALB %>% # convert to sf so we can plot
  dplyr::select(-species, -abundance, -season, -longitude, -latitude) %>% 
  dplyr::left_join(ALB_predict, ., by = "cellID") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

plotPredictions(ALB_predict_sf, "Figures/ALB_Model1preds.png") # Plot predictions
