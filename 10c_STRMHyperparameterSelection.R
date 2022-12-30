# DESCRIPTION: Hyperparameter selection

###########################
## Load preliminaries ##
###########################
# Load YFT data
source("010a_STRMData.R")

#### Grid search with a max # of trees ####
CVGrid <- CVgridSearch(train, test, tc = c(1, 2), bf = c(0.5, 0.75), lr = seq(0.005, 0.01, 0.001), pred_in = c(7:20), resp_in = 5)

print(CVGrid %>% dplyr::arrange(desc(test_AUC)), n =1) # BEST TEST AUC
print(CVGrid %>% dplyr::arrange(train_test_diff)) # Least overfitting, but difference is still > 0.1

##########################
## Best test AUC ##
##########################

STRM_model2 <- dismo::gbm.step(data = train, gbm.x = c(7:20),
                              gbm.y = 5, family = "bernoulli", n.folds = 5,
                              tree.complexity = 2, bag.fraction = 0.5, learning.rate = 0.009
)
saveRDS(STRM_model2, "Output/Models/STRM_model2.rds")

summary(STRM_model2) # get the relative importance of each of the predictors

# AUCs
STRM_model2$self.statistics$discrimination # Training AUC Score
STRM_model2$cv.statistics$discrimination.mean # Validation AUC Score

preds <- gbm::predict.gbm(STRM_model2, test, n.trees = STRM_model2$gbm.call$best.trees, type = "response") # predict to test

dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
.roc(test$abundance_presence, preds) # Get testing AUC

##########################
## Least overfitting ##
##########################

STRM_model3 <- dismo::gbm.step(data = train, gbm.x = c(7:20),
                               gbm.y = 5, family = "bernoulli", n.folds = 5,
                               tree.complexity = 1, bag.fraction = 0.5, learning.rate = 0.005
)
saveRDS(STRM_model3, "Output/Models/STRM_model3.rds")

summary(STRM_model3) # get the relative importance of each of the predictors

# AUCs
STRM_model3$self.statistics$discrimination # Training AUC Score
STRM_model3$cv.statistics$discrimination.mean # Validation AUC Score

preds <- gbm::predict.gbm(STRM_model3, test, n.trees = STRM_model3$gbm.call$best.trees, type = "response") # predict to test

dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
.roc(test$abundance_presence, preds) # Get testing AUC

####################################################
## Less overfitting with considerable AUC ##
####################################################

STRM_model4 <- dismo::gbm.step(data = train, gbm.x = c(7:20),
                               gbm.y = 5, family = "bernoulli", n.folds = 5,
                               tree.complexity = 1, bag.fraction = 0.75, learning.rate = 0.008
)
saveRDS(STRM_model4, "Output/Models/STRM_model4.rds")

summary(STRM_model4) # get the relative importance of each of the predictors

# AUCs
STRM_model4$self.statistics$discrimination # Training AUC Score
STRM_model4$cv.statistics$discrimination.mean # Validation AUC Score

preds <- gbm::predict.gbm(STRM_model4, test, n.trees = STRM_model4$gbm.call$best.trees, type = "response") # predict to test

dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
.roc(test$abundance_presence, preds) # Get testing AUC
