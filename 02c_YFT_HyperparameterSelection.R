# DESCRIPTION: Hyperparameter selection

###########################
## Load preliminaries ##
###########################
# Load YFT data
source("02a_YFT_Data.R")

#### Grid search with a max # of trees ####
CVGrid <- CVgridSearch(train, test, tc = c(1, 2, 3), bf = c(0.5, 0.75), lr = seq(0.005, 0.01, 0.001), pred_in = c(7:21, 23:24), resp_in = 5) # velocity is no longer included

print(CVGrid %>% dplyr::arrange(desc(test_AUC)), n = 1) # BEST TEST AUC
print(CVGrid %>% dplyr::arrange(train_test_diff), n = 1) # LEAST OVERFIT

###################
## LEAST OVERFIT ##
###################
YFT_model5 <- dismo::gbm.step(data = train, gbm.x = c(7:21, 23:24), # removed velocity
                               gbm.y = 5, family = "bernoulli", n.folds = 5,
                               tree.complexity = 1, bag.fraction = 0.75, learning.rate = 0.009
)

summary(YFT_model5) # get the relative importance of each of the predictors

# AUCs
YFT_model5$self.statistics$discrimination # Training AUC Score
YFT_model5$cv.statistics$discrimination.mean # Validation AUC Score

preds <- gbm::predict.gbm(YFT_model5, test, n.trees = YFT_model5$gbm.call$best.trees, type = "response") # predict to test

dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
.roc(test$abundance_presence, preds) # Get testing AUC

###################
## BEST TEST AUC ##
###################

YFT_model6 <- dismo::gbm.step(data = train, gbm.x = c(7:21, 23:24), # removed velocity
                               gbm.y = 5, family = "bernoulli", n.folds = 5,
                               tree.complexity = 3, bag.fraction = 0.75, learning.rate = 0.009
)
saveRDS(YFT_model6, "Output/Models/YFT_model6.rds") # save this model

summary(YFT_model6) # get the relative importance of each of the predictors

# AUCs
YFT_model6$self.statistics$discrimination # Training AUC Score
YFT_model6$cv.statistics$discrimination.mean # Validation AUC Score

preds <- gbm::predict.gbm(YFT_model6, test, n.trees = YFT_model6$gbm.call$best.trees, type = "response") # predict to test

dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
.roc(test$abundance_presence, preds) # Get testing AUC
