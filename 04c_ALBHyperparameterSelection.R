# DESCRIPTION: Hyperparameter selection

###########################
## Load preliminaries ##
###########################
# Load YFT data
source("04a_ALBData.R")

##########################
## Original predictors ##
##########################

#### Grid search with a max # of trees ####
CVGrid <- CVgridSearch(train, test, tc = c(1, 2), bf = c(0.5, 0.75), lr = seq(0.005, 0.01, 0.001), pred_in = c(7:13, 19:20), resp_in = 5)

print(CVGrid %>% dplyr::arrange(desc(test_AUC))) # BEST TEST AUC
print(CVGrid %>% dplyr::arrange(pred_dev)) # BEST HOLDOUT DEVIANCE; the same as above
print(CVGrid %>% dplyr::arrange(train_test_diff)) # LEAST OVERFIT; obviously the one with less tree depth but surprisingly the lowest learning rate?

##########################
## Best test AUC ##
##########################

ALB_model3 <- dismo::gbm.step(data = train, gbm.x = c(7:13, 19:20),
                              gbm.y = 5, family = "bernoulli", n.folds = 5,
                              tree.complexity = 2, bag.fraction = 0.75, learning.rate = 0.01
)
saveRDS(ALB_model3, "Output/Models/ALB_model3.rds")
summary(ALB_model3) # get the relative importance of each of the predictors

# AUCs
ALB_model3$self.statistics$discrimination # Training AUC Score
ALB_model3$cv.statistics$discrimination.mean # Validation AUC Score

preds <- gbm::predict.gbm(ALB_model3, test, n.trees = ALB_model3$gbm.call$best.trees, type = "response") # predict to test

dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
.roc(test$abundance_presence, preds) # Get testing AUC

##########################################
## Least overfit in moderate trees ##
##########################################
print(CVGrid %>% dplyr::filter(tree_complexity == 2) %>% dplyr::arrange(train_test_diff))

ALB_model4 <- dismo::gbm.step(data = train, gbm.x = c(7:13, 19:20),
                              gbm.y = 5, family = "bernoulli", n.folds = 5,
                              tree.complexity = 2, bag.fraction = 0.5, learning.rate = 0.005
)
saveRDS(ALB_model4, "Output/Models/ALB_model4.rds")
summary(ALB_model4) # get the relative importance of each of the predictors

# AUCs
ALB_model4$self.statistics$discrimination # Training AUC Score
ALB_model4$cv.statistics$discrimination.mean # Validation AUC Score

preds <- gbm::predict.gbm(ALB_model4, test, n.trees = ALB_model4$gbm.call$best.trees, type = "response") # predict to test

dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
.roc(test$abundance_presence, preds) # Get testing AUC

##########################################
## Least overfit overall ##
##########################################

ALB_model5 <- dismo::gbm.step(data = train, gbm.x = c(7:13, 19:20),
                              gbm.y = 5, family = "bernoulli", n.folds = 5,
                              tree.complexity = 1, bag.fraction = 0.75, learning.rate = 0.005
)
summary(ALB_model5) # get the relative importance of each of the predictors

# AUCs
ALB_model5$self.statistics$discrimination # Training AUC Score
ALB_model5$cv.statistics$discrimination.mean # Validation AUC Score

preds <- gbm::predict.gbm(ALB_model5, test, n.trees = ALB_model5$gbm.call$best.trees, type = "response") # predict to test

dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
.roc(test$abundance_presence, preds) # Get testing AUC

##########################
## Additional predictors ##
##########################

#### Grid search with a max # of trees ####
CVGrid <- CVgridSearch(train, test, tc = c(1, 2), bf = c(0.5, 0.75), lr = seq(0.005, 0.01, 0.001), pred_in = c(7:20), resp_in = 5)

print(CVGrid %>% dplyr::arrange(desc(test_AUC))) # BEST TEST AUC
print(CVGrid %>% dplyr::arrange(pred_dev)) # BEST HOLDOUT DEVIANCE
print(CVGrid %>% dplyr::arrange(train_test_diff)) # LEAST OVERFIT; obviously the one with less tree depth but surprisingly the lowest learning rate?

##########################
## Best test AUC ##
##########################

ALB_model6 <- dismo::gbm.step(data = train, gbm.x = c(7:20),
                              gbm.y = 5, family = "bernoulli", n.folds = 5,
                              tree.complexity = 2, bag.fraction = 0.75, learning.rate = 0.01
)
saveRDS(ALB_model6, "Output/Models/ALB_model6.rds")

summary(ALB_model6) # get the relative importance of each of the predictors

# AUCs
ALB_model6$self.statistics$discrimination # Training AUC Score
ALB_model6$cv.statistics$discrimination.mean # Validation AUC Score

preds <- gbm::predict.gbm(ALB_model6, test, n.trees = ALB_model6$gbm.call$best.trees, type = "response") # predict to test

dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
.roc(test$abundance_presence, preds) # Get testing AUC

##########################################
## Least overfitting in moderate trees ##
##########################################
print(CVGrid %>% dplyr::filter(tree_complexity == 2) %>% dplyr::arrange(train_test_diff))

ALB_model7 <- dismo::gbm.step(data = train, gbm.x = c(7:20),
                              gbm.y = 5, family = "bernoulli", n.folds = 5,
                              tree.complexity = 2, bag.fraction = 0.5, learning.rate = 0.005
)
saveRDS(ALB_model7, "Output/Models/ALB_model7.rds")

summary(ALB_model7) # get the relative importance of each of the predictors

# AUCs
ALB_model7$self.statistics$discrimination # Training AUC Score
ALB_model7$cv.statistics$discrimination.mean # Validation AUC Score

preds <- gbm::predict.gbm(ALB_model7, test, n.trees = ALB_model7$gbm.call$best.trees, type = "response") # predict to test

dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
.roc(test$abundance_presence, preds) # Get testing AUC

##########################################
## Least overfitting overall ##
##########################################

ALB_model8 <- dismo::gbm.step(data = train, gbm.x = c(7:20),
                              gbm.y = 5, family = "bernoulli", n.folds = 5,
                              tree.complexity = 1, bag.fraction = 0.75, learning.rate = 0.005
)
summary(ALB_model8) # get the relative importance of each of the predictors

# AUCs
ALB_model8$self.statistics$discrimination # Training AUC Score
ALB_model8$cv.statistics$discrimination.mean # Validation AUC Score

preds <- gbm::predict.gbm(ALB_model8, test, n.trees = ALB_model8$gbm.call$best.trees, type = "response") # predict to test

dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
.roc(test$abundance_presence, preds) # Get testing AUC
