# DESCRIPTION: Hyperparameter selection

###########################
## Load preliminaries ##
###########################
# Load YFT data
source("02a_YFTData.R")

##########################
## Original predictors ##
##########################

#### Grid search with a max # of trees ####
CVGrid <- CVgridSearch(train, test, tc = c(1, 2, 3), bf = c(0.5, 0.75), lr = seq(0.005, 0.01, 0.001), pred_in = c(7:13, 19:20), resp_in = 5)

print(CVGrid %>% dplyr::arrange(desc(test_AUC)), n = 1) # BEST TEST AUC
print(CVGrid %>% dplyr::arrange(pred_dev), n = 1) # BEST HOLDOUT DEVIANCE
print(CVGrid %>% dplyr::arrange(train_test_diff), n = 1) # LEAST OVERFIT; obviously the one with less tree depth but surprisingly the lowest learning rate?

############################################
## LEAST OVERFIT TREE COMPLEXITY = 3 ##
############################################
# Try out least overfit in deepest trees (tree complexity = 3)
print(CVGrid %>% dplyr::filter(tree_complexity == 3) %>% dplyr::arrange(train_test_diff), n = 1) # LEAST OVERFIT IN DEEPEST TREES

YFT_model6 <- dismo::gbm.step(data = train, gbm.x = c(7:13, 19:20),
                              gbm.y = 5, family = "bernoulli", n.folds = 5,
                              tree.complexity = 3, bag.fraction = 0.5, learning.rate = 0.007
                              )
summary(YFT_model6) # get the relative importance of each of the predictors

# AUCs
YFT_model6$self.statistics$discrimination # Training AUC Score
YFT_model6$cv.statistics$discrimination.mean # Validation AUC Score

preds <- gbm::predict.gbm(YFT_model6, test, n.trees = YFT_model6$gbm.call$best.trees, type = "response") # predict to test

dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
.roc(test$abundance_presence, preds) # Get testing AUC

############################################
## LEAST OVERFIT TREE COMPLEXITY = 2 ##
############################################
print(CVGrid %>% dplyr::filter(tree_complexity == 2) %>% dplyr::arrange(train_test_diff), n = 1) 

YFT_model7 <- dismo::gbm.step(data = train, gbm.x = c(7:13, 19:20),
                              gbm.y = 5, family = "bernoulli", n.folds = 5,
                              tree.complexity = 2, bag.fraction = 0.5, learning.rate = 0.008
)
saveRDS(YFT_model7, "Output/Models/YFT_model7.rds") # save this model
summary(YFT_model7) # get the relative importance of each of the predictors

# AUCs
YFT_model7$self.statistics$discrimination # Training AUC Score
YFT_model7$cv.statistics$discrimination.mean # Validation AUC Score

preds <- gbm::predict.gbm(YFT_model7, test, n.trees = YFT_model7$gbm.call$best.trees, type = "response") # predict to test

dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
.roc(test$abundance_presence, preds) # Get testing AUC

############################################
## HIGHER LEARNING RATE (TREE_COMPLEXITY = 2) ##
############################################
print(CVGrid %>% dplyr::filter(tree_complexity == 2 & learning_rate == 0.01) %>% dplyr::arrange(test_AUC), n = 1) 

YFT_model8 <- dismo::gbm.step(data = train, gbm.x = c(7:13, 19:20),
                              gbm.y = 5, family = "bernoulli", n.folds = 5,
                              tree.complexity = 2, bag.fraction = 0.5, learning.rate = 0.01
)
summary(YFT_model8) # get the relative importance of each of the predictors

# AUCs
YFT_model8$self.statistics$discrimination # Training AUC Score
YFT_model8$cv.statistics$discrimination.mean # Validation AUC Score

preds <- gbm::predict.gbm(YFT_model8, test, n.trees = YFT_model8$gbm.call$best.trees, type = "response") # predict to test

dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
.roc(test$abundance_presence, preds) # Get testing AUC

###################
## BEST TEST AUC ##
###################

YFT_model9 <- dismo::gbm.step(data = train, gbm.x = c(7:13, 19:20),
                              gbm.y = 5, family = "bernoulli", n.folds = 5,
                              tree.complexity = 3, bag.fraction = 0.75, learning.rate = 0.01
)
summary(YFT_model9) # get the relative importance of each of the predictors

# AUCs
YFT_model9$self.statistics$discrimination # Training AUC Score
YFT_model9$cv.statistics$discrimination.mean # Validation AUC Score

preds <- gbm::predict.gbm(YFT_model9, test, n.trees = YFT_model9$gbm.call$best.trees, type = "response") # predict to test

dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
.roc(test$abundance_presence, preds) # Get testing AUC

###########################################
## BEST TEST AUC in Moderate-depth trees ##
###########################################
print(CVGrid %>% dplyr::filter(tree_complexity == 2) %>% dplyr::arrange(desc(test_AUC)), n = 1)

YFT_model10 <- dismo::gbm.step(data = train, gbm.x = c(7:13, 19:20),
                              gbm.y = 5, family = "bernoulli", n.folds = 5,
                              tree.complexity = 2, bag.fraction = 0.75, learning.rate = 0.007
)
saveRDS(YFT_model10, "Output/Models/YFT_model10.rds") # save this model

summary(YFT_model10) # get the relative importance of each of the predictors

# AUCs
YFT_model10$self.statistics$discrimination # Training AUC Score
YFT_model10$cv.statistics$discrimination.mean # Validation AUC Score

preds <- gbm::predict.gbm(YFT_model10, test, n.trees = YFT_model10$gbm.call$best.trees, type = "response") # predict to test

dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
.roc(test$abundance_presence, preds) # Get testing AUC

###################
## LEAST OVERFIT ##
###################
YFT_model11 <- dismo::gbm.step(data = train, gbm.x = c(7:13, 19:20),
                               gbm.y = 5, family = "bernoulli", n.folds = 5,
                               tree.complexity = 1, bag.fraction = 0.5, learning.rate = 0.006
)

summary(YFT_model11) # get the relative importance of each of the predictors

# AUCs
YFT_model11$self.statistics$discrimination # Training AUC Score
YFT_model11$cv.statistics$discrimination.mean # Validation AUC Score

preds <- gbm::predict.gbm(YFT_model11, test, n.trees = YFT_model11$gbm.call$best.trees, type = "response") # predict to test

dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
.roc(test$abundance_presence, preds) # Get testing AUC

##########################
## Additional predictors ##
##########################

#### Grid search with a max # of trees ####
CVGrid <- CVgridSearch(train, test, tc = c(1, 2), bf = c(0.5, 0.75), lr = seq(0.005, 0.01, 0.001), pred_in = c(7:20), resp_in = 5)

print(CVGrid %>% dplyr::arrange(desc(test_AUC)), n =1) # BEST TEST AUC
print(CVGrid %>% dplyr::arrange(pred_dev)) # BEST HOLDOUT DEVIANCE; same as above
print(CVGrid %>% dplyr::arrange(train_test_diff)) # LEAST OVERFIT

###################
## BEST TEST AUC ##
###################

YFT_model12 <- dismo::gbm.step(data = train, gbm.x = c(7:20),
                               gbm.y = 5, family = "bernoulli", n.folds = 5,
                               tree.complexity = 2, bag.fraction = 0.75, learning.rate = 0.01
)
saveRDS(YFT_model12, "Output/Models/YFT_model12.rds") # save this model

summary(YFT_model12) # get the relative importance of each of the predictors

# AUCs
YFT_model12$self.statistics$discrimination # Training AUC Score
YFT_model12$cv.statistics$discrimination.mean # Validation AUC Score

preds <- gbm::predict.gbm(YFT_model12, test, n.trees = YFT_model12$gbm.call$best.trees, type = "response") # predict to test

dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
.roc(test$abundance_presence, preds) # Get testing AUC

######################################
## LEAST OVERFIT IN MODERATE TREES ##
######################################

print(CVGrid %>% dplyr::filter(tree_complexity == 2) %>% dplyr::arrange(train_test_diff), n = 1) # LEAST OVERFIT

YFT_model13 <- dismo::gbm.step(data = train, gbm.x = c(7:20),
                               gbm.y = 5, family = "bernoulli", n.folds = 5,
                               tree.complexity = 2, bag.fraction = 0.75, learning.rate = 0.005
)
saveRDS(YFT_model13, "Output/Models/YFT_model13.rds") # save this model

summary(YFT_model13) # get the relative importance of each of the predictors

# AUCs
YFT_model13$self.statistics$discrimination # Training AUC Score
YFT_model13$cv.statistics$discrimination.mean # Validation AUC Score

preds <- gbm::predict.gbm(YFT_model13, test, n.trees = YFT_model13$gbm.call$best.trees, type = "response") # predict to test

dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
.roc(test$abundance_presence, preds) # Get testing AUC

###################
## LEAST OVERFIT ##
###################

YFT_model14 <- dismo::gbm.step(data = train, gbm.x = c(7:20),
                               gbm.y = 5, family = "bernoulli", n.folds = 5,
                               tree.complexity = 1, bag.fraction = 0.75, learning.rate = 0.005
)

summary(YFT_model14) # get the relative importance of each of the predictors

# AUCs
YFT_model14$self.statistics$discrimination # Training AUC Score
YFT_model14$cv.statistics$discrimination.mean # Validation AUC Score

preds <- gbm::predict.gbm(YFT_model14, test, n.trees = YFT_model14$gbm.call$best.trees, type = "response") # predict to test

dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
.roc(test$abundance_presence, preds) # Get testing AUC
