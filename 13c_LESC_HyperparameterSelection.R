# DESCRIPTION: Hyperparameter selection

###########################
## Load preliminaries ##
###########################
# Load LESC data
source("13a_LESC_Data.R")

#### Grid search with a max # of trees ####
CVGrid <- CVgridSearch(train, test, tc = c(1, 2), bf = c(0.5, 0.75), lr = seq(0.005, 0.01, 0.001), pred_in = c(7:21, 23:24), resp_in = 5)

print(CVGrid %>% dplyr::arrange(desc(test_AUC)), n =1 ) # BEST TEST AUC
print(CVGrid %>% dplyr::arrange(train_test_diff))

##########################
## Best test AUC ##
##########################

LESC_model2 <- dismo::gbm.step(data = train, gbm.x = c(7:21, 23:24),
                               gbm.y = 5, family = "bernoulli", n.folds = 5,
                               tree.complexity = 2, bag.fraction = 0.75, learning.rate = 0.009
)
saveRDS(LESC_model2, "Output/Models/LESC_model2.rds")

summary(LESC_model2) # get the relative importance of each of the predictors

# AUCs
LESC_model2$self.statistics$discrimination # Training AUC Score
LESC_model2$cv.statistics$discrimination.mean # Validation AUC Score

preds <- gbm::predict.gbm(LESC_model2, test, n.trees = LESC_model2$gbm.call$best.trees, type = "response") # predict to test

dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
.roc(test$abundance_presence, preds) # Get testing AUC

###############################################
## Least overfitting with reasonable AUC ##
###############################################

LESC_model3 <- dismo::gbm.step(data = train, gbm.x = c(7:21, 23:24),
                               gbm.y = 5, family = "bernoulli", n.folds = 5,
                               tree.complexity = 1, bag.fraction = 0.75, learning.rate = 0.006
)
saveRDS(LESC_model3, "Output/Models/LESC_model3.rds")

summary(LESC_model3) # get the relative importance of each of the predictors

# AUCs
LESC_model3$self.statistics$discrimination # Training AUC Score
LESC_model3$cv.statistics$discrimination.mean # Validation AUC Score

preds <- gbm::predict.gbm(LESC_model3, test, n.trees = LESC_model3$gbm.call$best.trees, type = "response") # predict to test

dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
.roc(test$abundance_presence, preds) # Get testing AUC
