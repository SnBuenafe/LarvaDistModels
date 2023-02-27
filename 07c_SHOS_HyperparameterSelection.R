# DESCRIPTION: Hyperparameter selection

###########################
## Load preliminaries ##
###########################
# Load YFT data
source("07a_SHOS_Data.R")

##########################
## Additional predictors ##
##########################

#### Grid search with a max # of trees ####
CVGrid <- CVgridSearch(train, test, tc = c(1, 2), bf = c(0.5, 0.75), lr = seq(0.005, 0.01, 0.001), pred_in = c(7:21, 23:24), resp_in = 5)

print(CVGrid %>% dplyr::arrange(desc(test_AUC)), n = 1) # BEST TEST AUC

##########################
## Best test AUC ##
##########################

SHOS_model2 <- dismo::gbm.step(data = train, gbm.x = c(7:20),
                               gbm.y = 5, family = "bernoulli", n.folds = 5,
                               tree.complexity = 2, bag.fraction = 0.5, learning.rate = 0.007
)
saveRDS(SHOS_model2, "Output/Models/SHOS_model2.rds")

summary(SHOS_model2) # get the relative importance of each of the predictors

# AUCs
SHOS_model2$self.statistics$discrimination # Training AUC Score
SHOS_model2$cv.statistics$discrimination.mean # Validation AUC Score

preds <- gbm::predict.gbm(SHOS_model2, test, n.trees = SHOS_model2$gbm.call$best.trees, type = "response") # predict to test

dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
.roc(test$abundance_presence, preds) # Get testing AUC

