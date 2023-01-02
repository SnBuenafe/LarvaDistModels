# DESCRIPTION: Hyperparameter selection

###########################
## Load preliminaries ##
###########################
# Load SLT data
source("17a_SLT_Data.R")

#### Grid search with a max # of trees ####
CVGrid <- CVgridSearch(train, test, tc = c(1, 2), bf = c(0.5, 0.75), lr = seq(0.005, 0.007, 0.001), pred_in = c(7:20), resp_in = 5) # learning rates > 0.007 will not allow the algorithm to converge

print(CVGrid %>% dplyr::arrange(desc(test_AUC)), n = 1 ) # BEST TEST AUC

##########################
## Best test AUC ##
##########################

SLT_model2 <- dismo::gbm.step(data = train, gbm.x = c(7:20),
                               gbm.y = 5, family = "bernoulli", n.folds = 5,
                               tree.complexity = 2, bag.fraction = 0.75, learning.rate = 0.005
)
saveRDS(SLT_model2, "Output/Models/SLT_model2.rds")

summary(SLT_model2) # get the relative importance of each of the predictors

# AUCs
SLT_model2$self.statistics$discrimination # Training AUC Score
SLT_model2$cv.statistics$discrimination.mean # Validation AUC Score

preds <- gbm::predict.gbm(SLT_model2, test, n.trees = SLT_model2$gbm.call$best.trees, type = "response") # predict to test

dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
.roc(test$abundance_presence, preds) # Get testing AUC
