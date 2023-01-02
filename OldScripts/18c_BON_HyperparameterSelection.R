# DESCRIPTION: Hyperparameter selection

###########################
## Load preliminaries ##
###########################
# Load BON data
source("18a_BON_Data.R")s

#### Grid search with a max # of trees ####
CVGrid <- CVgridSearch(train, test, tc = c(1, 2), bf = c(0.5, 0.75), lr = seq(0.005, 0.01, 0.001), pred_in = c(7:20), resp_in = 5) # learning rates > 0.007 will not allow the algorithm to converge

print(CVGrid %>% dplyr::arrange(desc(test_AUC)), n = 1 ) # BEST TEST AUC

##########################
## Best test AUC ##
##########################

BON_model2 <- dismo::gbm.step(data = train, gbm.x = c(7:20),
                              gbm.y = 5, family = "bernoulli", n.folds = 5,
                              tree.complexity = 2, bag.fraction = 0.75, learning.rate = 0.005
)
saveRDS(BON_model2, "Output/Models/BON_model2.rds")

summary(BON_model2) # get the relative importance of each of the predictors

# AUCs
BON_model2$self.statistics$discrimination # Training AUC Score
BON_model2$cv.statistics$discrimination.mean # Validation AUC Score

preds <- gbm::predict.gbm(BON_model2, test, n.trees = BON_model2$gbm.call$best.trees, type = "response") # predict to test

dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
.roc(test$abundance_presence, preds) # Get testing AUC
