# DESCRIPTION: Hyperparameter selection

###########################
## Load preliminaries ##
###########################
# Load SBFT data
source("16a_SBFT_Data.R")

#### Grid search with a max # of trees ####
CVGrid <- CVgridSearch(train, test, tc = c(1, 2), bf = c(0.5, 0.75), lr = seq(0.005, 0.01, 0.001), pred_in = c(7:20), resp_in = 5) 

print(CVGrid %>% dplyr::arrange(desc(test_AUC)), n = 1 ) # BEST TEST AUC

##########################
## Best test AUC ##
##########################

SBFT_model2 <- dismo::gbm.step(data = train, gbm.x = c(7:20),
                              gbm.y = 5, family = "bernoulli", n.folds = 5,
                              tree.complexity = 1, bag.fraction = 0.5, learning.rate = 0.01
)
saveRDS(SBFT_model2, "Output/Models/SBFT_model2.rds")

summary(SBFT_model2) # get the relative importance of each of the predictors

# AUCs
SBFT_model2$self.statistics$discrimination # Training AUC Score
SBFT_model2$cv.statistics$discrimination.mean # Validation AUC Score

preds <- gbm::predict.gbm(SBFT_model2, test, n.trees = SBFT_model2$gbm.call$best.trees, type = "response") # predict to test

dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
.roc(test$abundance_presence, preds) # Get testing AUC
