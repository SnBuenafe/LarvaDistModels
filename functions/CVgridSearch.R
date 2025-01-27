# Description: Do a k-fold grid search

CVgridSearch <- function(train, 
                         test,
                         tc, # tree complexity list
                         bf, # bag fraction list
                         lr, # learning rate list
                         pred_in, # indices of predictors
                         resp_in, # index of response
                         folds = 5 # Use a 5-fold cross-validation
                         ) {
  
  gridTib <- dplyr::tibble(tree_complexity = numeric(),
                           bag_fraction = numeric(),
                           learning_rate = numeric(),
                           train_AUC = numeric(),
                           valid_AUC = numeric(),
                           test_AUC = numeric(),
                           train_test_diff = numeric(),
                           pred_dev = numeric())
  
  x = 1
  for(t in 1:length(tc)) {
    for(b in 1:length(bf)) {
      for(l in 1:length(lr)) {
        model <- dismo::gbm.step(data = train,
                                 gbm.x = pred_in,
                                 gbm.y = resp_in,
                                 tree.complexity = tc[t],
                                 learning.rate = lr[l],
                                 bag.fraction = bf[b],
                                 family = "bernoulli",
                                 n.folds = folds
        )
        
        # Populate the grid tibble
        gridTib[x, "tree_complexity"] = tc[t]
        gridTib[x, "bag_fraction"] = bf[b]
        gridTib[x, "learning_rate"] = lr[l]
        gridTib[x, "train_AUC"] = model$self.statistics$discrimination
        gridTib[x, "valid_AUC"] = model$cv.statistics$discrimination.mean
        
        # Predict
        pred <- gbm::predict.gbm(model, test, n.trees = model$gbm.call$best.trees, type = "response")
        roc <- get_testAUC(test[,resp_in], pred)
        
        gridTib[x, "test_AUC"] = roc
        gridTib[x, "train_test_diff"] = model$self.statistics$discrimination - roc
        
        # Calculate predictive deviance
        gridTib[x, "pred_dev"] = dismo::calc.deviance(test[,resp_in], pred, family = "bernoulli")
        
        print(paste0("Run ", x)) # sanity check
        
        x = x + 1
        
      }
    }
  }
  
  return(gridTib)
}
