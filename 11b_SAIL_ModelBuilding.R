# DESCRIPTION: Building optimal model for sailfish

###########################
## Load preliminaries ##
###########################
# Load SAIL data
source("11a_SAIL_Data.R")

#################
## Build model ##
#################

# Grid search
CVGrid <- CVgridSearch(train, test, tc = c(1, 2), bf = c(0.5, 0.75), lr = seq(0.005, 0.01, 0.001), pred_in = c(9:24), resp_in = 5)

print(CVGrid %>% dplyr::arrange(desc(test_AUC)), n = 1) # BEST TEST AUC

# Building most optimal model
SAIL_model1 <- dismo::gbm.step(data = train, gbm.x = c(9:24),
                              gbm.y = 5, family = "bernoulli", n.folds = 5,
                              tree.complexity = 1, bag.fraction = 0.75, learning.rate = 0.005
)
saveRDS(SAIL_model1, "Output/Models/SAIL_model1.rds")
# SAIL_model1 <- readRDS("Output/Models/SAIL_model1.rds")

summary(SAIL_model1) # get the relative importance of each of the predictors

# Printing AUCs
SAIL_model1$self.statistics$discrimination # Training AUC Score
SAIL_model1$cv.statistics$discrimination.mean # Validation AUC Score

preds <- gbm::predict.gbm(SAIL_model1, test, n.trees = SAIL_model1$gbm.call$best.trees, type = "response") # predict to test

dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
.roc(test$abundance_presence, preds) # Get testing AUC

train_tmp <- train %>% 
  dplyr::mutate(model = SAIL_model1$fitted)
preds <- gbm::predict.gbm(SAIL_model1, test, n.trees = SAIL_model1$gbm.call$best.trees, type = "response") # predict to test

test_tmp <- test %>% 
  dplyr::mutate(model = preds)

##############
## Plotting ##
##############

# January-March
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jan-mar", # season
                        SAIL_predict_season1, # rest of the ocean cells
                        SAIL_model1, # BRT model
                        `grid_SAIL_jan-mar` # grid of species for specific season
)

ggsquish1 <- plotSquishedModel(gg[[1]], SAIL_ds1) # Plot the squished model

# April-June
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "apr-jun", # season
                        SAIL_predict_season2, # rest of the ocean cells
                        SAIL_model1, # BRT model
                        `grid_SAIL_apr-jun` # grid of species for specific season
)

ggsquish2 <- plotSquishedModel(gg[[1]], SAIL_ds2) # Plot the squished model

# July-September
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jul-sept", # season
                        SAIL_predict_season3, # rest of the ocean cells
                        SAIL_model1, # BRT model
                        `grid_SAIL_jul-sept` # grid of species for specific season
)

ggsquish3 <- plotSquishedModel(gg[[1]], SAIL_ds3) # Plot the squished model

# October-December
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "oct-dec", # season
                        SAIL_predict_season4, # rest of the ocean cells
                        SAIL_model1, # BRT model
                        `grid_SAIL_oct-dec` # grid of species for specific season
)

ggsquish4 <- plotSquishedModel(gg[[1]], SAIL_ds4) # Plot the squished model

ggsquished <- (ggsquish1 + ggsquish2) / (ggsquish3 + ggsquish4) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = ggsquished, filename = "Figures/SAIL/SAIL_model1_squished_hatched.png", width = 27, height = 15, dpi = 600)
