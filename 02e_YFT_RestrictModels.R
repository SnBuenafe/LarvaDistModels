# DESCRIPTION: New models

###########################
## Load preliminaries ##
###########################
# Load YFT data
source("02a_YFT_Data.R")

#################
## Build model ##
#################

# Grid search
CVGrid <- CVgridSearch(train, test, tc = c(1, 2), bf = c(0.5, 0.75), lr = seq(0.005, 0.01, 0.001), pred_in = c(9:24), resp_in = 5)

print(CVGrid %>% dplyr::arrange(desc(test_AUC)), n = 1) # BEST TEST AUC

# Building most optimal model
YFT_model8 <- dismo::gbm.step(data = train, gbm.x = c(9:24),
                              gbm.y = 5, family = "bernoulli", n.folds = 5,
                              tree.complexity = 2, bag.fraction = 0.75, learning.rate = 0.008
)
saveRDS(YFT_model8, "Output/Models/YFT_model8.rds")
YFT_model8 <- readRDS("Output/Models/YFT_model8.rds")

summary(YFT_model8) # get the relative importance of each of the predictors

# Printing AUCs
YFT_model8$self.statistics$discrimination # Training AUC Score
YFT_model8$cv.statistics$discrimination.mean # Validation AUC Score

preds <- gbm::predict.gbm(YFT_model8, test, n.trees = YFT_model8$gbm.call$best.trees, type = "response") # predict to test

dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
.roc(test$abundance_presence, preds) # Get testing AUC

train_tmp <- train %>% 
  dplyr::mutate(model = YFT_model8$fitted)
preds <- gbm::predict.gbm(YFT_model8, test, n.trees = YFT_model8$gbm.call$best.trees, type = "response") # predict to test

test_tmp <- test %>% 
  dplyr::mutate(model = preds)

##############
## Plotting ##
##############
# January-March
gg <- plotSeasonPredict(train_tmp %>% dplyr::filter(latitude <= 40 & latitude >= -40), # training object with model column (fitted values)
                        test_tmp %>% dplyr::filter(latitude <= 40 & latitude >= -40), # testing object with model column (predictions)
                        "jan-mar", # season
                        YFT_predict_season1 %>% dplyr::filter(latitude <= 40 & latitude >= -40), # rest of the ocean cells
                        YFT_model8, # BRT model
                        `grid_YFT_jan-mar` %>% dplyr::filter(latitude <= 40 & latitude >= -40) # grid of species for specific season
)

ggsquish1 <- plotSquishedModel(gg[[1]], YFT_ds1) # Plot the squished model

# April-June
gg <- plotSeasonPredict(train_tmp%>% dplyr::filter(latitude <= 40 & latitude >= -40), # training object with model column (fitted values)
                        test_tmp%>% dplyr::filter(latitude <= 40 & latitude >= -40), # testing object with model column (predictions)
                        "apr-jun", # season
                        YFT_predict_season2 %>% dplyr::filter(latitude <= 40 & latitude >= -40), # rest of the ocean cells
                        YFT_model8, # BRT model
                        `grid_YFT_apr-jun` %>% dplyr::filter(latitude <= 40 & latitude >= -40) # grid of species for specific season
)

ggsquish2 <- plotSquishedModel(gg[[1]], YFT_ds2) # Plot the squished model

# July-September
gg <- plotSeasonPredict(train_tmp%>% dplyr::filter(latitude <= 40 & latitude >= -40), # training object with model column (fitted values)
                        test_tmp%>% dplyr::filter(latitude <= 40 & latitude >= -40), # testing object with model column (predictions)
                        "jul-sept", # season
                        YFT_predict_season3 %>% dplyr::filter(latitude <= 40 & latitude >= -40), # rest of the ocean cells
                        YFT_model8, # BRT model
                        `grid_YFT_jul-sept` %>% dplyr::filter(latitude <= 40 & latitude >= -40) # grid of species for specific season
)

ggsquish3 <- plotSquishedModel(gg[[1]], YFT_ds3) # Plot the squished model

# October-December
gg <- plotSeasonPredict(train_tmp%>% dplyr::filter(latitude <= 40 & latitude >= -40), # training object with model column (fitted values)
                        test_tmp%>% dplyr::filter(latitude <= 40 & latitude >= -40), # testing object with model column (predictions)
                        "oct-dec", # season
                        YFT_predict_season4 %>% dplyr::filter(latitude <= 40 & latitude >= -40), # rest of the ocean cells
                        YFT_model8, # BRT model
                        `grid_YFT_oct-dec` %>% dplyr::filter(latitude <= 40 & latitude >= -40)# grid of species for specific season
)

ggsquish4 <- plotSquishedModel(gg[[1]], YFT_ds4) # Plot the squished model

ggsquished <- (ggsquish1 + ggsquish2) / (ggsquish3 + ggsquish4) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = ggsquished, filename = "Figures/YFT/YFT_model8_squished_40.png", width = 27, height = 15, dpi = 600)
