# DESCRIPTION: Plotting potential models for SWO

###########################
## Load preliminaries ##
###########################
# Load SWO data
source("05a_SWOData.R")

###########################################################################
## Model 3: Best test AUC (original predictors) ##
###########################################################################
# Load the model 3
SWO_model3 <- readRDS("Output/Models/SWO_model3.rds")

# What are the min and max latitudes?
min(train$latitude)
max(train$latitude)

train_tmp <- train %>% 
  dplyr::mutate(model = SWO_model3$fitted)
preds <- gbm::predict.gbm(SWO_model3, test, n.trees = SWO_model3$gbm.call$best.trees, type = "response") # predict to test

test_tmp <- test %>% 
  dplyr::mutate(model = preds)

#### January-March
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jan-mar", # season
                        SWO_predict_season1 %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)), # rest of the ocean cells
                        SWO_model3, # BRT model
                        `grid_SWO_jan-mar` %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)) # grid of species for specific season with restricted ranges
)

ggseason1 <- plotModel(gg[[1]], gg[[2]]) + # Plot the model
  ggtitle("January-March")

#### April-June
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "apr-jun", # season
                        SWO_predict_season2 %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)), # rest of the ocean cells
                        SWO_model3, # BRT model
                        `grid_SWO_apr-jun` %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude))# grid of species for specific season
)

ggseason2 <- plotModel(gg[[1]], gg[[2]]) + # Plot the model
  ggtitle("April-June")

#### July-September
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jul-sept", # season
                        SWO_predict_season3 %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)), # rest of the ocean cells
                        SWO_model3, # BRT model
                        `grid_SWO_jul-sept` %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)) # grid of species for specific season
)

ggseason3 <- plotModel(gg[[1]], gg[[2]]) + # Plot the model
  ggtitle("July-September")

#### October-December
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "oct-dec", # season
                        SWO_predict_season4 %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)), # rest of the ocean cells
                        SWO_model3, # BRT model
                        `grid_SWO_oct-dec` %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude))# grid of species for specific season
)

ggseason4 <- plotModel(gg[[1]], gg[[2]]) + # Plot the model
  ggtitle("October-December")

ggseasons <- (ggseason1 + ggseason2) / (ggseason3 + ggseason4) + 
  plot_layout(guides = "collect") +
  plot_annotation("Model 3: Original predictors with best test AUC (AUC: 0.84)")

ggsave(plot = ggseasons, filename = "Figures/SWO/SWO_Model3.png", width = 27, height = 15, dpi = 600)

###########################################################################
## Model 4: Least overfitting in moderate trees (original predictors) ##
###########################################################################
# Load the model 4
SWO_model4 <- readRDS("Output/Models/SWO_model4.rds")

# What are the min and max latitudes?
min(train$latitude)
max(train$latitude)

train_tmp <- train %>% 
  dplyr::mutate(model = SWO_model4$fitted)
preds <- gbm::predict.gbm(SWO_model4, test, n.trees = SWO_model4$gbm.call$best.trees, type = "response") # predict to test

test_tmp <- test %>% 
  dplyr::mutate(model = preds)

#### January-March
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jan-mar", # season
                        SWO_predict_season1 %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)), # rest of the ocean cells
                        SWO_model4, # BRT model
                        `grid_SWO_jan-mar` %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)) # grid of species for specific season with restricted ranges
)

ggseason1 <- plotModel(gg[[1]], gg[[2]]) + # Plot the model
  ggtitle("January-March")

#### April-June
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "apr-jun", # season
                        SWO_predict_season2 %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)), # rest of the ocean cells
                        SWO_model4, # BRT model
                        `grid_SWO_apr-jun` %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude))# grid of species for specific season
)

ggseason2 <- plotModel(gg[[1]], gg[[2]]) + # Plot the model
  ggtitle("April-June")

#### July-September
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jul-sept", # season
                        SWO_predict_season3 %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)), # rest of the ocean cells
                        SWO_model4, # BRT model
                        `grid_SWO_jul-sept` %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)) # grid of species for specific season
)

ggseason3 <- plotModel(gg[[1]], gg[[2]]) + # Plot the model
  ggtitle("July-September")

#### October-December
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "oct-dec", # season
                        SWO_predict_season4 %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)), # rest of the ocean cells
                        SWO_model4, # BRT model
                        `grid_SWO_oct-dec` %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude))# grid of species for specific season
)

ggseason4 <- plotModel(gg[[1]], gg[[2]]) + # Plot the model
  ggtitle("October-December")

ggseasons <- (ggseason1 + ggseason2) / (ggseason3 + ggseason4) + 
  plot_layout(guides = "collect") +
  plot_annotation("Model 4: Original predictors with least overfitting with moderate complexity (AUC: 0.85)")

ggsave(plot = ggseasons, filename = "Figures/SWO/SWO_Model4.png", width = 27, height = 15, dpi = 600)

###########################################################################
## Model 6: Best test AUC (additional predictors) ##
###########################################################################
# Load the model 6
SWO_model6 <- readRDS("Output/Models/SWO_model6.rds")

# What are the min and max latitudes?
min(train$latitude)
max(train$latitude)

train_tmp <- train %>% 
  dplyr::mutate(model = SWO_model6$fitted)
preds <- gbm::predict.gbm(SWO_model6, test, n.trees = SWO_model6$gbm.call$best.trees, type = "response") # predict to test

test_tmp <- test %>% 
  dplyr::mutate(model = preds)

#### January-March
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jan-mar", # season
                        SWO_predict_season1 %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)), # rest of the ocean cells
                        SWO_model6, # BRT model
                        `grid_SWO_jan-mar` %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)) # grid of species for specific season with restricted ranges
)

ggseason1 <- plotModel(gg[[1]], gg[[2]]) + # Plot the model
  ggtitle("January-March")

#### April-June
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "apr-jun", # season
                        SWO_predict_season2 %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)), # rest of the ocean cells
                        SWO_model6, # BRT model
                        `grid_SWO_apr-jun` %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude))# grid of species for specific season
)

ggseason2 <- plotModel(gg[[1]], gg[[2]]) + # Plot the model
  ggtitle("April-June")

#### July-September
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jul-sept", # season
                        SWO_predict_season3 %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)), # rest of the ocean cells
                        SWO_model6, # BRT model
                        `grid_SWO_jul-sept` %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)) # grid of species for specific season
)

ggseason3 <- plotModel(gg[[1]], gg[[2]]) + # Plot the model
  ggtitle("July-September")

#### October-December
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "oct-dec", # season
                        SWO_predict_season4 %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)), # rest of the ocean cells
                        SWO_model6, # BRT model
                        `grid_SWO_oct-dec` %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude))# grid of species for specific season
)

ggseason4 <- plotModel(gg[[1]], gg[[2]]) + # Plot the model
  ggtitle("October-December")

ggseasons <- (ggseason1 + ggseason2) / (ggseason3 + ggseason4) + 
  plot_layout(guides = "collect") +
  plot_annotation("Model 6: Additional predictors with best test AUC (AUC: 0.85)")

ggsave(plot = ggseasons, filename = "Figures/SWO/SWO_Model6.png", width = 27, height = 15, dpi = 600)

#### Plot test vs predictors ####
pdf(file = "Figures/SWO/SWO_Model6_PredictorsTrain.pdf", width = 10, height = 8)
gbm.plot.fits(SWO_model6)
dev.off()

longitude <- ggplot() +
  geom_point(data = test_tmp, aes(x = longitude, y = model)) +
  ylab("fitted values") +
  theme_bw()
latitude <- ggplot() +
  geom_point(data = test_tmp, aes(x = latitude, y = model)) +
  ylab("fitted values") +
  theme_bw()
season <- ggplot() +
  geom_boxplot(data = test_tmp, aes(x = season, y = model)) +
  ylab("fitted values") +
  theme_bw()
tos <- ggplot() +
  geom_point(data = test_tmp, aes(x = tos_transformed, y = model)) +
  ylab("fitted values") +
  theme_bw()
o2os <- ggplot() +
  geom_point(data = test_tmp, aes(x = o2os_transformed, y = model)) +
  ylab("fitted values") +
  theme_bw()
phos <- ggplot() +
  geom_point(data = test_tmp, aes(x = phos_transformed, y = model)) +
  ylab("fitted values") +
  theme_bw()
chlos <- ggplot() +
  geom_point(data = test_tmp, aes(x = chlos_transformed, y = model)) +
  ylab("fitted values") +
  theme_bw()
sos <- ggplot() +
  geom_point(data = test_tmp, aes(x = sos_transformed, y = model)) +
  ylab("fitted values") +
  theme_bw()
mlotst <- ggplot() +
  geom_point(data = test_tmp, aes(x = mlotst_transformed, y = model)) +
  ylab("fitted values") +
  theme_bw()
no3os <- ggplot() +
  geom_point(data = test_tmp, aes(x = no3os_transformed, y = model)) +
  ylab("fitted values") +
  theme_bw()
po4os <- ggplot() +
  geom_point(data = test_tmp, aes(x = po4os_transformed, y = model)) +
  ylab("fitted values") +
  theme_bw()
nh4os <- ggplot() +
  geom_point(data = test_tmp, aes(x = nh4os_transformed, y = model)) +
  ylab("fitted values") +
  theme_bw()
depth <- ggplot() +
  geom_point(data = test_tmp, aes(x = meanDepth, y = model)) +
  ylab("fitted values") +
  theme_bw()
coast <- ggplot() +
  geom_point(data = test_tmp, aes(x = coastDistance, y = model)) +
  ylab("fitted values") +
  theme_bw()

ggpredictors <- (longitude | latitude | season | tos) /
  (o2os | phos | chlos | sos) /
  (mlotst | no3os | po4os | nh4os) /
  (depth | coast | plot_spacer() | plot_spacer())
ggsave(filename = "Figures/SWO/SWO_Model6_PredictorsTest.pdf", plot = ggpredictors, width = 12, height = 8, dpi = 300)

###########################################################################
## Model 7: Least overfitting w/ moderate complexity (additional predictors) ##
###########################################################################
# Load the model 7
SWO_model7 <- readRDS("Output/Models/SWO_model7.rds")

# What are the min and max latitudes?
min(train$latitude)
max(train$latitude)

train_tmp <- train %>% 
  dplyr::mutate(model = SWO_model7$fitted)
preds <- gbm::predict.gbm(SWO_model7, test, n.trees = SWO_model7$gbm.call$best.trees, type = "response") # predict to test

test_tmp <- test %>% 
  dplyr::mutate(model = preds)

#### January-March
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jan-mar", # season
                        SWO_predict_season1 %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)), # rest of the ocean cells
                        SWO_model7, # BRT model
                        `grid_SWO_jan-mar` %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)) # grid of species for specific season with restricted ranges
)

ggseason1 <- plotModel(gg[[1]], gg[[2]]) + # Plot the model
  ggtitle("January-March")

#### April-June
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "apr-jun", # season
                        SWO_predict_season2 %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)), # rest of the ocean cells
                        SWO_model7, # BRT model
                        `grid_SWO_apr-jun` %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude))# grid of species for specific season
)

ggseason2 <- plotModel(gg[[1]], gg[[2]]) + # Plot the model
  ggtitle("April-June")

#### July-September
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jul-sept", # season
                        SWO_predict_season3 %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)), # rest of the ocean cells
                        SWO_model7, # BRT model
                        `grid_SWO_jul-sept` %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)) # grid of species for specific season
)

ggseason3 <- plotModel(gg[[1]], gg[[2]]) + # Plot the model
  ggtitle("July-September")

#### October-December
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "oct-dec", # season
                        SWO_predict_season4 %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude)), # rest of the ocean cells
                        SWO_model7, # BRT model
                        `grid_SWO_oct-dec` %>% dplyr::filter(latitude >= min(train$latitude) & latitude <= max(train$latitude))# grid of species for specific season
)

ggseason4 <- plotModel(gg[[1]], gg[[2]]) + # Plot the model
  ggtitle("October-December")

ggseasons <- (ggseason1 + ggseason2) / (ggseason3 + ggseason4) + 
  plot_layout(guides = "collect") +
  plot_annotation("Model 7: Least overfitting w/ moderate complexity (AUC: 0.86)")

ggsave(plot = ggseasons, filename = "Figures/SWO/SWO_Model7.png", width = 27, height = 15, dpi = 600)
