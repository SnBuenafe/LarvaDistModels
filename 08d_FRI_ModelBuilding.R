# DESCRIPTION: Plotting potential models for FRI

###########################
## Load preliminaries ##
###########################
# Load FRI data
source("08a_FRI_Data.R")

###########################################################################
## Model 2: Best test AUC (additional predictors) ##
###########################################################################
# Load the model 2
FRI_model2 <- readRDS("Output/Models/FRI_model2.rds")

# What are the min and max latitudes?
min(FRI_build$latitude)
max(FRI_build$latitude)

train_tmp <- train %>% 
  dplyr::mutate(model = FRI_model2$fitted)
preds <- gbm::predict.gbm(FRI_model2, test, n.trees = FRI_model2$gbm.call$best.trees, type = "response") # predict to test

test_tmp <- test %>% 
  dplyr::mutate(model = preds)

#### January-March
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jan-mar", # season
                        FRI_predict_season1 %>% dplyr::filter(latitude >= min(FRI_build$latitude) & latitude <= max(FRI_build$latitude)), # rest of the ocean cells
                        FRI_model2, # BRT model
                        `grid_FRI_jan-mar` %>% dplyr::filter(latitude >= min(FRI_build$latitude) & latitude <= max(FRI_build$latitude)) # grid of species for specific season with restricted ranges
)

ggseason1 <- plotModel(gg[[1]]) + # Plot the model
  ggtitle("January-March")

#### April-June
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "apr-jun", # season
                        FRI_predict_season2 %>% dplyr::filter(latitude >= min(FRI_build$latitude) & latitude <= max(FRI_build$latitude)), # rest of the ocean cells
                        FRI_model2, # BRT model
                        `grid_FRI_apr-jun` %>% dplyr::filter(latitude >= min(FRI_build$latitude) & latitude <= max(FRI_build$latitude))# grid of species for specific season
)

ggseason2 <- plotModel(gg[[1]]) + # Plot the model
  ggtitle("April-June")

#### July-September
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jul-sept", # season
                        FRI_predict_season3 %>% dplyr::filter(latitude >= min(FRI_build$latitude) & latitude <= max(FRI_build$latitude)), # rest of the ocean cells
                        FRI_model2, # BRT model
                        `grid_FRI_jul-sept` %>% dplyr::filter(latitude >= min(FRI_build$latitude) & latitude <= max(FRI_build$latitude)) # grid of species for specific season
)

ggseason3 <- plotModel(gg[[1]]) + # Plot the model
  ggtitle("July-September")

#### October-December
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "oct-dec", # season
                        FRI_predict_season4 %>% dplyr::filter(latitude >= min(FRI_build$latitude) & latitude <= max(FRI_build$latitude)), # rest of the ocean cells
                        FRI_model2, # BRT model
                        `grid_FRI_oct-dec` %>% dplyr::filter(latitude >= min(FRI_build$latitude) & latitude <= max(FRI_build$latitude))# grid of species for specific season
)

ggseason4 <- plotModel(gg[[1]]) + # Plot the model
  ggtitle("October-December")

ggseasons <- (ggseason1 + ggseason2) / (ggseason3 + ggseason4) + 
  plot_layout(guides = "collect") +
  plot_annotation("Model 2: Additional predictors with best test AUC (AUC: 0.89)")

ggsave(plot = ggseasons, filename = "Figures/FRI/FRI_model2.png", width = 27, height = 15, dpi = 600)

#### Plot relative importance of variables ####
rel_imp <- summary(FRI_model2)

ggrel <- ggplot(data = rel_imp, aes(x = reorder(var, rel.inf), y = rel.inf)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(expand = c(0,0)) +
  coord_flip() +
  theme_classic()

ggsave(plot = ggrel, filename = "Figures/FRI/FRI_Model2_RelImportance.png", width = 7, height = 5, dpi = 300)

#### Plot test vs predictors ####
pdf(file = "Figures/FRI/FRI_model2_PredictorsTrain.pdf", width = 10, height = 8)
gbm.plot.fits(FRI_model2)
dev.off()

ggpredictors <- plotPredictors(test_tmp)
ggsave(filename = "Figures/FRI/FRI_model2_PredictorsTest.pdf", plot = ggpredictors, width = 12, height = 8, dpi = 300)

