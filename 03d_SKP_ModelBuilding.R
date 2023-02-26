# DESCRIPTION: Plotting potential models for SKP

###########################
## Load preliminaries ##
###########################
# Load SKP data
source("03a_SKP_Data.R")

###########################################################################
## Model 2: Best test AUC (additional predictors) ##
###########################################################################
# Load the model 2
SKP_model2 <- readRDS("Output/Models/SKP_model2.rds")

# What are the min and max latitudes?
min(SKP_build$latitude)
max(SKP_build$latitude)

train_tmp <- train %>% 
  dplyr::mutate(model = SKP_model2$fitted)
preds <- gbm::predict.gbm(SKP_model2, test, n.trees = SKP_model2$gbm.call$best.trees, type = "response") # predict to test

test_tmp <- test %>% 
  dplyr::mutate(model = preds)

# Find the limits we want to use for squished
quantile(train_tmp$model, 0.99)
quantile(test_tmp$model, 0.99)

#### January-March
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jan-mar", # season
                        SKP_predict_season1 %>% dplyr::filter(latitude >= min(SKP_build$latitude) & latitude <= max(SKP_build$latitude)), # rest of the ocean cells
                        SKP_model2, # BRT model
                        `grid_SKP_jan-mar` %>% dplyr::filter(latitude >= min(SKP_build$latitude) & latitude <= max(SKP_build$latitude)) # grid of species for specific season with restricted ranges
)

ggseason1 <- plotModel(gg[[1]]) # Plot the model

ggsquish1 <- plotSquishedModel(gg[[1]]) # Plot the squished model
  

#### April-June
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "apr-jun", # season
                        SKP_predict_season2 %>% dplyr::filter(latitude >= min(SKP_build$latitude) & latitude <= max(SKP_build$latitude)), # rest of the ocean cells
                        SKP_model2, # BRT model
                        `grid_SKP_apr-jun` %>% dplyr::filter(latitude >= min(SKP_build$latitude) & latitude <= max(SKP_build$latitude))# grid of species for specific season
)

ggseason2 <- plotModel(gg[[1]]) # Plot the model

ggsquish2 <- plotSquishedModel(gg[[1]]) # Plot the squished model

#### July-September
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jul-sept", # season
                        SKP_predict_season3 %>% dplyr::filter(latitude >= min(SKP_build$latitude) & latitude <= max(SKP_build$latitude)), # rest of the ocean cells
                        SKP_model2, # BRT model
                        `grid_SKP_jul-sept` %>% dplyr::filter(latitude >= min(SKP_build$latitude) & latitude <= max(SKP_build$latitude)) # grid of species for specific season
)

ggseason3 <- plotModel(gg[[1]]) # Plot the model

ggsquish3 <- plotSquishedModel(gg[[1]]) # Plot the squished model

#### October-December
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "oct-dec", # season
                        SKP_predict_season4 %>% dplyr::filter(latitude >= min(SKP_build$latitude) & latitude <= max(SKP_build$latitude)), # rest of the ocean cells
                        SKP_model2, # BRT model
                        `grid_SKP_oct-dec` %>% dplyr::filter(latitude >= min(SKP_build$latitude) & latitude <= max(SKP_build$latitude))# grid of species for specific season
)

ggseason4 <- plotModel(gg[[1]]) # Plot the model

ggsquish4 <- plotSquishedModel(gg[[1]]) # Plot the squished model

ggseasons <- (ggseason1 + ggseason2) / (ggseason3 + ggseason4) + 
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = ggseasons, filename = "Figures/SKP/SKP_model2.png", width = 27, height = 15, dpi = 600)

ggsquished <- (ggsquish1 + ggsquish2) / (ggsquish3 + ggsquish4) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = ggsquished, filename = "Figures/SKP/SKP_model2_squished.png", width = 27, height = 15, dpi = 600)

#### Plot relative importance of variables ####
rel_imp <- summary(SKP_model2)

ggrel <- ggplot(data = rel_imp, aes(x = reorder(var, rel.inf), y = rel.inf)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(expand = c(0,0)) +
  coord_flip() +
  theme_classic()

ggsave(plot = ggrel, filename = "Figures/SKP/SKP_model2_RelImportance.png", width = 7, height = 5, dpi = 300)

#### Plot test vs predictors ####
ggpredictors <- plotPredictors(train_tmp)
ggsave(file = "Figures/SKP/SKP_model2_PredictorsTrain.png", plot = ggpredictors, width = 12, height = 8, dpi = 300)

ggpredictors <- plotPredictors(test_tmp)
ggsave(filename = "Figures/SKP/SKP_model2_PredictorsTest.pdf", plot = ggpredictors, width = 12, height = 8, dpi = 300)
