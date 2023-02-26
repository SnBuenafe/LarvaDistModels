# DESCRIPTION: Plotting potential models for ALB

###########################
## Load preliminaries ##
###########################
# Load ALB data
source("04a_ALB_Data.R")

############################
## Model 2: Best test AUC ##
############################

# Load the model 2
ALB_model2 <- readRDS("Output/Models/ALB_model2.rds")

# What are the min and max latitudes?
min(ALB_build$latitude)
max(ALB_build$latitude)

train_tmp <- train %>% 
  dplyr::mutate(model = ALB_model2$fitted)
preds <- gbm::predict.gbm(ALB_model2, test, n.trees = ALB_model2$gbm.call$best.trees, type = "response") # predict to test

test_tmp <- test %>% 
  dplyr::mutate(model = preds)

#### January-March
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jan-mar", # season
                        ALB_predict_season1 %>% dplyr::filter(latitude >= min(ALB_build$latitude) & latitude <= max(ALB_build$latitude)), # rest of the ocean cells
                        ALB_model2, # BRT model
                        `grid_ALB_jan-mar` %>% dplyr::filter(latitude >= min(ALB_build$latitude) & latitude <= max(ALB_build$latitude)) # grid of species for specific season with restricted ranges
)

ggseason1 <- plotModel(gg[[1]]) # Plot the model

ggsquish1 <- plotSquishedModel(gg[[1]]) # Plot the squished model

#### April-June
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "apr-jun", # season
                        ALB_predict_season2 %>% dplyr::filter(latitude >= min(ALB_build$latitude) & latitude <= max(ALB_build$latitude)), # rest of the ocean cells
                        ALB_model2, # BRT model
                        `grid_ALB_apr-jun` %>% dplyr::filter(latitude >= min(ALB_build$latitude) & latitude <= max(ALB_build$latitude))# grid of species for specific season
)

ggseason2 <- plotModel(gg[[1]]) # Plot the model

ggsquish2 <- plotSquishedModel(gg[[1]]) # Plot the squished model

#### July-September
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jul-sept", # season
                        ALB_predict_season3 %>% dplyr::filter(latitude >= min(ALB_build$latitude) & latitude <= max(ALB_build$latitude)), # rest of the ocean cells
                        ALB_model2, # BRT model
                        `grid_ALB_jul-sept` %>% dplyr::filter(latitude >= min(ALB_build$latitude) & latitude <= max(ALB_build$latitude)) # grid of species for specific season
)

ggseason3 <- plotModel(gg[[1]]) # Plot the model

ggsquish3 <- plotSquishedModel(gg[[1]]) # Plot the squished model

#### October-December
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "oct-dec", # season
                        ALB_predict_season4 %>% dplyr::filter(latitude >= min(ALB_build$latitude) & latitude <= max(ALB_build$latitude)), # rest of the ocean cells
                        ALB_model2, # BRT model
                        `grid_ALB_oct-dec` %>% dplyr::filter(latitude >= min(ALB_build$latitude) & latitude <= max(ALB_build$latitude))# grid of species for specific season
)

ggseason4 <- plotModel(gg[[1]]) # Plot the model

ggsquish4 <- plotSquishedModel(gg[[1]]) # Plot the squished model

ggseasons <- (ggseason1 + ggseason2) / (ggseason3 + ggseason4) + 
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = ggseasons, filename = "Figures/ALB/ALB_model2.png", width = 27, height = 15, dpi = 600)

ggsquished <- (ggsquish1 + ggsquish2) / (ggsquish3 + ggsquish4) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = ggsquished, filename = "Figures/ALB/ALB_model2_squished.png", width = 27, height = 15, dpi = 600)

#### Plot relative importance of variables ####
rel_imp <- summary(ALB_model2)

ggrel <- ggplot(data = rel_imp, aes(x = reorder(var, rel.inf), y = rel.inf)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(expand = c(0,0)) +
  coord_flip() +
  theme_classic()

ggsave(plot = ggrel, filename = "Figures/ALB/ALB_Model2_RelImportance.png", width = 7, height = 5, dpi = 300)

#### Plot test vs predictors ####
ggpredictors <- plotPredictors(train_tmp)
ggsave(file = "Figures/ALB/ALB_Model2_PredictorsTrain.pdf", plot = ggpredictors, width = 12, height = 8, dpi = 300)

ggpredictors <- plotPredictors(test_tmp)
ggsave(filename = "Figures/ALB/ALB_model2_PredictorsTest.pdf", plot = ggpredictors, width = 12, height = 8, dpi = 300)
