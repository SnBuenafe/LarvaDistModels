# DESCRIPTION: Plotting potential models for STRM

###########################
## Load preliminaries ##
###########################
# Load STRM data
source("10a_STRM_Data.R")

################################
## Model 3: Least overfitting ##
################################
# Load the model 3
STRM_model3 <- readRDS("Output/Models/STRM_model3.rds")

# What are the min and max latitudes?
min(STRM_build$latitude)
max(STRM_build$latitude)

train_tmp <- train %>% 
  dplyr::mutate(model = STRM_model3$fitted)
preds <- gbm::predict.gbm(STRM_model3, test, n.trees = STRM_model3$gbm.call$best.trees, type = "response") # predict to test

test_tmp <- test %>% 
  dplyr::mutate(model = preds)

#### January-March
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jan-mar", # season
                        STRM_predict_season1 %>% dplyr::filter(latitude >= min(STRM_build$latitude) & latitude <= max(STRM_build$latitude)), # rest of the ocean cells
                        STRM_model3, # BRT model
                        `grid_STRM_jan-mar` %>% dplyr::filter(latitude >= min(STRM_build$latitude) & latitude <= max(STRM_build$latitude)) # grid of species for specific season with restricted ranges
)

ggseason1 <- plotModel(gg[[1]]) # Plot the model

ggsquish1 <- plotSquishedModel(gg[[1]]) # Plot the squished model

#### April-June
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "apr-jun", # season
                        STRM_predict_season2 %>% dplyr::filter(latitude >= min(STRM_build$latitude) & latitude <= max(STRM_build$latitude)), # rest of the ocean cells
                        STRM_model3, # BRT model
                        `grid_STRM_apr-jun` %>% dplyr::filter(latitude >= min(STRM_build$latitude) & latitude <= max(STRM_build$latitude))# grid of species for specific season
)

ggseason2 <- plotModel(gg[[1]]) # Plot the model

ggsquish2 <- plotSquishedModel(gg[[1]]) # Plot the squished model

#### July-September
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jul-sept", # season
                        STRM_predict_season3 %>% dplyr::filter(latitude >= min(STRM_build$latitude) & latitude <= max(STRM_build$latitude)), # rest of the ocean cells
                        STRM_model3, # BRT model
                        `grid_STRM_jul-sept` %>% dplyr::filter(latitude >= min(STRM_build$latitude) & latitude <= max(STRM_build$latitude)) # grid of species for specific season
)

ggseason3 <- plotModel(gg[[1]]) # Plot the model

ggsquish3 <- plotSquishedModel(gg[[1]]) # Plot the squished model

#### October-December
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "oct-dec", # season
                        STRM_predict_season4 %>% dplyr::filter(latitude >= min(STRM_build$latitude) & latitude <= max(STRM_build$latitude)), # rest of the ocean cells
                        STRM_model3, # BRT model
                        `grid_STRM_oct-dec` %>% dplyr::filter(latitude >= min(STRM_build$latitude) & latitude <= max(STRM_build$latitude))# grid of species for specific season
)

ggseason4 <- plotModel(gg[[1]]) # Plot the model

ggsquish4 <- plotSquishedModel(gg[[1]]) # Plot the squished model

ggseasons <- (ggseason1 + ggseason2) / (ggseason3 + ggseason4)  + 
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = ggseasons, filename = "Figures/STRM/STRM_model3.png", width = 27, height = 15, dpi = 600)

ggsquished <- (ggsquish1 + ggsquish2) / (ggsquish3 + ggsquish4)  + 
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = ggsquished, filename = "Figures/STRM/STRM_model3_squished.png", width = 27, height = 15, dpi = 600)

#### Plot relative importance of variables ####
rel_imp <- summary(STRM_model3)

ggrel <- ggplot(data = rel_imp, aes(x = reorder(var, rel.inf), y = rel.inf)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(expand = c(0,0)) +
  coord_flip() +
  theme_classic()

ggsave(plot = ggrel, filename = "Figures/STRM/STRM_model3_RelImportance.png", width = 7, height = 5, dpi = 300)

#### Plot test vs predictors ####
ggpredictors <- plotPredictors(train_tmp)
ggsave(file = "Figures/STRM/STRM_model3_PredictorsTrain.pdf", plot = ggpredictors, width = 12, height = 8, dpi = 300)

ggpredictors <- plotPredictors(test_tmp)
ggsave(filename = "Figures/STRM/STRM_model3_PredictorsTest.pdf", plot = ggpredictors, width = 12, height = 8, dpi = 300)
