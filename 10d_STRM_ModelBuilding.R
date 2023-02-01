# DESCRIPTION: Plotting potential models for STRM

###########################
## Load preliminaries ##
###########################
# Load STRM data
source("10a_STRM_Data.R")

###########################################################################
## Model 4: Less overfitting with considerable AUC ##
###########################################################################
# Load the model 4
STRM_model4 <- readRDS("Output/Models/STRM_model4.rds")

# What are the min and max latitudes?
min(STRM_build$latitude)
max(STRM_build$latitude)

train_tmp <- train %>% 
  dplyr::mutate(model = STRM_model4$fitted)
preds <- gbm::predict.gbm(STRM_model4, test, n.trees = STRM_model4$gbm.call$best.trees, type = "response") # predict to test

test_tmp <- test %>% 
  dplyr::mutate(model = preds)

#### January-March
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jan-mar", # season
                        STRM_predict_season1 %>% dplyr::filter(latitude >= min(STRM_build$latitude) & latitude <= max(STRM_build$latitude)), # rest of the ocean cells
                        STRM_model4, # BRT model
                        `grid_STRM_jan-mar` %>% dplyr::filter(latitude >= min(STRM_build$latitude) & latitude <= max(STRM_build$latitude)) # grid of species for specific season with restricted ranges
)

ggseason1 <- plotModel(gg[[1]]) + # Plot the model
  ggtitle("January-March")

ggsquish1 <- plotSquishedModel(gg[[1]]) + # Plot the squished model
  ggtitle("January-March")

#### April-June
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "apr-jun", # season
                        STRM_predict_season2 %>% dplyr::filter(latitude >= min(STRM_build$latitude) & latitude <= max(STRM_build$latitude)), # rest of the ocean cells
                        STRM_model4, # BRT model
                        `grid_STRM_apr-jun` %>% dplyr::filter(latitude >= min(STRM_build$latitude) & latitude <= max(STRM_build$latitude))# grid of species for specific season
)

ggseason2 <- plotModel(gg[[1]]) + # Plot the model
  ggtitle("April-June")

ggsquish2 <- plotSquishedModel(gg[[1]]) + # Plot the squished model
  ggtitle("April-June")

#### July-September
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jul-sept", # season
                        STRM_predict_season3 %>% dplyr::filter(latitude >= min(STRM_build$latitude) & latitude <= max(STRM_build$latitude)), # rest of the ocean cells
                        STRM_model4, # BRT model
                        `grid_STRM_jul-sept` %>% dplyr::filter(latitude >= min(STRM_build$latitude) & latitude <= max(STRM_build$latitude)) # grid of species for specific season
)

ggseason3 <- plotModel(gg[[1]]) + # Plot the model
  ggtitle("July-September")

ggsquish3 <- plotSquishedModel(gg[[1]]) + # Plot the squished model
  ggtitle("July-September")

#### October-December
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "oct-dec", # season
                        STRM_predict_season4 %>% dplyr::filter(latitude >= min(STRM_build$latitude) & latitude <= max(STRM_build$latitude)), # rest of the ocean cells
                        STRM_model4, # BRT model
                        `grid_STRM_oct-dec` %>% dplyr::filter(latitude >= min(STRM_build$latitude) & latitude <= max(STRM_build$latitude))# grid of species for specific season
)

ggseason4 <- plotModel(gg[[1]]) + # Plot the model
  ggtitle("October-December")

ggsquish4 <- plotSquishedModel(gg[[1]]) + # Plot the squished model
  ggtitle("October-December")

ggseasons <- (ggseason1 + ggseason2) / (ggseason3 + ggseason4) + 
  plot_layout(guides = "collect") +
  plot_annotation("Model 4: Additional predictors with less overfitting and considerable AUC (AUC: 0.79)")

ggsave(plot = ggseasons, filename = "Figures/STRM/STRM_model4.png", width = 27, height = 15, dpi = 600)

ggsquished <- (ggsquish1 + ggsquish2) / (ggsquish3 + ggsquish4) +
  plot_annotation("Best model (squished); AUC: 0.79")

ggsave(plot = ggsquished, filename = "Figures/STRM/STRM_model4_squished.png", width = 27, height = 15, dpi = 600)

#### Plot relative importance of variables ####
rel_imp <- summary(STRM_model4)

ggrel <- ggplot(data = rel_imp, aes(x = reorder(var, rel.inf), y = rel.inf)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(expand = c(0,0)) +
  coord_flip() +
  theme_classic()

ggsave(plot = ggrel, filename = "Figures/STRM/STRM_model4_RelImportance.png", width = 7, height = 5, dpi = 300)

#### Plot test vs predictors ####
pdf(file = "Figures/STRM/STRM_model4_PredictorsTrain.pdf", width = 10, height = 8)
gbm.plot.fits(STRM_model4)
dev.off()

ggpredictors <- plotPredictors(test_tmp)
ggsave(filename = "Figures/STRM/STRM_model4_PredictorsTest.pdf", plot = ggpredictors, width = 12, height = 8, dpi = 300)
