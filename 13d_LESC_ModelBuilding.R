# DESCRIPTION: Plotting potential models for LESC

###########################
## Load preliminaries ##
###########################
# Load LESC data
source("13a_LESC_Data.R")

###########################################################################
## Model 2: Best test AUC ##
###########################################################################
# Load the model 2
LESC_model2 <- readRDS("Output/Models/LESC_model2.rds")

# What are the min and max latitudes?
min(LESC_build$latitude)
max(LESC_build$latitude)

train_tmp <- train %>% 
  dplyr::mutate(model = LESC_model2$fitted)
preds <- gbm::predict.gbm(LESC_model2, test, n.trees = LESC_model2$gbm.call$best.trees, type = "response") # predict to test

test_tmp <- test %>% 
  dplyr::mutate(model = preds)

#### January-March
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jan-mar", # season
                        LESC_predict_season1 %>% dplyr::filter(latitude >= min(LESC_build$latitude) & latitude <= max(LESC_build$latitude)), # rest of the ocean cells
                        LESC_model2, # BRT model
                        `grid_LESC_jan-mar` %>% dplyr::filter(latitude >= min(LESC_build$latitude) & latitude <= max(LESC_build$latitude)) # grid of species for specific season with restricted ranges
)

ggseason1 <- plotModel(gg[[1]]) # Plot the model

ggsquish1 <- plotSquishedModel(gg[[1]]) # Plot the squished model

#### April-June
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "apr-jun", # season
                        LESC_predict_season2 %>% dplyr::filter(latitude >= min(LESC_build$latitude) & latitude <= max(LESC_build$latitude)), # rest of the ocean cells
                        LESC_model2, # BRT model
                        `grid_LESC_apr-jun` %>% dplyr::filter(latitude >= min(LESC_build$latitude) & latitude <= max(LESC_build$latitude))# grid of species for specific season
)

ggseason2 <- plotModel(gg[[1]]) # Plot the model

ggsquish2 <- plotSquishedModel(gg[[1]]) # Plot the squished model

#### July-September
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jul-sept", # season
                        LESC_predict_season3 %>% dplyr::filter(latitude >= min(LESC_build$latitude) & latitude <= max(LESC_build$latitude)), # rest of the ocean cells
                        LESC_model2, # BRT model
                        `grid_LESC_jul-sept` %>% dplyr::filter(latitude >= min(LESC_build$latitude) & latitude <= max(LESC_build$latitude)) # grid of species for specific season
)

ggseason3 <- plotModel(gg[[1]]) # Plot the model

ggsquish3 <- plotSquishedModel(gg[[1]]) # Plot the squished model

#### October-December
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "oct-dec", # season
                        LESC_predict_season4 %>% dplyr::filter(latitude >= min(LESC_build$latitude) & latitude <= max(LESC_build$latitude)), # rest of the ocean cells
                        LESC_model2, # BRT model
                        `grid_LESC_oct-dec` %>% dplyr::filter(latitude >= min(LESC_build$latitude) & latitude <= max(LESC_build$latitude))# grid of species for specific season
)

ggseason4 <- plotModel(gg[[1]]) # Plot the model

ggsquish4 <- plotSquishedModel(gg[[1]]) # Plot the squished model

ggseasons <- (ggseason1 + ggseason2) / (ggseason3 + ggseason4) + 
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = ggseasons, filename = "Figures/LESC/LESC_model2.png", width = 27, height = 15, dpi = 600)

ggsquished <- (ggsquish1 + ggsquish2) / (ggsquish3 + ggsquish4) + 
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = ggsquished, filename = "Figures/LESC/LESC_model2_squished.png", width = 27, height = 15, dpi = 600)

#### Plot relative importance of variables ####
rel_imp <- summary(LESC_model2)

ggrel <- ggplot(data = rel_imp, aes(x = reorder(var, rel.inf), y = rel.inf)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(expand = c(0,0)) +
  coord_flip() +
  theme_classic()

ggsave(plot = ggrel, filename = "Figures/LESC/LESC_model2_RelImportance.png", width = 7, height = 5, dpi = 300)

#### Plot test vs predictors ####
pdf(file = "Figures/LESC/LESC_model2_PredictorsTrain.pdf", width = 10, height = 8)
gbm.plot.fits(LESC_model2)
dev.off()

ggpredictors <- plotPredictors(test_tmp)
ggsave(filename = "Figures/LESC/LESC_model2_PredictorsTest.pdf", plot = ggpredictors, width = 12, height = 8, dpi = 300)
