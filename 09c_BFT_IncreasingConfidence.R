# DESCRIPTION: Increasing confidence of seasonal species distribution maps

# Load preliminaries
source("09a_BFT_Data.R") # Load BFT data
model_dir <- here::here("Output", "Models")
preds_dir <- here::here("Output", "Predictions")
figure_dir <- here::here("Figures", "BFT")
model <- readRDS(here::here(model_dir, "BFT_model1.rds")) # Load model we're using (here we're using the full model)

# Making sure train and test data sets have the fitted predictions
preds <- gbm::predict.gbm(model, test, n.trees = model$gbm.call$best.trees, type = "response")
train_tmp <- train %>%
  dplyr::mutate(model = model$fitted)
test_tmp <- test %>%
  dplyr::mutate(model = preds)

#### January-March ####
# Associate seasonal grids with the 10x10 grid
full_grid <- associateGrids(`grid_BFT_jan-mar`, grid_100)

# Create seasonal map
gg <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jan-mar", # season
                        BFT_predict_season1, # rest of the ocean cells
                        model, # BRT model
                        `grid_BFT_jan-mar` # grid of species for specific season
)

# Filtering 10x10 grid cells that have at least 10% of its area as sampling points
gg_filt <- restrictThreshold(full_grid,
                             gg,
                             10)
saveRDS(object = gg_filt, file = here::here(preds_dir, paste("BFT", "jan-mar.csv", sep = "_"))) # save predictions

# Plot model removing 10x10 areas with lower confidence
gg1 <- plotConfidence(gg_filt, full_grid)

#### April-June ####
# Associate seasonal grids with the 10x10 grid
full_grid <- associateGrids(`grid_BFT_apr-jun`, grid_100)

# Create seasonal map
gg <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "apr-jun", # season
                        BFT_predict_season2, # rest of the ocean cells
                        model, # BRT model
                        `grid_BFT_apr-jun` # grid of species for specific season
)

# Filtering 10x10 grid cells that have at least 10% of its area as sampling points
gg_filt <- restrictThreshold(full_grid,
                             gg,
                             10)
saveRDS(object = gg_filt, file = here::here(preds_dir, paste("BFT", "apr-jun.csv", sep = "_"))) # save predictions

# Plot model removing 10x10 areas with lower confidence
gg2 <- plotConfidence(gg_filt, full_grid)

#### July-September ####
# Associate seasonal grids with the 10x10 grid
full_grid <- associateGrids(`grid_BFT_jul-sept`, grid_100)

# Create seasonal map
gg <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jul-sept", # season
                        BFT_predict_season3, # rest of the ocean cells
                        model, # BRT model
                        `grid_BFT_jul-sept` # grid of species for specific season
)

# Filtering 10x10 grid cells that have at least 10% of its area as sampling points
gg_filt <- restrictThreshold(full_grid,
                             gg,
                             10)
saveRDS(object = gg_filt, file = here::here(preds_dir, paste("BFT", "jul-sept.csv", sep = "_"))) # save predictions

# Plot model removing 10x10 areas with lower confidence
gg3 <- plotConfidence(gg_filt, full_grid)

#### October-December ####
# Associate seasonal grids with the 10x10 grid
full_grid <- associateGrids(`grid_BFT_oct-dec`, grid_100)

# Create seasonal map
gg <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "oct-dec", # season
                        BFT_predict_season4, # rest of the ocean cells
                        model, # BRT model
                        `grid_BFT_oct-dec` # grid of species for specific season
)

# Filtering 10x10 grid cells that have at least 10% of its area as sampling points
gg_filt <- restrictThreshold(full_grid,
                             gg,
                             10)
saveRDS(object = gg_filt, file = here::here(preds_dir, paste("BFT", "oct-dec.csv", sep = "_"))) # save predictions

# Plot model removing 10x10 areas with lower confidence
gg4 <- plotConfidence(gg_filt, full_grid)

gg_full <- (gg1 + gg2) / (gg3 + gg4) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = gg_full, filename = here::here(figure_dir, "BFT_model1_highconf.png"), width = 27, height = 15, dpi = 600)
