# Make a 250km x 250km grid

Bndry <- spatialplanr::SpatPlan_Get_Boundary(Limits = c(xmin = -40, xmax = 40, ymax = 45, ymin = -40),
                                             cCRS = moll_pacific) 

diameter <- sqrt(62500*1e6)

grid_250 <- sf::st_make_grid(Bndry,
                             square = TRUE,
                             cellsize = c(diameter, diameter) # 5 times the size of the grid
                             ) %>% 
  sf::st_sf()

# Removing degree grids that wholly or partially intersect with the landmass object.
logi_Reg <- grid_250 %>% 
  sf::st_intersects(grid) %>%
  lengths > 0 # Get logical vector instead of sparse geometry binary

grid_250_filt <- grid_250[logi_Reg, ]

ggplot() + geom_sf(data = grid, color = "red") + geom_sf(data = grid_250_filt, fill = NA, color = "black")

# Superimpose it with the YFT data
source("02a_YFT_Data.R")
YFT_model1 <- readRDS("Output/Models/YFT_model1.rds")

preds <- gbm::predict.gbm(YFT_model1, test, n.trees = YFT_model1$gbm.call$best.trees, type = "response") # predict to test

train_tmp <- train %>% 
  dplyr::mutate(model = YFT_model1$fitted)
preds <- gbm::predict.gbm(YFT_model1, test, n.trees = YFT_model1$gbm.call$best.trees, type = "response") # predict to test

test_tmp <- test %>% 
  dplyr::mutate(model = preds)

# January-March
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jan-mar", # season
                        YFT_predict_season1, # rest of the ocean cells
                        YFT_model1, # BRT model
                        `grid_YFT_jan-mar` # grid of species for specific season
)

tmp <- sf::st_nearest_feature(gg[[1]], grid_250_filt)

plotSquishedModel(gg[[1]], YFT_ds1) +
  geom_sf(data = grid_250_filt, color = "red", fill = NA)
