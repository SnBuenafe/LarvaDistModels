# DESCRIPTION: Remove 10 degree grid cells that have <25 cells as sampling points (25%)

# Load preliminaries
source("02a_YFT_Data.R") # Load YFT data

# Build the 10x10 grid
grid_100 <- sf::st_make_grid(Bndry,
                         square = TRUE,
                         cellsize = c(10,10),
                         what = "polygons") %>%
  sf::st_sf()

# First get all the PUs partially/wholly within the planning region
logi_Reg <- sf::st_centroid(grid_100) %>%
  sf::st_intersects(Bndry) %>%
  lengths > 0 # Get logical vector instead of sparse geometry binary

grid_100 <- grid_100[logi_Reg, ] # Get TRUE

# Second, get all the pu's with < 50 % area on land (approximated from the centroid)
logi_Ocean <- sf::st_centroid(grid_100) %>%
  sf::st_intersects(oceans) %>%
  lengths > 0 # Get logical vector instead of sparse geometry binary

grid_100 <- grid_100[logi_Ocean==TRUE, ] # Get TRUE

grid_100 %<>%
  dplyr::mutate(cellID = dplyr::row_number()) # Add a cell ID reference

# Associate seasonal grids with the 10x10 grid
season_grid <- `grid_YFT_jan-mar` %>% 
  sf::st_as_sf()
tmp <- sf::st_nearest_feature(season_grid, grid_100)

new_grid <- `grid_YFT_jan-mar` %>% 
  dplyr::mutate(grid_100_category = grid_100$cellID[tmp[cellID]])

# Take the # of 1x1 grid cells that are considered sampling points in each 10x10 grid cells
new_grid %<>%
  dplyr::mutate(samp_point = ifelse(!is.na(abundance), yes = 1, no = abundance)) %>% # counting sampling points
  dplyr::group_by(grid_100_category) %>% 
  dplyr::mutate(samp_point = sum(samp_point, na.rm = TRUE), count = n()) %>% 
  dplyr::mutate(grid_1_perc = abundance*100/count) %>% 
  ungroup()

# Try for model 1
YFT_model1 <- readRDS("Output/Models/YFT_model1.rds")
preds <- gbm::predict.gbm(YFT_model1, test, n.trees = YFT_model1$gbm.call$best.trees, type = "response")

# Plot maps
train_tmp <- train %>% 
  dplyr::mutate(model = YFT_model1$fitted)
test_tmp <- test %>% 
  dplyr::mutate(model = preds)

gg <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jan-mar", # season
                        YFT_predict_season1, # rest of the ocean cells
                        YFT_model1, # BRT model
                        `grid_YFT_jan-mar` # grid of species for specific season
)

new_gg <- dplyr::left_join(new_grid, gg[[1]] %>% tibble::as_tibble()) %>% 
  dplyr::select(cellID, ocean, model, grid_100_category, grid_1_perc, geometry) %>% 
  sf::st_as_sf()

filt_cellID <- new_grid %>% 
  dplyr::filter(grid_1_perc > 0.25) %>%  # cellID of 10x10 grid that have >25% 1x1 grid cells
  dplyr::select(grid_100_category) %>% 
  unique() %>% 
  pull()

new_gg1 <- plotModel(new_gg, YFT_ds1) + # Plot the squished model
  geom_sf(data = grid_100 %>% dplyr::filter(cellID %in% filt_cellID), fill = NA, color = "red")

new_gg1_abundance <- ggplot() +
  geom_sf(data = new_grid %>% 
            dplyr::filter(!is.na(abundance)) %>% 
            sf::st_as_sf(), aes(fill = abundance)) +
  geom_sf(data = landmass, fill = "black", color = NA, size = 0.01) +
  geom_sf(data = grid_100 %>% dplyr::filter(cellID %in% filt_cellID), fill = NA, color = "red") 

new_gg1 + new_gg1_abundance

new_gg2 <- plotModel(new_gg %>% dplyr::filter(grid_1_perc >= 10), YFT_ds1) # Plot model with >10% of its area having sampling points
