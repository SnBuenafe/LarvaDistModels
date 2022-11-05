# Load preliminaries
source("00_Utils.R")
# Load dfs
source("01a_DataLayers_Assembling.R")

# Load yellowfin tuna full dataset
YFT_ds <- read_csv("Output/YFT_full.csv", show_col_types = FALSE)

# Always build models with known data only
YFT_filtered <- YFT_ds %>% 
  dplyr::filter(!is.na(abundance)) %>% # filter those with data!
  dplyr::mutate(across(where(is.character), ~factor(.))) %>% # convert all characters to factors
  dplyr::mutate(abundance_presence = case_when(abundance > 0 ~ 1,
                                               abundance == 0 ~ 0)) %>%  # mutate the abundance data into 1s and 0s
  dplyr::select(-geometry) %>% 
  as.data.frame()

# Data.frame for predictions
YFT_predict <- YFT_ds %>% 
  dplyr::filter(is.na(abundance)) %>% # filter those with data!
  dplyr::mutate(across(where(is.character), ~factor(.))) %>% # convert all characters to factors
  dplyr::mutate(abundance_presence = case_when(abundance > 0 ~ 1,
                                               abundance == 0 ~ 0)) %>%  # mutate the abundance data into 1s and 0s
  dplyr::select(-geometry) %>% 
  as.data.frame() #gbm.step doesn't work if it's a tibble...

#### Model 0 ####
# Using ocean basins
# Check the plot
prob <- read_csv("Output/python/YFT_model0.csv", show_col_types = FALSE)[,2] %>% pull()
YFT_filtered$model <- prob
YFT_sf <- grid %>% # convert to sf so we can plot
  dplyr::left_join(YFT_filtered, ., by = "cellID") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

plotModel(YFT_sf, "Figures/YFT_PythonModel0.png") # Plot the model

pred <- read_csv("Output/python/YFT_model0_preds.csv", show_col_types = FALSE)[,2] %>% pull()
YFT_predict$predictions <- pred
YFT_predict_sf <- grid_YFT %>% # convert to sf so we can plot
  dplyr::select(-species, -abundance, -season, -longitude, -latitude) %>% 
  dplyr::left_join(YFT_predict, ., by = "cellID") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

plotPredictions(YFT_predict_sf, "Figures/YFT_PythonModel0preds.png") # Plot predictions

#### Model 1 ####
# model 1: longitude and latitude
prob <- read_csv("Output/python/YFT_model1.csv", show_col_types = FALSE)[,2] %>% pull()
YFT_filtered$model <- prob
YFT_sf <- grid %>% # convert to sf so we can plot
  dplyr::left_join(YFT_filtered, ., by = "cellID") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

plotModel(YFT_sf, "Figures/YFT_PythonModel1.png") # Plot the model

pred <- read_csv("Output/python/YFT_model1_preds.csv", show_col_types = FALSE)[,2] %>% pull()
YFT_predict$predictions <- pred
YFT_predict_sf <- grid_YFT %>% # convert to sf so we can plot
  dplyr::select(-species, -abundance, -season, -longitude, -latitude) %>% 
  dplyr::left_join(YFT_predict, ., by = "cellID") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

plotPredictions(YFT_predict_sf, "Figures/YFT_PythonModel1preds.png") # Plot predictions

#### Model 2 ####
# model 2: no geographic location
prob <- read_csv("Output/python/YFT_model2.csv", show_col_types = FALSE)[,2] %>% pull()
YFT_filtered$model <- prob
YFT_sf <- grid %>% # convert to sf so we can plot
  dplyr::left_join(YFT_filtered, ., by = "cellID") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

plotModel(YFT_sf, "Figures/YFT_PythonModel2.png") # Plot the model

pred <- read_csv("Output/python/YFT_model2_preds.csv", show_col_types = FALSE)[,2] %>% pull()
YFT_predict$predictions <- pred
YFT_predict_sf <- grid_YFT %>% # convert to sf so we can plot
  dplyr::select(-species, -abundance, -season, -longitude, -latitude) %>% 
  dplyr::left_join(YFT_predict, ., by = "cellID") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

plotPredictions(YFT_predict_sf, "Figures/YFT_PythonModel2preds.png") # Plot predictions

#### Model 3 ####
# model 3: latitude
prob <- read_csv("Output/python/YFT_model3.csv", show_col_types = FALSE)[,2] %>% pull()
YFT_filtered$model <- prob
YFT_sf <- grid %>% # convert to sf so we can plot
  dplyr::left_join(YFT_filtered, ., by = "cellID") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

plotModel(YFT_sf, "Figures/YFT_PythonModel3.png") # Plot the model

pred <- read_csv("Output/python/YFT_model3_preds.csv", show_col_types = FALSE)[,2] %>% pull()
YFT_predict$predictions <- pred
YFT_predict_sf <- grid_YFT %>% # convert to sf so we can plot
  dplyr::select(-species, -abundance, -season, -longitude, -latitude) %>% 
  dplyr::left_join(YFT_predict, ., by = "cellID") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

plotPredictions(YFT_predict_sf, "Figures/YFT_PythonModel3preds.png") # Plot predictions

#### Model 4 ####
# model 4: longitude
prob <- read_csv("Output/python/YFT_model4.csv", show_col_types = FALSE)[,2] %>% pull()
YFT_filtered$model <- prob
YFT_sf <- grid %>% # convert to sf so we can plot
  dplyr::left_join(YFT_filtered, ., by = "cellID") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

plotModel(YFT_sf, "Figures/YFT_PythonModel4.png") # Plot the model

pred <- read_csv("Output/python/YFT_model4_preds.csv", show_col_types = FALSE)[,2] %>% pull()
YFT_predict$predictions <- pred
YFT_predict_sf <- grid_YFT %>% # convert to sf so we can plot
  dplyr::select(-species, -abundance, -season, -longitude, -latitude) %>% 
  dplyr::left_join(YFT_predict, ., by = "cellID") %>% 
  sf::st_as_sf(sf_column_name = "geometry")

plotPredictions(YFT_predict_sf, "Figures/YFT_PythonModel4preds.png") # Plot predictions
