# DESCRIPTION: ALB Dataset

# Load preliminaries
source("00_Utils.R")

sf <- combineFish(species = "albacore") %>% 
  sf::st_transform(crs = moll) %>% 
  sf::st_centroid() # transform into point data

seasons <- c("jan-mar", "apr-jun", "jul-sept", "oct-dec")
for(s in 1:length(seasons)) {
  gridded <- assembleGrid(grid, sf %>% dplyr::filter(season == seasons[s]))
  
  assign(paste("grid", "ALB", seasons[s], sep = "_"), gridded)
}

# Load skipjack tuna datasets
ALB_ds1 <- read_csv("Output/CSV/ALB_historical_jan-mar.csv", show_col_types = FALSE)
ALB_ds2 <- read_csv("Output/CSV/ALB_historical_apr-jun.csv", show_col_types = FALSE)
ALB_ds3 <- read_csv("Output/CSV/ALB_historical_jul-sept.csv", show_col_types = FALSE)
ALB_ds4 <- read_csv("Output/CSV/ALB_historical_oct-dec.csv", show_col_types = FALSE)

# Build model with known data only
ALB_build <- dplyr::bind_rows(ALB_ds1 %>% dplyr::filter(!is.na(abundance)),
                              ALB_ds2 %>% dplyr::filter(!is.na(abundance)),
                              ALB_ds3 %>% dplyr::filter(!is.na(abundance)),
                              ALB_ds4 %>% dplyr::filter(!is.na(abundance))) %>% 
  dplyr::mutate(across(where(is.character), ~factor(.))) %>% # convert all characters to factors
  dplyr::mutate(abundance_presence = case_when(abundance > 0 ~ 1,
                                               abundance == 0 ~ 0), 
                row = row_number()) %>%  # mutate the abundance data into 1s and 0s
  dplyr::select(-geometry) %>% 
  dplyr::select(row, cellID, species, abundance, abundance_presence, ocean, longitude, latitude, season, everything()) %>% # arrange columns
  as.data.frame() #gbm.step doesn't work if it's a tibble...

# We divide the data into train (training and validation) and test
nrow(ALB_build) * 0.9 # = 11890

set.seed(5301)
train <- slice_sample(ALB_build, n = 11890, replace = FALSE) # 90% training set
test <- ALB_build[!ALB_build$row %in% train$row, ] # 10% testing set

# Data.frame for predictions
# January-March predictions
ALB_predict_season1 <- ALB_ds1 %>% dplyr::filter(is.na(abundance)) %>% 
  dplyr::mutate(across(where(is.character), ~factor(.))) %>% # convert all characters to factors
  dplyr::select(-geometry, -abundance, -species) %>% 
  dplyr::select(cellID, ocean, longitude, latitude, season, everything()) %>% # arrange columns
  dplyr::mutate(season = "jan-mar") %>% 
  as.data.frame() #gbm.step doesn't work if it's a tibble...

# April-June
ALB_predict_season2 <- ALB_ds2 %>% dplyr::filter(is.na(abundance)) %>% 
  dplyr::mutate(across(where(is.character), ~factor(.))) %>% # convert all characters to factors
  dplyr::select(-geometry, -abundance, -species) %>% 
  dplyr::select(cellID, ocean, longitude, latitude, season, everything()) %>% # arrange columns
  dplyr::mutate(season = "apr-jun") %>% 
  as.data.frame() #gbm.step doesn't work if it's a tibble...

# July-September
ALB_predict_season3 <- ALB_ds3 %>% dplyr::filter(is.na(abundance)) %>% 
  dplyr::mutate(across(where(is.character), ~factor(.))) %>% # convert all characters to factors
  dplyr::select(-geometry, -abundance, -species) %>% 
  dplyr::select(cellID, ocean, longitude, latitude, season, everything()) %>% # arrange columns
  dplyr::mutate(season = "jul-sept") %>% 
  as.data.frame() #gbm.step doesn't work if it's a tibble...

# October-December
ALB_predict_season4 <- ALB_ds4 %>% dplyr::filter(is.na(abundance)) %>% 
  dplyr::mutate(across(where(is.character), ~factor(.))) %>% # convert all characters to factors
  dplyr::select(-geometry, -abundance, -species) %>% 
  dplyr::select(cellID, ocean, longitude, latitude, season, everything()) %>% # arrange columns
  dplyr::mutate(season = "oct-dec") %>% 
  as.data.frame() #gbm.step doesn't work if it's a tibble...