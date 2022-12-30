# DESCRIPTION: SAIL Dataset

# Load preliminaries
source("00_Utils.R")

sf <- combineFish(species = "sailfish") %>% 
  sf::st_transform(crs = moll) %>% 
  sf::st_centroid() # transform into point data

seasons <- c("jan-mar", "apr-jun", "jul-sept", "oct-dec")
for(s in 1:length(seasons)) {
  gridded <- assembleGrid(grid, sf %>% dplyr::filter(season == seasons[s]))
  
  assign(paste("grid", "SAIL", seasons[s], sep = "_"), gridded)
}

# Load yellowfin tuna datasets
SAIL_ds1 <- read_csv("Output/CSV/SAIL_historical_jan-mar.csv", show_col_types = FALSE)
SAIL_ds2 <- read_csv("Output/CSV/SAIL_historical_apr-jun.csv", show_col_types = FALSE)
SAIL_ds3 <- read_csv("Output/CSV/SAIL_historical_jul-sept.csv", show_col_types = FALSE)
SAIL_ds4 <- read_csv("Output/CSV/SAIL_historical_oct-dec.csv", show_col_types = FALSE)

# Build model with known data only
SAIL_build <- dplyr::bind_rows(SAIL_ds1 %>% dplyr::filter(!is.na(abundance)),
                              SAIL_ds2 %>% dplyr::filter(!is.na(abundance)),
                              SAIL_ds3 %>% dplyr::filter(!is.na(abundance)),
                              SAIL_ds4 %>% dplyr::filter(!is.na(abundance))) %>% 
  dplyr::mutate(across(where(is.character), ~factor(.))) %>% # convert all characters to factors
  dplyr::mutate(abundance_presence = case_when(abundance > 0 ~ 1,
                                               abundance == 0 ~ 0), 
                row = row_number()) %>%  # mutate the abundance data into 1s and 0s
  dplyr::select(-geometry) %>% 
  dplyr::select(row, cellID, species, abundance, abundance_presence, ocean, longitude, latitude, season, everything()) %>% # arrange columns
  as.data.frame() #gbm.step doesn't work if it's a tibble...

# We divide the data into train (training and validation) and test
nrow(SAIL_build) * 0.9 # = 11890

set.seed(5492) # rerun this
train <- slice_sample(SAIL_build, n = 11890, replace = FALSE) # 90% training set
saveRDS(train, "Output/Train/SAIL_train.csv")
test <- SAIL_build[!SAIL_build$row %in% train$row, ] # 10% testing set
saveRDS(test, "Output/Test/SAIL_test.csv")

# Data.frame for predictions
# January-March predictions
SAIL_predict_season1 <- SAIL_ds1 %>% dplyr::filter(is.na(abundance)) %>% 
  dplyr::mutate(across(where(is.character), ~factor(.))) %>% # convert all characters to factors
  dplyr::select(-geometry, -abundance, -species) %>% 
  dplyr::select(cellID, ocean, longitude, latitude, season, everything()) %>% # arrange columns
  dplyr::mutate(season = "jan-mar") %>% 
  as.data.frame() #gbm.step doesn't work if it's a tibble...

# April-June
SAIL_predict_season2 <- SAIL_ds2 %>% dplyr::filter(is.na(abundance)) %>% 
  dplyr::mutate(across(where(is.character), ~factor(.))) %>% # convert all characters to factors
  dplyr::select(-geometry, -abundance, -species) %>% 
  dplyr::select(cellID, ocean, longitude, latitude, season, everything()) %>% # arrange columns
  dplyr::mutate(season = "apr-jun") %>% 
  as.data.frame() #gbm.step doesn't work if it's a tibble...

# July-September
SAIL_predict_season3 <- SAIL_ds3 %>% dplyr::filter(is.na(abundance)) %>% 
  dplyr::mutate(across(where(is.character), ~factor(.))) %>% # convert all characters to factors
  dplyr::select(-geometry, -abundance, -species) %>% 
  dplyr::select(cellID, ocean, longitude, latitude, season, everything()) %>% # arrange columns
  dplyr::mutate(season = "jul-sept") %>% 
  as.data.frame() #gbm.step doesn't work if it's a tibble...

# October-December
SAIL_predict_season4 <- SAIL_ds4 %>% dplyr::filter(is.na(abundance)) %>% 
  dplyr::mutate(across(where(is.character), ~factor(.))) %>% # convert all characters to factors
  dplyr::select(-geometry, -abundance, -species) %>% 
  dplyr::select(cellID, ocean, longitude, latitude, season, everything()) %>% # arrange columns
  dplyr::mutate(season = "oct-dec") %>% 
  as.data.frame() #gbm.step doesn't work if it's a tibble...
