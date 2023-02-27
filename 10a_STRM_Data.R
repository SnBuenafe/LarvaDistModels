# DESCRIPTION: Assembling striped marlin dataset

# Load preliminaries
source("00_Utils.R")

sf <- combineFish(species = "striped-marlin") %>% 
  sf::st_transform(crs = moll) %>% 
  sf::st_centroid() # transform into point data

seasons <- c("jan-mar", "apr-jun", "jul-sept", "oct-dec")
for(s in 1:length(seasons)) {
  gridded <- assembleGrid(grid, sf %>% dplyr::filter(season == seasons[s]))
  
  assign(paste("grid", "STRM", seasons[s], sep = "_"), gridded)
}

# Load striped marlin datasets
STRM_ds1 <- read_csv("Output/CSV/STRM_historical_jan-mar.csv", show_col_types = FALSE)
STRM_ds2 <- read_csv("Output/CSV/STRM_historical_apr-jun.csv", show_col_types = FALSE)
STRM_ds3 <- read_csv("Output/CSV/STRM_historical_jul-sept.csv", show_col_types = FALSE)
STRM_ds4 <- read_csv("Output/CSV/STRM_historical_oct-dec.csv", show_col_types = FALSE)

# Build model with known data only
STRM_build <- dplyr::bind_rows(STRM_ds1 %>% dplyr::filter(!is.na(abundance)),
                              STRM_ds2 %>% dplyr::filter(!is.na(abundance)),
                              STRM_ds3 %>% dplyr::filter(!is.na(abundance)),
                              STRM_ds4 %>% dplyr::filter(!is.na(abundance))) %>% 
  dplyr::mutate(across(where(is.character), ~factor(.))) %>% # convert all characters to factors
  dplyr::mutate(abundance_presence = case_when(abundance > 0 ~ 1,
                                               abundance == 0 ~ 0), 
                row = row_number()) %>%  # mutate the abundance data into 1s and 0s
  dplyr::select(-geometry) %>% 
  dplyr::select(row, cellID, species, abundance, abundance_presence, ocean, longitude, latitude, season, everything()) %>% # arrange columns
  as.data.frame() #gbm.step doesn't work if it's a tibble...

# We divide the data into train (training and validation) and test
nrow(STRM_build) * 0.9 # = 11890

set.seed(3265) # rerun this
train <- slice_sample(STRM_build, n = 11890, replace = FALSE) # 90% training set
saveRDS(train, "Output/Train/STRM_train.csv")
test <- STRM_build[!STRM_build$row %in% train$row, ] # 10% testing set
saveRDS(test, "Output/Test/STRM_test.csv")

# Data.frame for predictions
# January-March predictions
STRM_predict_season1 <- STRM_ds1 %>% dplyr::filter(is.na(abundance)) %>% 
  dplyr::mutate(across(where(is.character), ~factor(.))) %>% # convert all characters to factors
  dplyr::select(-geometry, -abundance, -species) %>% 
  dplyr::select(cellID, ocean, longitude, latitude, season, everything()) %>% # arrange columns
  dplyr::mutate(season = "jan-mar") %>% 
  as.data.frame() #gbm.step doesn't work if it's a tibble...

# April-June
STRM_predict_season2 <- STRM_ds2 %>% dplyr::filter(is.na(abundance)) %>% 
  dplyr::mutate(across(where(is.character), ~factor(.))) %>% # convert all characters to factors
  dplyr::select(-geometry, -abundance, -species) %>% 
  dplyr::select(cellID, ocean, longitude, latitude, season, everything()) %>% # arrange columns
  dplyr::mutate(season = "apr-jun") %>% 
  as.data.frame() #gbm.step doesn't work if it's a tibble...

# July-September
STRM_predict_season3 <- STRM_ds3 %>% dplyr::filter(is.na(abundance)) %>% 
  dplyr::mutate(across(where(is.character), ~factor(.))) %>% # convert all characters to factors
  dplyr::select(-geometry, -abundance, -species) %>% 
  dplyr::select(cellID, ocean, longitude, latitude, season, everything()) %>% # arrange columns
  dplyr::mutate(season = "jul-sept") %>% 
  as.data.frame() #gbm.step doesn't work if it's a tibble...

# October-December
STRM_predict_season4 <- STRM_ds4 %>% dplyr::filter(is.na(abundance)) %>% 
  dplyr::mutate(across(where(is.character), ~factor(.))) %>% # convert all characters to factors
  dplyr::select(-geometry, -abundance, -species) %>% 
  dplyr::select(cellID, ocean, longitude, latitude, season, everything()) %>% # arrange columns
  dplyr::mutate(season = "oct-dec") %>% 
  as.data.frame() #gbm.step doesn't work if it's a tibble...
