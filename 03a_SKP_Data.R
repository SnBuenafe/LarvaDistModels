# DESCRIPTION: Assembling skipjack tuna dataset

# Load preliminaries
source("00_Preliminaries.R")
input_dir <- here::here("Output", "CSV")

# Function to restrict adult distribution predictor to just skipjack tunas
restrict_predictor <- function(x){
  x %<>%
    dplyr::select(c(1:21, 23, 39)) %>%  # restrict the predictors
    dplyr::mutate(Katsuwonus_pelamis = ifelse(is.na(Katsuwonus_pelamis), yes = 0, no = Katsuwonus_pelamis)) # replace NAs of adult predictions to 0s
}

# Function to hatch areas where adults are unlikely to be found
restrict_adult <- function(x, y) {
  sf <- x %>% 
    dplyr::mutate(adult_cat = ifelse(Katsuwonus_pelamis >= 0.01, yes = 1, no = 0)) %>% 
    dplyr::select(-geometry) %>% 
    dplyr::left_join(., y) %>% 
    sf::st_as_sf(crs = cCRS)
}

# Create species sf object
sf <- combineFish(species = "skipjack-tuna") %>% 
  fSpatPlan_Convert2PacificCentered(., cCRS = cCRS) %>% 
  sf::st_centroid() # transform into point data

seasons <- c("jan-mar", "apr-jun", "jul-sept", "oct-dec")
for(s in 1:length(seasons)) {
  gridded <- assembleGrid(grid, sf %>% dplyr::filter(season == seasons[s]))
  
  assign(paste("grid", "SKP", seasons[s], sep = "_"), gridded)
}

# Load skipjack tuna datasets
SKP_ds1 <- read_csv("Output/CSV/SKP_historical_jan-mar.csv", show_col_types = FALSE) %>% # January-March
  restrict_predictor()

SKP_ds2 <- read_csv("Output/CSV/SKP_historical_apr-jun.csv", show_col_types = FALSE) %>% # April-June
  restrict_predictor()

SKP_ds3 <- read_csv("Output/CSV/SKP_historical_jul-sept.csv", show_col_types = FALSE) %>% # July-September
  restrict_predictor()

SKP_ds4 <- read_csv("Output/CSV/SKP_historical_oct-dec.csv", show_col_types = FALSE) %>% # October-December
  restrict_predictor()

# Build model with known data only
SKP_build <- dplyr::bind_rows(SKP_ds1 %>% dplyr::filter(!is.na(abundance)),
                              SKP_ds2 %>% dplyr::filter(!is.na(abundance)),
                              SKP_ds3 %>% dplyr::filter(!is.na(abundance)),
                              SKP_ds4 %>% dplyr::filter(!is.na(abundance))) %>% 
  organize_build()

# We divide the data into train (training and validation) and test
nrow(SKP_build) * 0.9 # = 11063.7

set.seed(2170)
train <- slice_sample(SKP_build, n = 11063, replace = FALSE) # 90% training set
test <- SKP_build[!SKP_build$row %in% train$row, ] # 10% testing set

# Prepare data frame for predictions
SKP_predict_season1 <- organize_predict(SKP_ds1) # January-March
SKP_predict_season2 <- organize_predict(SKP_ds2) # April-June
SKP_predict_season3 <- organize_predict(SKP_ds3) # July-September
SKP_predict_season4 <- organize_predict(SKP_ds4) # October-December
