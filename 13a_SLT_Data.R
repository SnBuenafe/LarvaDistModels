# DESCRIPTION: Assembling slender tuna dataset

# Load preliminaries
source("00_Preliminaries.R")
input_dir <- here::here("Output", "CSV")

# Define functions
# Function to restrict adult distribution predictor to just slender tuna
restrict_predictor <- function(x){
  x %<>%
    dplyr::select(c(1:21, 45, 51)) %>%  # restrict the predictors
    rowwise() %>% 
    dplyr::mutate(adult = mean(c(Allothunnus_fallai), na.rm = TRUE)) %>% 
    ungroup() %>% 
    dplyr::mutate(adult = ifelse(is.na(adult), yes = 0, no = adult)) # replace NAs of adult predictions to 0s
}

# Function to hatch areas where adults are unlikely to be found
restrict_adult <- function(x, y) {
  sf <- x %>% 
    dplyr::mutate(adult_cat = ifelse(adult >= 0.01, yes = 1, no = 0)) %>% # used a 0.01 threshold
    dplyr::select(-geometry) %>% 
    dplyr::left_join(., y) %>% 
    sf::st_as_sf(crs = cCRS)
}

# Create species sf object
sf <- combineFish(species = "slender-tuna") %>% 
  fSpatPlan_Convert2PacificCentered(., cCRS = cCRS) %>% 
  sf::st_centroid() # transform into point data

seasons <- c("jan-mar", "apr-jun", "jul-sept", "oct-dec")
for(s in 1:length(seasons)) {
  gridded <- assembleGrid(grid, sf %>% dplyr::filter(season == seasons[s]))
  
  assign(paste("grid", "SLT", seasons[s], sep = "_"), gridded)
}

# Load SLTtle tuna datasets
SLT_ds1 <- read_csv(here::here(input_dir, "SLT_historical_jan-mar.csv"), show_col_types = FALSE) %>% # January-March
  restrict_predictor()

SLT_ds2 <- read_csv(here::here(input_dir, "SLT_historical_apr-jun.csv"), show_col_types = FALSE) %>% # April-June
  restrict_predictor()

SLT_ds3 <- read_csv(here::here(input_dir, "SLT_historical_jul-sept.csv"), show_col_types = FALSE) %>% # July-September
  restrict_predictor()

SLT_ds4 <- read_csv(here::here(input_dir, "SLT_historical_oct-dec.csv"), show_col_types = FALSE) %>% # October-December
  restrict_predictor()

# Build model with known data only
SLT_build <- dplyr::bind_rows(SLT_ds1 %>% dplyr::filter(!is.na(abundance)),
                               SLT_ds2 %>% dplyr::filter(!is.na(abundance)),
                               SLT_ds3 %>% dplyr::filter(!is.na(abundance)),
                               SLT_ds4 %>% dplyr::filter(!is.na(abundance))) %>% 
  organize_build()

# We divide the data into train (training and validation) and test
nrow(SLT_build) * 0.9 # = 11063.7

set.seed(187585)
train <- slice_sample(SLT_build, n = 11063, replace = FALSE) # 90% training set
test <- SLT_build[!SLT_build$row %in% train$row, ] # 10% testing set

# Prepare data frame for predictions
SLT_predict_season1 <- organize_predict(SLT_ds1) # January-March
SLT_predict_season2 <- organize_predict(SLT_ds2) # April-June
SLT_predict_season3 <- organize_predict(SLT_ds3) # July-September
SLT_predict_season4 <- organize_predict(SLT_ds4) # October-December
