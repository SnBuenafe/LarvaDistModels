# DESCRIPTION: Assembling shortbill spearfish dataset

# Load preliminaries
source("00_SetupGrid.R")
source("00_Preliminaries.R")
species <- "SHOS"

# Define functions
# Function to restrict adult distribution predictor to just shortbill spearfish
restrict_predictor <- function(x){
  x %<>%
    dplyr::select(c(1:21, 31, 51)) %>%  # restrict the predictors
    rowwise() %>% 
    dplyr::mutate(adult = mean(c(Tetrapturus_angustirostris), na.rm = TRUE)) %>% 
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
sf <- combineFish(species = "shortbill-spearfish") %>% 
  fSpatPlan_Convert2PacificCentered(., cCRS = cCRS) %>% 
  sf::st_centroid() # transform into point data

seasons <- c("jan-mar", "apr-jun", "jul-sept", "oct-dec")
for(s in 1:length(seasons)) {
  gridded <- assembleGrid(grid, sf %>% dplyr::filter(season == seasons[s]))
  
  assign(paste("grid", species, seasons[s], sep = "_"), gridded)
}

# Load shortbill spearfish datasets
SHOS_ds1 <- read_csv(here::here(input_dir, "SHOS_historical_jan-mar.csv"), show_col_types = FALSE) %>% # January-March
  restrict_predictor()

SHOS_ds2 <- read_csv(here::here(input_dir, "SHOS_historical_apr-jun.csv"), show_col_types = FALSE) %>% # April-June
  restrict_predictor()

SHOS_ds3 <- read_csv(here::here(input_dir, "SHOS_historical_jul-sept.csv"), show_col_types = FALSE) %>% # July-September
  restrict_predictor()

SHOS_ds4 <- read_csv(here::here(input_dir, "SHOS_historical_oct-dec.csv"), show_col_types = FALSE) %>% # October-December
  restrict_predictor()

# Build model with known data only
SHOS_build <- dplyr::bind_rows(SHOS_ds1 %>% dplyr::filter(!is.na(abundance)),
                              SHOS_ds2 %>% dplyr::filter(!is.na(abundance)),
                              SHOS_ds3 %>% dplyr::filter(!is.na(abundance)),
                              SHOS_ds4 %>% dplyr::filter(!is.na(abundance))) %>% 
  organize_build()

# We divide the data into train (training and validation) and test
nrow(SHOS_build) * 0.9 # = 11063.7

set.seed(368899)
train <- slice_sample(SHOS_build, n = 11063, replace = FALSE) # 90% training set
test <- SHOS_build[!SHOS_build$row %in% train$row, ] # 10% testing set

# Prepare data frame for predictions
SHOS_predict_season1 <- organize_predict(SHOS_ds1) # January-March
SHOS_predict_season2 <- organize_predict(SHOS_ds2) # April-June
SHOS_predict_season3 <- organize_predict(SHOS_ds3) # July-September
SHOS_predict_season4 <- organize_predict(SHOS_ds4) # October-December
