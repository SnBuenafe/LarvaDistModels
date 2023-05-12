# DESCRIPTION: Assembling sailfish dataset

# Load preliminaries
source("00_SetupGrid.R")
source("00_Preliminaries.R")
species <- "SAIL"

# Function to restrict adult distribution predictor to just sailfish
restrict_predictor <- function(x){
  x %<>%
    dplyr::select(c(1:21, 34, 51)) %>%  # restrict the predictors
    dplyr::mutate(Istiophorus_platypterus = ifelse(is.na(Istiophorus_platypterus), yes = 0, no = Istiophorus_platypterus)) # replace NAs of adult predictions to 0s
}

# Function to hatch areas where adults are unlikely to be found
restrict_adult <- function(x, y) {
  sf <- x %>% 
    dplyr::mutate(adult_cat = ifelse(Istiophorus_platypterus >= 0.01, yes = 1, no = 0)) %>% 
    dplyr::select(-geometry) %>% 
    dplyr::left_join(., y) %>% 
    sf::st_as_sf(crs = cCRS)
}

# Create species sf object
sf <- combineFish(species = "sailfish") %>% 
  fSpatPlan_Convert2PacificCentered(., cCRS = cCRS) %>% 
  sf::st_centroid() # transform into point data

seasons <- c("jan-mar", "apr-jun", "jul-sept", "oct-dec")
for(s in 1:length(seasons)) {
  gridded <- assembleGrid(grid, sf %>% dplyr::filter(season == seasons[s]))
  
  assign(paste("grid", species, seasons[s], sep = "_"), gridded)
}

# Load sailfish datasets
SAIL_ds1 <- read_csv(here::here(input_dir, "SAIL_historical_jan-mar.csv"), show_col_types = FALSE) %>% # January-March
  restrict_predictor()

SAIL_ds2 <- read_csv(here::here(input_dir, "SAIL_historical_apr-jun.csv"), show_col_types = FALSE)  %>% # April-June
  restrict_predictor()

SAIL_ds3 <- read_csv(here::here(input_dir, "SAIL_historical_jul-sept.csv"), show_col_types = FALSE)  %>% # July-September
  restrict_predictor()

SAIL_ds4 <- read_csv(here::here(input_dir, "SAIL_historical_oct-dec.csv"), show_col_types = FALSE)  %>% # October-December
  restrict_predictor()

# Build model with known data only
SAIL_build <- dplyr::bind_rows(SAIL_ds1 %>% dplyr::filter(!is.na(abundance)),
                              SAIL_ds2 %>% dplyr::filter(!is.na(abundance)),
                              SAIL_ds3 %>% dplyr::filter(!is.na(abundance)),
                              SAIL_ds4 %>% dplyr::filter(!is.na(abundance))) %>% 
  organize_build()

# We divide the data into train (training and validation) and test
nrow(SAIL_build) * 0.9 # = 11063.7

set.seed(5492) 
train <- slice_sample(SAIL_build, n = 11063, replace = FALSE) # 90% training set
test <- SAIL_build[!SAIL_build$row %in% train$row, ] # 10% testing set

# Prepare data frame for predictions
SAIL_predict_season1 <- organize_predict(SAIL_ds1) # January-March
SAIL_predict_season2 <- organize_predict(SAIL_ds2) # April-June
SAIL_predict_season3 <- organize_predict(SAIL_ds3) # July-September
SAIL_predict_season4 <- organize_predict(SAIL_ds4) # October-December
