# DESCRIPTION: Assembling frigate tuna dataset

# Load preliminaries
source("00_Preliminaries.R")
source("00_SetupGrid.R")
species <- "FRI"

# Function to restrict adult distribution predictor to just frigate tuna
restrict_predictor <- function(x){
  x %<>%
    dplyr::select(c(1:21, 27:28, 51)) %>%  # restrict the predictors
    rowwise() %>% 
    dplyr::mutate(adult = mean(c(Auxis_rochei, Auxis_thazard), na.rm = TRUE)) %>% 
    ungroup() %>% 
    dplyr::mutate(adult = ifelse(is.na(adult), yes = 0, no = adult)) # replace NAs of adult predictions to 0s
}

# Function to hatch areas where adults are unlikely to be found
restrict_adult <- function(x, y) {
  sf <- x %>% 
    dplyr::mutate(adult_cat = ifelse(adult >= 0.01, yes = 1, no = 0)) %>% 
    dplyr::select(-geometry) %>% 
    dplyr::left_join(., y) %>% 
    sf::st_as_sf(crs = cCRS)
}

# Create species sf object
sf <- combineFish(species = "frigate-tuna") %>% 
  fSpatPlan_Convert2PacificCentered(., cCRS = cCRS) %>% 
  sf::st_centroid() # transform into point data

seasons <- c("jan-mar", "apr-jun", "jul-sept", "oct-dec")
for(s in 1:length(seasons)) {
  gridded <- assembleGrid(grid, sf %>% dplyr::filter(season == seasons[s]))
  
  assign(paste("grid", species, seasons[s], sep = "_"), gridded)
}

# Load frigate tuna datasets
FRI_ds1 <- read_csv(here::here(input_dir, "FRI_historical_jan-mar.csv"), show_col_types = FALSE) %>% # January-March
  restrict_predictor()

FRI_ds2 <- read_csv(here::here(input_dir, "FRI_historical_apr-jun.csv"), show_col_types = FALSE)  %>% # April-June
  restrict_predictor()

FRI_ds3 <- read_csv(here::here(input_dir, "FRI_historical_jul-sept.csv"), show_col_types = FALSE)  %>% # July-September
  restrict_predictor()

FRI_ds4 <- read_csv(here::here(input_dir, "FRI_historical_oct-dec.csv"), show_col_types = FALSE)  %>% # October-December
  restrict_predictor()

# Build model with known data only
FRI_build <- dplyr::bind_rows(FRI_ds1 %>% dplyr::filter(!is.na(abundance)),
                              FRI_ds2 %>% dplyr::filter(!is.na(abundance)),
                              FRI_ds3 %>% dplyr::filter(!is.na(abundance)),
                              FRI_ds4 %>% dplyr::filter(!is.na(abundance))) %>% 
  organize_build()

# We divide the data into train (training and validation) and test
nrow(FRI_build) * 0.9 # = 11063.7

set.seed(7500426)
train <- slice_sample(FRI_build, n = 11063, replace = FALSE) # 90% training set
test <- FRI_build[!FRI_build$row %in% train$row, ] # 10% testing set

# Prepare data frame for predictions
FRI_predict_season1 <- organize_predict(FRI_ds1) # January-March
FRI_predict_season2 <- organize_predict(FRI_ds2) # April-June
FRI_predict_season3 <- organize_predict(FRI_ds3) # July-September
FRI_predict_season4 <- organize_predict(FRI_ds4) # October-December
