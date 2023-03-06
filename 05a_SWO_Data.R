# DESCRIPTION: Assembling swordfish dataset

# Load preliminaries
source("00_Utils.R")

# Function to restrict adult distribution predictor to just swordfish
restrict_predictor <- function(x){
  x %<>%
    dplyr::select(c(1:19, 21:22, 30, 40)) %>%  # restrict the predictors
    dplyr::mutate(Xiphias_gladius = ifelse(is.na(Xiphias_gladius), yes = 0, no = Xiphias_gladius)) # replace NAs of adult predictions to 0s
}

# Function to hatch areas where adults are unlikely to be found
restrict_adult <- function(x, y) {
  sf <- x %>% 
    dplyr::mutate(adult_cat = ifelse(Xiphias_gladius >= 0.01, yes = 1, no = 0)) %>% 
    dplyr::select(-geometry) %>% 
    dplyr::left_join(., y) %>% 
    sf::st_as_sf(crs = moll_pacific)
}

sf <- combineFish(species = "swordfish") %>% 
  fSpatPlan_Convert2PacificCentered(., cCRS = moll_pacific) %>%
  sf::st_centroid() # transform into point data

seasons <- c("jan-mar", "apr-jun", "jul-sept", "oct-dec")
for(s in 1:length(seasons)) {
  gridded <- assembleGrid(grid, sf %>% dplyr::filter(season == seasons[s]))
  
  assign(paste("grid", "SWO", seasons[s], sep = "_"), gridded)
}

# Load swordfish datasets
SWO_ds1 <- read_csv("Output/CSV/SWO_historical_jan-mar.csv", show_col_types = FALSE) %>% # January-March
  restrict_predictor()
SWO_ds2 <- read_csv("Output/CSV/SWO_historical_apr-jun.csv", show_col_types = FALSE) %>% # April-June
  restrict_predictor()
SWO_ds3 <- read_csv("Output/CSV/SWO_historical_jul-sept.csv", show_col_types = FALSE) %>% # July-September
  restrict_predictor()
SWO_ds4 <- read_csv("Output/CSV/SWO_historical_oct-dec.csv", show_col_types = FALSE) %>% # October-December
  restrict_predictor()

# Build model with known data only
SWO_build <- dplyr::bind_rows(SWO_ds1 %>% dplyr::filter(!is.na(abundance)),
                              SWO_ds2 %>% dplyr::filter(!is.na(abundance)),
                              SWO_ds3 %>% dplyr::filter(!is.na(abundance)),
                              SWO_ds4 %>% dplyr::filter(!is.na(abundance))) %>% 
  organize_build()

# We divide the data into train (training and validation) and test
nrow(SWO_build) * 0.9 # = 11051.1

set.seed(471832519)
train <- slice_sample(SWO_build, n = 11051, replace = FALSE) # 90% training set
test <- SWO_build[!SWO_build$row %in% train$row, ] # 10% testing set

# Prepare data frame for predictions
SWO_predict_season1 <- organize_predict(SWO_ds1) # January-March
SWO_predict_season2 <- organize_predict(SWO_ds2) # April-June
SWO_predict_season3 <- organize_predict(SWO_ds3) # July-September
SWO_predict_season4 <- organize_predict(SWO_ds4) # October-December