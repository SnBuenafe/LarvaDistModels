# DESCRIPTION: Assembling yellowfin tuna dataset

# Load preliminaries
source("00_Preliminaries.R")
input_dir <- here::here("Output", "CSV")

# Define functions
# Function to restrict adult distribution predictor to just yellowfin tunas
restrict_predictor <- function(x){
  x %<>%
    dplyr::select(c(1:22, 39)) %>%  # restrict the predictors
    dplyr::mutate(Thunnus_albacares = ifelse(is.na(Thunnus_albacares), yes = 0, no = Thunnus_albacares)) # replace NAs of adult predictions to 0s
}

# Function to hatch areas where adults are unlikely to be found
restrict_adult <- function(x, y) {
  sf <- x %>% 
    dplyr::mutate(adult_cat = ifelse(Thunnus_albacares >= 0.01, yes = 1, no = 0)) %>% 
    dplyr::select(-geometry) %>% 
    dplyr::left_join(., y) %>% 
    sf::st_as_sf(crs = cCRS)
}

# Create species sf object
sf <- combineFish(species = "yellowfin-tuna") %>% 
  fSpatPlan_Convert2PacificCentered(., cCRS = cCRS) %>% 
  sf::st_centroid() # transform into point data

seasons <- c("jan-mar", "apr-jun", "jul-sept", "oct-dec")
for(s in 1:length(seasons)) {
  gridded <- assembleGrid(grid, sf %>% dplyr::filter(season == seasons[s])) 
  
  assign(paste("grid", "YFT", seasons[s], sep = "_"), gridded)
}

# Load yellowfin tuna datasets
YFT_ds1 <- read_csv(here::here(input_dir, "YFT_historical_jan-mar.csv"), show_col_types = FALSE) %>% # January-March
  restrict_predictor()

YFT_ds2 <- read_csv(here::here(input_dir, "YFT_historical_apr-jun.csv"), show_col_types = FALSE)  %>% # April-June
  restrict_predictor()
  
YFT_ds3 <- read_csv(here::here(input_dir, "YFT_historical_jul-sept.csv"), show_col_types = FALSE)  %>% # July-September
  restrict_predictor()

YFT_ds4 <- read_csv(here::here(input_dir, "YFT_historical_oct-dec.csv"), show_col_types = FALSE)  %>% # October-December
  restrict_predictor()

# Build model with known data only
YFT_build <- dplyr::bind_rows(YFT_ds1 %>% dplyr::filter(!is.na(abundance)),
                              YFT_ds2 %>% dplyr::filter(!is.na(abundance)),
                              YFT_ds3 %>% dplyr::filter(!is.na(abundance)),
                              YFT_ds4 %>% dplyr::filter(!is.na(abundance))) %>% 
  organize_build()

# We divide the data into train (training and validation) and test
nrow(YFT_build) * 0.9 # = 11063.7

set.seed(1234)
train <- slice_sample(YFT_build, n = 11063, replace = FALSE) # 90% training set
test <- YFT_build[!YFT_build$row %in% train$row, ] # 10% testing set

# Prepare data frame for predictions
YFT_predict_season1 <- organize_predict(YFT_ds1) # January-March
YFT_predict_season2 <- organize_predict(YFT_ds2) # April -June
YFT_predict_season3 <- organize_predict(YFT_ds3) # July-September
YFT_predict_season4 <- organize_predict(YFT_ds4) # October-December
  