# DESCRIPTION: Assembling bluefin tuna dataset

# Load preliminaries
source("00_SetupGrid.R")
source("00_Preliminaries.R")
species <- "BFT"
figure_dir <- here::here(figure_dir, species)

# Function to restrict adult distribution predictor to just bluefin tunas
restrict_predictor <- function(x){
  x %<>%
    dplyr::select(c(1:21, 40, 51)) %>%  # restrict the predictors
    rowwise() %>% 
    dplyr::mutate(adult = mean(c(Thunnus_orientalis), na.rm = TRUE)) %>% # Just looking at Pacific tuna adult distributions
    ungroup() %>% 
    dplyr::mutate(adult = ifelse(is.na(adult), yes = 0, no = adult)) # replace NAs of adult predictions to 0s
}

# Create species sf object
sf <- combineFish(species = "a-bluefin-tuna") %>% 
  fSpatPlan_Convert2PacificCentered(., cCRS = cCRS) %>% 
  sf::st_centroid() # transform into point data

seasons <- c("jan-mar", "apr-jun", "jul-sept", "oct-dec")
for(s in 1:length(seasons)) {
  gridded <- assembleGrid(grid, sf %>% dplyr::filter(season == seasons[s]))
  
  assign(paste("grid", species, seasons[s], sep = "_"), gridded)
}

# Load striped marlin datasets
BFT_ds1 <- read_csv(here::here(input_dir, paste(species, "jan-mar.csv", sep = "_")), show_col_types = FALSE) %>% # January-March
  restrict_predictor()

BFT_ds2 <- read_csv(here::here(input_dir, paste(species, "apr-jun.csv", sep = "_")), show_col_types = FALSE)  %>% # April-June
  restrict_predictor()

BFT_ds3 <- read_csv(here::here(input_dir, paste(species, "jul-sept.csv", sep = "_")), show_col_types = FALSE)  %>% # July-September
  restrict_predictor()

BFT_ds4 <- read_csv(here::here(input_dir, paste(species, "oct-dec.csv", sep = "_")), show_col_types = FALSE)  %>% # October-December
  restrict_predictor()

# Build model with known data only
BFT_build <- dplyr::bind_rows(BFT_ds1 %>% dplyr::filter(!is.na(abundance)),
                              BFT_ds2 %>% dplyr::filter(!is.na(abundance)),
                              BFT_ds3 %>% dplyr::filter(!is.na(abundance)),
                              BFT_ds4 %>% dplyr::filter(!is.na(abundance))) %>% 
  organize_build()

# We divide the data into train (training and validation) and test
nrow(BFT_build) * 0.8 # = 9834.4

set.seed(3265)
train <- slice_sample(BFT_build, n = 9834, replace = FALSE) # 90% training set
test <- BFT_build[!BFT_build$row %in% train$row, ] # 10% testing set

# Prepare data frame for predictions
BFT_predict_season1 <- organize_predict(BFT_ds1) # January-March
BFT_predict_season2 <- organize_predict(BFT_ds2) # April-June
BFT_predict_season3 <- organize_predict(BFT_ds3) # July-September
BFT_predict_season4 <- organize_predict(BFT_ds4) # October-December
