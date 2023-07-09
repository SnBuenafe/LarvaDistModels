# DESCRIPTION: Assembling slender tuna dataset

# Load preliminaries
source("00_SetupGrid.R")
source("00_Preliminaries.R")
species <- "SLT"
figure_dir <- here::here(figure_dir, species)

# Function to restrict adult distribution predictor to just slender tuna
restrict_predictor <- function(x){
  x %<>%
    dplyr::select(c(1:21, 45, 51)) %>%  # restrict the predictors
    rowwise() %>% 
    dplyr::mutate(adult = mean(c(Allothunnus_fallai), na.rm = TRUE)) %>% 
    ungroup() %>% 
    dplyr::mutate(adult = ifelse(is.na(adult), yes = 0, no = adult)) # replace NAs of adult predictions to 0s
}

# Create species sf object
sf <- combineFish(species = "slender-tuna") %>% 
  fSpatPlan_Convert2PacificCentered(., cCRS = cCRS) %>% 
  sf::st_centroid() # transform into point data

seasons <- c("jan-mar", "apr-jun", "jul-sept", "oct-dec")
for(s in 1:length(seasons)) {
  gridded <- assembleGrid(grid, sf %>% dplyr::filter(season == seasons[s]))
  
  assign(paste("grid", species, seasons[s], sep = "_"), gridded)
}

# Load SLTtle tuna datasets
SLT_ds1 <- read_csv(here::here(input_dir, paste(species, "jan-mar.csv", sep = "_")), show_col_types = FALSE) %>% # January-March
  restrict_predictor()

SLT_ds2 <- read_csv(here::here(input_dir, paste(species, "apr-jun.csv", sep = "_")), show_col_types = FALSE) %>% # April-June
  restrict_predictor()

SLT_ds3 <- read_csv(here::here(input_dir, paste(species, "jul-sept.csv", sep = "_")), show_col_types = FALSE) %>% # July-September
  restrict_predictor()

SLT_ds4 <- read_csv(here::here(input_dir, paste(species, "oct-dec.csv", sep = "_")), show_col_types = FALSE) %>% # October-December
  restrict_predictor()

# Build model with known data only
SLT_build <- dplyr::bind_rows(SLT_ds1 %>% dplyr::filter(!is.na(abundance)),
                               SLT_ds2 %>% dplyr::filter(!is.na(abundance)),
                               SLT_ds3 %>% dplyr::filter(!is.na(abundance)),
                               SLT_ds4 %>% dplyr::filter(!is.na(abundance))) %>% 
  organize_build()

# We divide the data into train (training and validation) and test
nrow(SLT_build) * 0.8 # = 9834.4

set.seed(187585)
train <- slice_sample(SLT_build, n = 9834, replace = FALSE) # 80% training set
test <- SLT_build[!SLT_build$row %in% train$row, ] # 20% testing set

# Prepare data frame for predictions
SLT_predict_season1 <- organize_predict(SLT_ds1) # January-March
SLT_predict_season2 <- organize_predict(SLT_ds2) # April-June
SLT_predict_season3 <- organize_predict(SLT_ds3) # July-September
SLT_predict_season4 <- organize_predict(SLT_ds4) # October-December
