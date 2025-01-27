# DESCRIPTION: Assembling bigeye tuna dataset

# Load preliminaries
source(file.path("analyses", "02_preliminaries", "00_SetupGrid.R"))
source(file.path("analyses", "02_preliminaries", "00_Preliminaries.R"))
species <- "BET"
figure_dir <- file.path(figure_dir, species)

# Define functions
# Function to restrict adult distribution predictor to just bigeye tunas
restrict_predictor <- function(x){
  x %<>%
    dplyr::select(c(1:21, 25:26, 51)) %>%  # restrict the predictors
    rowwise() %>% 
    dplyr::mutate(adult = mean(c(Thunnus_obesus, Thunnus_atlanticus), na.rm = TRUE)) %>% 
    ungroup() %>% 
    dplyr::mutate(adult = ifelse(is.na(adult), yes = 0, no = adult)) # replace NAs of adult predictions to 0s
}

# Create species sf object
sf <- combineFish(species = "bigeye-tuna") %>% 
  fSpatPlan_Convert2PacificCentered(., cCRS = cCRS) %>% 
  sf::st_centroid() # transform into point data

seasons <- c("jan-mar", "apr-jun", "jul-sep", "oct-dec")
for(s in 1:length(seasons)) {
  gridded <- assembleGrid(grid, sf %>% dplyr::filter(season == seasons[s]))
  
  assign(paste("grid", species, seasons[s], sep = "_"), gridded)
}

# Load bigeye tuna datasets
BET_ds1 <- readRDS(file.path(input_dir, paste0(species, "_", seasons[1], ".rds"))) %>% 
  restrict_predictor()

BET_ds2 <- readRDS(file.path(input_dir, paste0(species, "_", seasons[2], ".rds"))) %>% 
  restrict_predictor()

BET_ds3 <- readRDS(file.path(input_dir, paste0(species, "_", seasons[3], ".rds"))) %>% 
  restrict_predictor()

BET_ds4 <- readRDS(file.path(input_dir, paste0(species, "_", seasons[4], ".rds"))) %>% 
  restrict_predictor()

# Build model with known data only
BET_build <- dplyr::bind_rows(BET_ds1 %>% dplyr::filter(!is.na(abundance)),
                              BET_ds2 %>% dplyr::filter(!is.na(abundance)),
                              BET_ds3 %>% dplyr::filter(!is.na(abundance)),
                              BET_ds4 %>% dplyr::filter(!is.na(abundance))) %>% 
  organize_build()

# We divide the data into train (training and validation) and test
nrow(BET_build) * 0.8 # = 9834.4

set.seed(2221096)
train <- slice_sample(BET_build, n = 9834, replace = FALSE) # 80% training set
test <- BET_build[!BET_build$row %in% train$row, ] # 20% testing set

# Prepare data frame for predictions
BET_predict_season1 <- organize_predict(BET_ds1) # January-March
BET_predict_season2 <- organize_predict(BET_ds2) # April-June
BET_predict_season3 <- organize_predict(BET_ds3) # July-September
BET_predict_season4 <- organize_predict(BET_ds4) # October-December
