# DESCRIPTION: Assembling southern bluefin tuna dataset

# Load preliminaries
source(file.path("analyses", "02_preliminaries", "00_SetupGrid.R"))
source(file.path("analyses", "02_preliminaries", "00_Preliminaries.R"))
species <- "SBFT"
figure_dir <- file.path(figure_dir, species)

# Define functions
# Function to restrict adult distribution predictor to just southern bluefin tuna
restrict_predictor <- function(x){
  x %<>%
    dplyr::select(c(1:21, 44, 51)) %>%  # restrict the predictors
    rowwise() %>% 
    dplyr::mutate(adult = mean(c(Thunnus_maccoyii), na.rm = TRUE)) %>% 
    ungroup() %>% 
    dplyr::mutate(adult = ifelse(is.na(adult), yes = 0, no = adult)) # replace NAs of adult predictions to 0s
}

# Create species sf object
sf <- combineFish(species = "southern-bluefin-tuna") %>% 
  fSpatPlan_Convert2PacificCentered(., cCRS = cCRS) %>% 
  sf::st_centroid() # transform into point data

seasons <- c("jan-mar", "apr-jun", "jul-sep", "oct-dec")
for(s in 1:length(seasons)) {
  gridded <- assembleGrid(grid, sf %>% dplyr::filter(season == seasons[s]))
  
  assign(paste("grid", species, seasons[s], sep = "_"), gridded)
}

# Load southern bluefin tuna datasets
SBFT_ds1 <- readRDS(file.path(input_dir, paste0(species, "_", seasons[1], ".rds"))) %>% 
  restrict_predictor()

SBFT_ds2 <- readRDS(file.path(input_dir, paste0(species, "_", seasons[2], ".rds"))) %>% 
  restrict_predictor()

SBFT_ds3 <- readRDS(file.path(input_dir, paste0(species, "_", seasons[3], ".rds"))) %>% 
  restrict_predictor()

SBFT_ds4 <- readRDS(file.path(input_dir, paste0(species, "_", seasons[4], ".rds"))) %>% 
  restrict_predictor()

# Build model with known data only
SBFT_build <- dplyr::bind_rows(SBFT_ds1 %>% dplyr::filter(!is.na(abundance)),
                              SBFT_ds2 %>% dplyr::filter(!is.na(abundance)),
                              SBFT_ds3 %>% dplyr::filter(!is.na(abundance)),
                              SBFT_ds4 %>% dplyr::filter(!is.na(abundance))) %>% 
  organize_build()

# We divide the data into train (training and validation) and test
nrow(SBFT_build) * 0.8 # = 9834.4

set.seed(8163519)
train <- slice_sample(SBFT_build, n = 9834, replace = FALSE) # 80% training set
test <- SBFT_build[!SBFT_build$row %in% train$row, ] # 20% testing set

# Prepare data frame for predictions
SBFT_predict_season1 <- organize_predict(SBFT_ds1) # January-March
SBFT_predict_season2 <- organize_predict(SBFT_ds2) # April-June
SBFT_predict_season3 <- organize_predict(SBFT_ds3) # July-September
SBFT_predict_season4 <- organize_predict(SBFT_ds4) # October-December
