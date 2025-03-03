# DESCRIPTION: Assembling skipjack tuna dataset

# Load preliminaries
source(file.path("analyses", "02_preliminaries", "00_SetupGrid.R"))
source(file.path("analyses", "02_preliminaries", "00_Preliminaries.R"))

species <- "SKP"
figure_dir <- file.path(figure_dir, species)

# Function to restrict adult distribution predictor to just skipjack tunas
restrict_predictor <- function(x){
  x %<>%
    dplyr::select(c(1:21, 23, 51)) %>%  # restrict the predictors
    dplyr::mutate(adult = ifelse(is.na(Katsuwonus_pelamis), yes = 0, no = Katsuwonus_pelamis)) # replace NAs of adult predictions to 0s
}

# Create species sf object
sf <- combineFish(species = "skipjack-tuna") %>% 
  fSpatPlan_Convert2PacificCentered(., cCRS = cCRS) %>% 
  sf::st_centroid() # transform into point data

seasons <- c("jan-mar", "apr-jun", "jul-sep", "oct-dec")
for(s in 1:length(seasons)) {
  gridded <- assembleGrid(grid, sf %>% dplyr::filter(season == seasons[s]))
  
  assign(paste("grid", species, seasons[s], sep = "_"), gridded)
}

# Load skipjack tuna datasets
SKP_ds1 <- readRDS(file.path(input_dir, paste0(species, "_", seasons[1], ".rds"))) %>% 
  restrict_predictor()

SKP_ds2 <- readRDS(file.path(input_dir, paste0(species, "_", seasons[2], ".rds"))) %>% 
  restrict_predictor()

SKP_ds3 <- readRDS(file.path(input_dir, paste0(species, "_", seasons[3], ".rds"))) %>% 
  restrict_predictor()

SKP_ds4 <- readRDS(file.path(input_dir, paste0(species, "_", seasons[4], ".rds"))) %>% 
  restrict_predictor()

# Build model with known data only
SKP_build <- dplyr::bind_rows(SKP_ds1 %>% dplyr::filter(!is.na(abundance)),
                              SKP_ds2 %>% dplyr::filter(!is.na(abundance)),
                              SKP_ds3 %>% dplyr::filter(!is.na(abundance)),
                              SKP_ds4 %>% dplyr::filter(!is.na(abundance))) %>% 
  organize_build()

# We divide the data into train (training and validation) and test
nrow(SKP_build) * 0.8 # = 9834.4

set.seed(2170)
train <- slice_sample(SKP_build, n = 9834, replace = FALSE) # 80% training set
test <- SKP_build[!SKP_build$row %in% train$row, ] # 20% testing set

# Prepare data frame for predictions
SKP_predict_season1 <- organize_predict(SKP_ds1) # January-March
SKP_predict_season2 <- organize_predict(SKP_ds2) # April-June
SKP_predict_season3 <- organize_predict(SKP_ds3) # July-September
SKP_predict_season4 <- organize_predict(SKP_ds4) # October-December
