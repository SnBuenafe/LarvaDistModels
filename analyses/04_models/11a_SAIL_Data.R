# DESCRIPTION: Assembling sailfish dataset

# Load preliminaries
source(file.path("analyses", "02_preliminaries", "00_SetupGrid.R"))
source(file.path("analyses", "02_preliminaries", "00_Preliminaries.R"))
species <- "SAIL"
figure_dir <- file.path(figure_dir, species)

# Function to restrict adult distribution predictor to just sailfish
restrict_predictor <- function(x){
  x %<>%
    dplyr::select(c(1:21, 34, 51)) %>%  # restrict the predictors
    dplyr::mutate(adult = ifelse(is.na(Istiophorus_platypterus), yes = 0, no = Istiophorus_platypterus)) # replace NAs of adult predictions to 0s
}

# Create species sf object
sf <- combineFish(species = "sailfish") %>% 
  fSpatPlan_Convert2PacificCentered(., cCRS = cCRS) %>% 
  sf::st_centroid() # transform into point data

seasons <- c("jan-mar", "apr-jun", "jul-sep", "oct-dec")
for(s in 1:length(seasons)) {
  gridded <- assembleGrid(grid, sf %>% dplyr::filter(season == seasons[s]))
  
  assign(paste("grid", species, seasons[s], sep = "_"), gridded)
}

# Load sailfish datasets
SAIL_ds1 <- readRDS(file.path(input_dir, paste0(species, "_", seasons[1], ".rds"))) %>% 
  restrict_predictor()

SAIL_ds2 <- readRDS(file.path(input_dir, paste0(species, "_", seasons[2], ".rds"))) %>% 
  restrict_predictor()

SAIL_ds3 <- readRDS(file.path(input_dir, paste0(species, "_", seasons[3], ".rds"))) %>% 
  restrict_predictor()

SAIL_ds4 <- readRDS(file.path(input_dir, paste0(species, "_", seasons[4], ".rds"))) %>% 
  restrict_predictor()

# Build model with known data only
SAIL_build <- dplyr::bind_rows(SAIL_ds1 %>% dplyr::filter(!is.na(abundance)),
                              SAIL_ds2 %>% dplyr::filter(!is.na(abundance)),
                              SAIL_ds3 %>% dplyr::filter(!is.na(abundance)),
                              SAIL_ds4 %>% dplyr::filter(!is.na(abundance))) %>% 
  organize_build()

# We divide the data into train (training and validation) and test
nrow(SAIL_build) * 0.8 # = 9834.4

set.seed(5492) 
train <- slice_sample(SAIL_build, n = 9834, replace = FALSE) # 80% training set
test <- SAIL_build[!SAIL_build$row %in% train$row, ] # 20% testing set

# Prepare data frame for predictions
SAIL_predict_season1 <- organize_predict(SAIL_ds1) # January-March
SAIL_predict_season2 <- organize_predict(SAIL_ds2) # April-June
SAIL_predict_season3 <- organize_predict(SAIL_ds3) # July-September
SAIL_predict_season4 <- organize_predict(SAIL_ds4) # October-December
