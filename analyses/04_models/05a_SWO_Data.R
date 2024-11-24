# DESCRIPTION: Assembling swordfish dataset

# Load preliminaries
source(file.path("analyses", "02_preliminaries", "00_SetupGrid.R"))
source(file.path("analyses", "02_preliminaries", "00_Preliminaries.R"))
species <- "SWO"
figure_dir <- here::here(figure_dir, species)

# Define functions
# Function to restrict adult distribution predictor to just swordfish
restrict_predictor <- function(x){
  x %<>%
    dplyr::select(c(1:21, 29, 51)) %>%  # restrict the predictors
    dplyr::mutate(adult = ifelse(is.na(Xiphias_gladius), yes = 0, no = Xiphias_gladius)) # replace NAs of adult predictions to 0s
}

# Create species sf object
sf <- combineFish(species = "swordfish") %>% 
  fSpatPlan_Convert2PacificCentered(., cCRS = cCRS) %>%
  sf::st_centroid() # transform into point data

seasons <- c("jan-mar", "apr-jun", "jul-sep", "oct-dec")
for(s in 1:length(seasons)) {
  gridded <- assembleGrid(grid, sf %>% dplyr::filter(season == seasons[s]))
  
  assign(paste("grid", species, seasons[s], sep = "_"), gridded)
}

# Load swordfish datasets
SWO_ds1 <- readRDS(here::here(input_dir, paste0(species, "_", seasons[1], ".rds"))) %>% 
  restrict_predictor()

SWO_ds2 <- readRDS(here::here(input_dir, paste0(species, "_", seasons[2], ".rds"))) %>% 
  restrict_predictor()

SWO_ds3 <- readRDS(here::here(input_dir, paste0(species, "_", seasons[3], ".rds"))) %>% 
  restrict_predictor()

SWO_ds4 <- readRDS(here::here(input_dir, paste0(species, "_", seasons[4], ".rds"))) %>% 
  restrict_predictor()

# Build model with known data only
SWO_build <- dplyr::bind_rows(SWO_ds1 %>% dplyr::filter(!is.na(abundance)),
                              SWO_ds2 %>% dplyr::filter(!is.na(abundance)),
                              SWO_ds3 %>% dplyr::filter(!is.na(abundance)),
                              SWO_ds4 %>% dplyr::filter(!is.na(abundance))) %>% 
  organize_build()

# We divide the data into train (training and validation) and test
nrow(SWO_build) * 0.8 # = 9834.4

set.seed(471832519)
train <- slice_sample(SWO_build, n = 9834, replace = FALSE) # 80% training set
test <- SWO_build[!SWO_build$row %in% train$row, ] # 20% testing set

# Prepare data frame for predictions
SWO_predict_season1 <- organize_predict(SWO_ds1) # January-March
SWO_predict_season2 <- organize_predict(SWO_ds2) # April-June
SWO_predict_season3 <- organize_predict(SWO_ds3) # July-September
SWO_predict_season4 <- organize_predict(SWO_ds4) # October-December
