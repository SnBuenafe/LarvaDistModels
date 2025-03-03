# DESCRIPTION: Assembling longfin escolar dataset

# Load preliminaries
source(file.path("analyses", "02_preliminaries", "00_SetupGrid.R"))
source(file.path("analyses", "02_preliminaries", "00_Preliminaries.R"))
species <- "LESC"
figure_dir <- file.path(figure_dir, species)

# Define functions
# Function to restrict adult distribution predictor to just longfin escolar
restrict_predictor <- function(x){
  x %<>%
    dplyr::select(c(1:21, 38, 51)) %>%  # restrict the predictors
    rowwise() %>% 
    dplyr::mutate(adult = mean(c(Scombrolabrax_heterolepis), na.rm = TRUE)) %>% 
    ungroup() %>% 
    dplyr::mutate(adult = ifelse(is.na(adult), yes = 0, no = adult)) # replace NAs of adult predictions to 0s
}

# Create species sf object
sf <- combineFish(species = "longfin-escolar") %>% 
  fSpatPlan_Convert2PacificCentered(., cCRS = cCRS) %>% 
  sf::st_centroid() # transform into point data

seasons <- c("jan-mar", "apr-jun", "jul-sep", "oct-dec")
for(s in 1:length(seasons)) {
  gridded <- assembleGrid(grid, sf %>% dplyr::filter(season == seasons[s]))
  
  assign(paste("grid", species, seasons[s], sep = "_"), gridded)
}

# Load longfin escolar datasets
LESC_ds1 <- readRDS(file.path(input_dir, paste0(species, "_", seasons[1], ".rds"))) %>% 
  restrict_predictor()

LESC_ds2 <- readRDS(file.path(input_dir, paste0(species, "_", seasons[2], ".rds"))) %>% 
  restrict_predictor()

LESC_ds3 <- readRDS(file.path(input_dir, paste0(species, "_", seasons[3], ".rds"))) %>% 
  restrict_predictor()

LESC_ds4 <- readRDS(file.path(input_dir, paste0(species, "_", seasons[4], ".rds"))) %>% 
  restrict_predictor()

# Build model with known data only
LESC_build <- dplyr::bind_rows(LESC_ds1 %>% dplyr::filter(!is.na(abundance)),
                               LESC_ds2 %>% dplyr::filter(!is.na(abundance)),
                               LESC_ds3 %>% dplyr::filter(!is.na(abundance)),
                               LESC_ds4 %>% dplyr::filter(!is.na(abundance))) %>% 
  organize_build()

# We divide the data into train (training and validation) and test
nrow(LESC_build) * 0.8 # = 9834.4

set.seed(661868)
train <- slice_sample(LESC_build, n = 9834, replace = FALSE) # 90% training set
test <- LESC_build[!LESC_build$row %in% train$row, ] # 10% testing set

# Prepare data frame for predictions
LESC_predict_season1 <- organize_predict(LESC_ds1) # January-March
LESC_predict_season2 <- organize_predict(LESC_ds2) # April-June
LESC_predict_season3 <- organize_predict(LESC_ds3) # July-September
LESC_predict_season4 <- organize_predict(LESC_ds4) # October-December
