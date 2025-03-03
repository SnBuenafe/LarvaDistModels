# DESCRIPTION: Assembling striped marlin dataset

# Load preliminaries
source(file.path("analyses", "02_preliminaries", "00_SetupGrid.R"))
source(file.path("analyses", "02_preliminaries", "00_Preliminaries.R"))
species <- "STRM"
figure_dir <- file.path(figure_dir, species)

# Function to restrict adult distribution predictor to just striped marlin
restrict_predictor <- function(x){
  x %<>%
    dplyr::select(c(1:21, 32:33, 51)) %>%  # restrict the predictors
    rowwise() %>% 
    dplyr::mutate(adult = mean(c(Kajikia_audax, Kajikia_albida), na.rm = TRUE)) %>% 
    ungroup() %>% 
    dplyr::mutate(adult = ifelse(is.na(adult), yes = 0, no = adult)) # replace NAs of adult predictions to 0s
}

# Create species sf object
sf <- combineFish(species = "striped-marlin") %>% 
  fSpatPlan_Convert2PacificCentered(., cCRS = cCRS) %>% 
  sf::st_centroid() # transform into point data

seasons <- c("jan-mar", "apr-jun", "jul-sep", "oct-dec")
for(s in 1:length(seasons)) {
  gridded <- assembleGrid(grid, sf %>% dplyr::filter(season == seasons[s]))
  
  assign(paste("grid", species, seasons[s], sep = "_"), gridded)
}

# Load striped marlin datasets
STRM_ds1 <- readRDS(file.path(input_dir, paste0(species, "_", seasons[1], ".rds"))) %>% 
  restrict_predictor()

STRM_ds2 <- readRDS(file.path(input_dir, paste0(species, "_", seasons[2], ".rds"))) %>% 
  restrict_predictor()

STRM_ds3 <- readRDS(file.path(input_dir, paste0(species, "_", seasons[3], ".rds"))) %>% 
  restrict_predictor()

STRM_ds4 <- readRDS(file.path(input_dir, paste0(species, "_", seasons[4], ".rds"))) %>% 
  restrict_predictor()

# Build model with known data only
STRM_build <- dplyr::bind_rows(STRM_ds1 %>% dplyr::filter(!is.na(abundance)),
                               STRM_ds2 %>% dplyr::filter(!is.na(abundance)),
                               STRM_ds3 %>% dplyr::filter(!is.na(abundance)),
                               STRM_ds4 %>% dplyr::filter(!is.na(abundance))) %>% 
  organize_build()

# We divide the data into train (training and validation) and test
nrow(STRM_build) * 0.8 # = 9834.4

set.seed(922439)
train <- slice_sample(STRM_build, n = 9834, replace = FALSE) # 80% training set
test <- STRM_build[!STRM_build$row %in% train$row, ] # 20% testing set

# Prepare data frame for predictions
STRM_predict_season1 <- organize_predict(STRM_ds1) # January-March
STRM_predict_season2 <- organize_predict(STRM_ds2) # April-June
STRM_predict_season3 <- organize_predict(STRM_ds3) # July-September
STRM_predict_season4 <- organize_predict(STRM_ds4) # October-December
