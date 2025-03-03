# DESCRIPTION: Assembling shortbill spearfish dataset

# Load preliminaries
source(file.path("analyses", "02_preliminaries", "00_SetupGrid.R"))
source(file.path("analyses", "02_preliminaries", "00_Preliminaries.R"))
species <- "SHOS"
figure_dir <- file.path(figure_dir, species)

# Function to restrict adult distribution predictor to just shortbill spearfish
restrict_predictor <- function(x){
  x %<>%
    dplyr::select(c(1:21, 31, 51)) %>%  # restrict the predictors
    rowwise() %>% 
    dplyr::mutate(adult = mean(c(Tetrapturus_angustirostris), na.rm = TRUE)) %>% 
    ungroup() %>% 
    dplyr::mutate(adult = ifelse(is.na(adult), yes = 0, no = adult)) # replace NAs of adult predictions to 0s
}

# Create species sf object
sf <- combineFish(species = "shortbill-spearfish") %>% 
  fSpatPlan_Convert2PacificCentered(., cCRS = cCRS) %>% 
  sf::st_centroid() # transform into point data

seasons <- c("jan-mar", "apr-jun", "jul-sep", "oct-dec")
for(s in 1:length(seasons)) {
  gridded <- assembleGrid(grid, sf %>% dplyr::filter(season == seasons[s]))
  
  assign(paste("grid", species, seasons[s], sep = "_"), gridded)
}

# Load shortbill spearfish datasets
SHOS_ds1 <- readRDS(file.path(input_dir, paste0(species, "_", seasons[1], ".rds"))) %>% 
  restrict_predictor()

SHOS_ds2 <- readRDS(file.path(input_dir, paste0(species, "_", seasons[2], ".rds"))) %>% 
  restrict_predictor()

SHOS_ds3 <- readRDS(file.path(input_dir, paste0(species, "_", seasons[3], ".rds"))) %>% 
  restrict_predictor()

SHOS_ds4 <- readRDS(file.path(input_dir, paste0(species, "_", seasons[4], ".rds"))) %>% 
  restrict_predictor()

# Build model with known data only
SHOS_build <- dplyr::bind_rows(SHOS_ds1 %>% dplyr::filter(!is.na(abundance)),
                              SHOS_ds2 %>% dplyr::filter(!is.na(abundance)),
                              SHOS_ds3 %>% dplyr::filter(!is.na(abundance)),
                              SHOS_ds4 %>% dplyr::filter(!is.na(abundance))) %>% 
  organize_build()

# We divide the data into train (training and validation) and test
nrow(SHOS_build) * 0.8 # = 9834.4

set.seed(368899)
train <- slice_sample(SHOS_build, n = 9834, replace = FALSE) # 80% training set
test <- SHOS_build[!SHOS_build$row %in% train$row, ] # 20% testing set

# Prepare data frame for predictions
SHOS_predict_season1 <- organize_predict(SHOS_ds1) # January-March
SHOS_predict_season2 <- organize_predict(SHOS_ds2) # April-June
SHOS_predict_season3 <- organize_predict(SHOS_ds3) # July-September
SHOS_predict_season4 <- organize_predict(SHOS_ds4) # October-December
