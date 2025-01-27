# DESCRIPTION: Assembling bluefin tuna dataset

# Load preliminaries
source(file.path("analyses", "02_preliminaries", "00_SetupGrid.R"))
source(file.path("analyses", "02_preliminaries", "00_Preliminaries.R"))
species <- "BFT"
figure_dir <- file.path(figure_dir, species)

# Function to restrict adult distribution predictor to just bluefin tunas
restrict_predictor <- function(x){
  x %<>%
    dplyr::select(c(1:21, 40, 51)) %>%  # restrict the predictors
    rowwise() %>% 
    dplyr::mutate(adult = mean(c(Thunnus_orientalis), na.rm = TRUE)) %>% # Just looking at Pacific tuna adult distributions
    ungroup() %>% 
    dplyr::mutate(adult = ifelse(is.na(adult), yes = 0, no = adult)) # replace NAs of adult predictions to 0s
}

files <- list.files(file.path("data_input", "fish"), full.names = TRUE, pattern = "a-bluefin")

obs <- purrr::map(files, readRDS) %>% 
  bind_rows()

# Create species sf object
sf <- combineFish(species = "a-bluefin-tuna") %>% 
  fSpatPlan_Convert2PacificCentered(., cCRS = cCRS) %>% 
  sf::st_centroid() # transform into point data

seasons <- c("jan-mar", "apr-jun", "jul-sep", "oct-dec")
for(s in 1:length(seasons)) {
  gridded <- assembleGrid(grid, sf %>% dplyr::filter(season == seasons[s]))
  
  assign(paste("grid", species, seasons[s], sep = "_"), gridded)
}

# Load striped marlin datasets
BFT_ds1 <- readRDS(file.path(input_dir, paste0(species, "_", seasons[1], ".rds"))) %>% 
  restrict_predictor()

BFT_ds2 <- readRDS(file.path(input_dir, paste0(species, "_", seasons[2], ".rds"))) %>% 
  restrict_predictor()

BFT_ds3 <- readRDS(file.path(input_dir, paste0(species, "_", seasons[3], ".rds"))) %>% 
  restrict_predictor()

BFT_ds4 <- readRDS(file.path(input_dir, paste0(species, "_", seasons[4], ".rds"))) %>% 
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
train <- slice_sample(BFT_build, n = 9834, replace = FALSE) # 80% training set
test <- BFT_build[!BFT_build$row %in% train$row, ] # 20% testing set

# Prepare data frame for predictions
BFT_predict_season1 <- organize_predict(BFT_ds1) # January-March
BFT_predict_season2 <- organize_predict(BFT_ds2) # April-June
BFT_predict_season3 <- organize_predict(BFT_ds3) # July-September
BFT_predict_season4 <- organize_predict(BFT_ds4) # October-December
