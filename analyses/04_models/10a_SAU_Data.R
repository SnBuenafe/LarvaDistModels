# DESCRIPTION: Assembling sauries dataset

# Load preliminaries
source(file.path("analyses", "02_preliminaries", "00_SetupGrid.R"))
source(file.path("analyses", "02_preliminaries", "00_Preliminaries.R"))
species <- "SAU"
figure_dir <- here::here(figure_dir, species)

# Function to restrict adult distribution predictor to just sauries
restrict_predictor <- function(x){
  x %<>%
    dplyr::select(c(1:21, 35:37, 51)) %>%  # restrict the predictors
    rowwise() %>% 
    dplyr::mutate(adult = mean(c(Cololabis_saira, Cololabis_adocetus, Scomberesox_saurus), na.rm = TRUE)) %>% 
    ungroup() %>% 
    dplyr::mutate(adult = ifelse(is.na(adult), yes = 0, no = adult)) # replace NAs of adult predictions to 0s
}

# Create species sf object
sf <- combineFish(species = "sauries") %>% 
  fSpatPlan_Convert2PacificCentered(., cCRS = cCRS) %>% 
  sf::st_centroid() # transform into point data

seasons <- c("jan-mar", "apr-jun", "jul-sep", "oct-dec")
for(s in 1:length(seasons)) {
  gridded <- assembleGrid(grid, sf %>% dplyr::filter(season == seasons[s]))
  
  assign(paste("grid", species, seasons[s], sep = "_"), gridded)
}

# Load sauries datasets
SAU_ds1 <- readRDS(here::here(input_dir, paste0(species, "_", seasons[1], ".rds"))) %>% 
  restrict_predictor()

SAU_ds2 <- readRDS(here::here(input_dir, paste0(species, "_", seasons[2], ".rds"))) %>% 
  restrict_predictor()

SAU_ds3 <- readRDS(here::here(input_dir, paste0(species, "_", seasons[3], ".rds"))) %>% 
  restrict_predictor()

SAU_ds4 <- readRDS(here::here(input_dir, paste0(species, "_", seasons[4], ".rds"))) %>% 
  restrict_predictor()

# Build model with known data only
SAU_build <- dplyr::bind_rows(SAU_ds1 %>% dplyr::filter(!is.na(abundance)),
                               SAU_ds2 %>% dplyr::filter(!is.na(abundance)),
                               SAU_ds3 %>% dplyr::filter(!is.na(abundance)),
                               SAU_ds4 %>% dplyr::filter(!is.na(abundance))) %>% 
  organize_build()

# We divide the data into train (training and validation) and test
nrow(SAU_build) * 0.8 # = 9834.4

set.seed(4411)
train <- slice_sample(SAU_build, n = 9834, replace = FALSE) # 80% training set
test <- SAU_build[!SAU_build$row %in% train$row, ] # 20% testing set

# Prepare data frame for predictions
SAU_predict_season1 <- organize_predict(SAU_ds1) # January-March
SAU_predict_season2 <- organize_predict(SAU_ds2) # April-June
SAU_predict_season3 <- organize_predict(SAU_ds3) # July-September
SAU_predict_season4 <- organize_predict(SAU_ds4) # October-December
