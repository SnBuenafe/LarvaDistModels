# DESCRIPTION: Assembling albacore dataset

# Load preliminaries
source(file.path("analyses", "02_preliminaries", "00_SetupGrid.R"))
source(file.path("analyses", "02_preliminaries", "00_Preliminaries.R"))
species <- "ALB"
figure_dir <- file.path(figure_dir, species)

# Function to restrict adult distribution predictor to just albacore
restrict_predictor <- function(x){
  x %<>%
    dplyr::select(c(1:21, 24, 51)) %>%  # restrict the predictors
    dplyr::mutate(adult = ifelse(is.na(Thunnus_alalunga), yes = 0, no = Thunnus_alalunga)) # replace NAs of adult predictions to 0s
}

# Create species sf object
sf <- combineFish(species = "albacore") %>% 
  fSpatPlan_Convert2PacificCentered(., cCRS = cCRS) %>% 
  sf::st_centroid() # transform into point data

seasons <- c("jan-mar", "apr-jun", "jul-sep", "oct-dec")
for(s in 1:length(seasons)) {
  gridded <- assembleGrid(grid, sf %>% dplyr::filter(season == seasons[s]))
  
  assign(paste("grid", species, seasons[s], sep = "_"), gridded)
}

# Load albacore datasets
ALB_ds1 <- readRDS(file.path(input_dir, paste0(species, "_", seasons[1], ".rds"))) %>% 
  restrict_predictor()

ALB_ds2 <- readRDS(file.path(input_dir, paste0(species, "_", seasons[2], ".rds"))) %>% 
  restrict_predictor()

ALB_ds3 <- readRDS(file.path(input_dir, paste0(species, "_", seasons[3], ".rds"))) %>% 
  restrict_predictor()

ALB_ds4 <- readRDS(file.path(input_dir, paste0(species, "_", seasons[4], ".rds"))) %>% 
  restrict_predictor()

# Build model with known data only
ALB_build <- dplyr::bind_rows(ALB_ds1 %>% dplyr::filter(!is.na(abundance)),
                              ALB_ds2 %>% dplyr::filter(!is.na(abundance)),
                              ALB_ds3 %>% dplyr::filter(!is.na(abundance)),
                              ALB_ds4 %>% dplyr::filter(!is.na(abundance))) %>% 
  organize_build()

# We divide the data into train (training and validation) and test
nrow(ALB_build) * 0.8 # = 9834.4

set.seed(5301)
train <- slice_sample(ALB_build, n = 9834, replace = FALSE) # 80% training set
test <- ALB_build[!ALB_build$row %in% train$row, ] # 20% testing set

# Prepare data frame for predictions
ALB_predict_season1 <- organize_predict(ALB_ds1) # January-March
ALB_predict_season2 <- organize_predict(ALB_ds2) # April-June
ALB_predict_season3 <- organize_predict(ALB_ds3) # July-September
ALB_predict_season4 <- organize_predict(ALB_ds4) # October-December
