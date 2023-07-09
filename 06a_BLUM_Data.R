# DESCRIPTION: Assembling blue marlin dataset

# Load preliminaries
source("00_SetupGrid.R")
source("00_Preliminaries.R")
species <- "BLUM"
figure_dir <- here::here(figure_dir, species)

# Function to restrict adult distribution predictor to just blue marlin
restrict_predictor <- function(x){
  x %<>%
    dplyr::select(c(1:21, 30, 51)) %>%  # restrict the predictors
    dplyr::mutate(adult = ifelse(is.na(Makaira_nigricans), yes = 0, no = Makaira_nigricans)) # replace NAs of adult predictions to 0s
}

# Create species sf object
sf <- combineFish(species = "blue-marlin") %>% 
  fSpatPlan_Convert2PacificCentered(., cCRS = cCRS) %>% 
  sf::st_centroid() # transform into point data

seasons <- c("jan-mar", "apr-jun", "jul-sept", "oct-dec")
for(s in 1:length(seasons)) {
  gridded <- assembleGrid(grid, sf %>% dplyr::filter(season == seasons[s]))
  
  assign(paste("grid", species, seasons[s], sep = "_"), gridded)
}

# Load blue marlin datasets
BLUM_ds1 <- read_csv(here::here(input_dir, paste(species, "jan-mar.csv", sep = "_")), show_col_types = FALSE) %>% # January-March
  restrict_predictor()

BLUM_ds2 <- read_csv(here::here(input_dir, paste(species, "apr-jun.csv", sep = "_")), show_col_types = FALSE)  %>% # April-June
  restrict_predictor()

BLUM_ds3 <- read_csv(here::here(input_dir, paste(species, "jul-sept.csv", sep = "_")), show_col_types = FALSE)  %>% # July-September
  restrict_predictor()

BLUM_ds4 <- read_csv(here::here(input_dir, paste(species, "oct-dec.csv", sep = "_")), show_col_types = FALSE)  %>% # October-December
  restrict_predictor()

# Build model with known data only
BLUM_build <- dplyr::bind_rows(BLUM_ds1 %>% dplyr::filter(!is.na(abundance)),
                               BLUM_ds2 %>% dplyr::filter(!is.na(abundance)),
                               BLUM_ds3 %>% dplyr::filter(!is.na(abundance)),
                               BLUM_ds4 %>% dplyr::filter(!is.na(abundance))) %>% 
  organize_build()

# We divide the data into train (training and validation) and test
nrow(BLUM_build) * 0.8 # = 9834.4

set.seed(5533285)
train <- slice_sample(BLUM_build, n = 9834, replace = FALSE) # 80% training set
test <- BLUM_build[!BLUM_build$row %in% train$row, ] # 20% testing set

# Prepare data frame for predictions
BLUM_predict_season1 <- organize_predict(BLUM_ds1) # January-March
BLUM_predict_season2 <- organize_predict(BLUM_ds2) # April-June
BLUM_predict_season3 <- organize_predict(BLUM_ds3) # July-September
BLUM_predict_season4 <- organize_predict(BLUM_ds4) # October-December
