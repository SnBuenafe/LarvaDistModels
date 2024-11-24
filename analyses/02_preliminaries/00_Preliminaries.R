# DESCRIPTION: Setting preliminaries for the rest of the code

# install.packages("pacman")
pacman::p_load(tidyverse, here, dismo, gbm, cmocean, magrittr, patchwork, sf)

# Load all helper functions
utils <- list.files(path = here("functions"), pattern = "*.R", full.names = TRUE)
sapply(X = utils, FUN = source) %>% invisible()

# Directories
input_dir <- here("data_output", "ModelPredictors")
figure_dir <- here("Figures")
model_dir <- here("data_output", "Models")
CVgrid_dir <- here("data_output", "CVGrid")
preds_dir <- here("data_output", "Predictions")
clim_dir <- here("Data", "Climatology", "sf")
pc_dir <- here("data_output", "PCA")
rast_dir <- here("data_output", "FinalRaster")

brt_cols <- c("longitude", "latitude", "season", "tos_transformed", "o2os_transformed",
              "phos_transformed", "chlos_transformed", "sos_transformed", 
              "mlotst_transformed", "no3os_transformed", "po4os_transformed", 
              "nh4os_transformed", "thermal_front_transformed", 
              "salinity_front_transformed", "eke", "meanDepth", "coastDistance") # "adult" # c(7:23, 25)

spec_dict <- tibble::tribble(
  ~code, ~common,
  "YFT", "Yellowfin tuna",
  "SKP", "Skipjack tuna",
  "ALB", "Albacore",
  "SWO", "Swordfish",
  "BLUM", "Blue marlin",
  "FRI", "Frigate tuna",
  "BET", "Bigeye tuna",
  "BFT", "P. bluefin tuna",
  "SAU", "Sauries",
  "SAIL", "Sailfish",
  "SBFT", "S. bluefin tuna",
  "SLT", "Slender tuna",
  "BON", "Bonitos",
  "SHOS", "Shortbill spearfish",
  "STRM", "Striped marlin",
  "LESC", "Longfin escolar",
  "LIT", "Little tuna"
)