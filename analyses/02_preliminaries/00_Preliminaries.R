# DESCRIPTION: Setting preliminaries for the rest of the code

# install.packages("pacman")
pacman::p_load(tidyverse, here, dismo, gbm, cmocean, magrittr, patchwork)

# Load all helper functions
utils <- list.files(path = here("Utils"), pattern = "*.R", full.names = TRUE)
sapply(X = utils, FUN = source) %>% invisible()

# Directories
input_dir <- here("Output", "CSV")
figure_dir <- here("Figures")
model_dir <- here("Output", "Models")
preds_dir <- here("Output", "Predictions")
clim_dir <- here("Data", "Climatology", "sf")
pc_dir <- here("Output", "PCA")
rast_dir <- here("Output", "FinalRaster")

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