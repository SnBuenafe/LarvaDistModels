# install.packages("pacman")
pacman::p_load(tidyverse, here, dismo, gbm, cmocean, magrittr, patchwork)

# Load all helper functions
utils <- list.files(path = here::here("Utils"), pattern = "*.R", full.names = TRUE)
sapply(X = utils, FUN = source) %>% invisible()

# Directories
input_dir <- here::here("Output", "CSV")
figure_dir <- here::here("Figures")
model_dir <- here::here("Output", "Models")
preds_dir <- here::here("Output", "Predictions")
clim_dir <- here::here("Data", "Climatology", "sf")
pc_dir <- here::here("Output", "PCA")
rast_dir <- here::here("Output", "FinalRaster")
