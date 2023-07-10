# Load packages
# install.packages("pacman")
pacman::p_load(tidyverse, sf, terra, patchwork, stars, purrr, cmocean, RColorBrewer)

# Load grid
source("00_SetupGrid.R")

# Set up directories
input_dir <- here::here("Data", "Climatology", "ensemble")
output_dir <- here::here("Data", "Climatology", "sf")
figure_dir <- here::here("Figures")

# Call helper functions
source("Utils/rs2sf.R")
source("Utils/spat2sf.R")
source("Utils/replaceNN.R")
source("Utils/change_gglayout.R")
