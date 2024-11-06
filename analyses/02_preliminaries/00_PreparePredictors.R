# DESCRIPTION: Setting preliminaries for preparing the predictors

# Load packages
# install.packages("pacman")
pacman::p_load(tidyverse, sf, terra, patchwork, stars, purrr, cmocean, here, RColorBrewer, data.table)

# Set up directories
input_dir <- here("data_input", "climate_ensemble")
output_dir <- here("data_input", "climate_sf")
figure_dir <- here("figures")

# Load grid
source(here(preliminaries_dir, "00_SetupGrid.R"))

# Call helper functions
source("functions/rs2sf.R")
source("functions/spat2sf.R")
source("functions/replaceNN.R")
source("functions/change_gglayout.R")

# Load modified VoCC R package functions
# See here for the original code: https://github.com/JorGarMol/VoCC
# See here for the modified code: https://github.com/DavidSchoeman/terra_VoCC

source("functions/spatGrad.R")
source("functions/angulo.R")