# # Call packages
# suppressPackageStartupMessages({
#   library(tidyverse) # needed
#   library(ncdf4)
#   library(terra) # needed
#   library(sf) # needed
#   library(rnaturalearth) # needed
#   library(cmocean)
#   library(patchwork)
#   library(magrittr)
#   library(raster)
#   library(dismo) # for boosted regression trees
#   library(RColorBrewer)
#   library(patchwork) # needed
#   library(Hmisc)
#   library(corrplot)
#   library(VoCC)
#   library(stars) # needed
#   library(spatialplanr)
#   library(ggpattern)
#   library(here) # needed
#   library(ggridges)
# })

# install.packages("pacman")
pacman::p_load(tidyverse, here, dismo, gbm, cmocean, magrittr)

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
