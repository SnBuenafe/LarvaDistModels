# Call packages
suppressPackageStartupMessages({
  library(tidyverse) # needed
  library(ncdf4)
  library(terra) # needed
  library(sf) # needed
  library(rnaturalearth) # needed
  library(cmocean)
  library(patchwork)
  library(magrittr)
  library(raster)
  library(dismo) # for boosted regression trees
  library(RColorBrewer)
  library(patchwork) # needed
  library(Hmisc)
  library(corrplot)
  library(VoCC)
  library(stars) # needed
  library(spatialplanr)
  library(ggpattern)
  library(here) # needed
  library(ggridges)
})

# Load all helper functions
utils <- list.files(path = here::here("Utils"), pattern = "*.R", full.names = TRUE)
sapply(X = utils, FUN = source) %>% invisible()

