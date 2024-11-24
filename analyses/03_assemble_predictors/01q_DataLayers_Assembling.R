# DESCRIPTION: Assembling grids for all species

# Load packages
library(here)

# Define directories
preliminaries_dir <- here("analyses", "02_preliminaries")

# Load preliminaries
source(here(preliminaries_dir, "00_SetupGrid.R"))
source("functions/combineFish.R")
source("functions/assembleGrid.R")
source("functions/joinPredictors.R")
input_dir <- here("data_input")

# List all species in Nishikawa dataset
species_code <- tibble::tribble(~species, ~code,
                                "yellowfin-tuna", "YFT",
                                "albacore", "ALB",
                                "skipjack-tuna", "SKP",
                                "swordfish", "SWO",
                                "blue-marlin", "BLUM",
                                "shortbill-spearfish", "SHOS",
                                "frigate-tuna", "FRI",
                                "bigeye-tuna", "BET",
                                "striped-marlin", "STRM",
                                "sauries", "SAU",
                                "sailfish", "SAIL",
                                "longfin-escolar", "LESC",
                                "a-bluefin-tuna", "BFT",
                                "little-tuna", "LIT",
                                "southern-bluefin-tuna", "SBFT",
                                "slender-tuna", "SLT",
                                "bonitos", "BON",
                                "black-marlin", "BLAM"
                                )

# Assemble fish grids per season
for(i in 1:nrow(species_code)) {
  sf <- combineFish(species = species_code$species[i]) %>% # combine seasonal data of species into one sf object
    fSpatPlan_Convert2PacificCentered(., cCRS = cCRS) %>% 
    st_centroid() # transform into point data
  
  seasons <- c("jan-mar", "apr-jun", "jul-sep", "oct-dec")
  for(s in 1:length(seasons)) {
    gridded <- assembleGrid(grid, sf %>% 
                              filter(season == seasons[s])) # assemble the fish data on the grid
    
    assign(paste("grid", species_code$code[i], seasons[s], sep = "_"), gridded)
  }
}

# Load climatology data
predictors <- c("tos", "o2os", "phos", "chlos", "sos", "mlotst", "no3os", "po4os", "nh4os", "thermal_front", "salinity_front", "eke")

seasons <- c("jan-mar", "apr-jun", "jul-sep", "oct-dec")

for(p in 1:length(predictors)) {
  for(s in 1:length(seasons)) {
    df <- readRDS(here::here(input_dir, "climate_sf", paste0(predictors[p], "_omip2_", seasons[s], "_interpolated.rds"))) %>% 
      dplyr::select(-geometry)
    
    assign(paste(predictors[p], seasons[s], sep = "_"), df)
  }
}

# Load other predictors
# Bathymetry
bathy <- readRDS(here::here(input_dir, "gebco.rds")) %>% 
  dplyr::as_tibble() %>% 
  dplyr::select(-geometry)

# Coastline
dist2coast <- readRDS(here::here(input_dir, "coast_distance.rds")) %>% 
  dplyr::as_tibble() %>% 
  dplyr::select(-geometry, -ocean)

# AquaMaps
aqua <- readRDS(here::here(input_dir, "AquaMaps_sf.rds")) %>% 
  dplyr::as_tibble() %>% 
  dplyr::select(-geometry)

# Joining all the data with the species data for each season
for(f in 1:nrow(species_code)) {
  for(s in 1:length(seasons)) {
    df <- joinPredictors(grid = eval(sym(paste("grid", species_code$code[f], seasons[s], sep = "_"))),
                         tos = eval(sym(paste("tos", seasons[s], sep = "_"))),
                         o2os = eval(sym(paste("o2os", seasons[s], sep = "_"))),
                         phos = eval(sym(paste("phos", seasons[s], sep = "_"))),
                         chlos = eval(sym(paste("chlos", seasons[s], sep = "_"))),
                         sos = eval(sym(paste("sos", seasons[s], sep = "_"))),
                         mlotst = eval(sym(paste("mlotst", seasons[s], sep = "_"))),
                         no3os = eval(sym(paste("no3os", seasons[s], sep = "_"))),
                         po4os = eval(sym(paste("po4os", seasons[s], sep = "_"))),
                         nh4os = eval(sym(paste("nh4os", seasons[s], sep = "_"))),
                         tf = eval(sym(paste("thermal_front", seasons[s], sep = "_"))),
                         sf = eval(sym(paste("salinity_front", seasons[s], sep = "_"))),
                         eke = eval(sym(paste("eke", seasons[s], sep = "_"))),
                         bathy = bathy,
                         dist2coast = dist2coast,
                         species = aqua)
    
    saveRDS(df, file = paste0("data_output/ModelPredictors/", paste(species_code$code[f], seasons[s], sep = "_"), ".rds"))
  }
}
