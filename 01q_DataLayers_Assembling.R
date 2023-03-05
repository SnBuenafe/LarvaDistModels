# Load preliminaries
source("00_Utils.R")

#############
# Fish data #
#############
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

# Grid per season
for(i in 1:nrow(species_code)) {
  sf <- combineFish(species = species_code$species[i]) %>% 
    fSpatPlan_Convert2PacificCentered(., cCRS = moll_pacific) %>% 
    sf::st_centroid() # transform into point data
  
  seasons <- c("jan-mar", "apr-jun", "jul-sept", "oct-dec")
  for(s in 1:length(seasons)) {
    gridded <- assembleGrid(grid, sf %>% dplyr::filter(season == seasons[s]))
    
    assign(paste("grid", species_code$code[i], seasons[s], sep = "_"), gridded)
  }
}

##########################
# Load climatology data #
##########################
predictors <- c("tos", "o2os", "phos", "chlos", "sos", "mlotst", "no3os", "po4os", "nh4os", "thermal_front", "salinity_front", "mesoscale_features")

# Load historical data
seasons <- c("jan-mar", "apr-jun", "jul-sept", "oct-dec")

for(p in 1:length(predictors)) {
  for(s in 1:length(seasons)) {
    df <- readRDS(paste0("Data/Climatology/sf/", predictors[p], "_historical_", seasons[s], "_interpolated.rds")) %>% 
      dplyr::select(-geometry)
    
    assign(paste(predictors[p], "historical", seasons[s], sep = "_"), df)
  }
}

##########################
# Load other predictors #
##########################
# Bathymetry
bathy <- readRDS("Data/GEBCO/gebco2500.rds") %>% 
  dplyr::as_tibble() %>% 
  dplyr::select(-geometry)

# Coastline
dist2coast <- readRDS("Data/CoastDistance.rds") %>% 
  dplyr::as_tibble() %>% 
  dplyr::select(-geometry)

# AquaMaps
aqua <- readRDS("Data/AquaMaps_sf.rds") %>% 
  dplyr::as_tibble() %>% 
  dplyr::left_join(., grid) %>% 
  dplyr::select(-geometry)

###############################################################
# Joining all predictor and response per fish species #
###############################################################

# Joining all the data with the species data for each season (HISTORICAL)
for(f in 1:nrow(species_code)) {
  for(s in 1:length(seasons)) {
    df <- joinPredictors(grid = eval(sym(paste("grid", species_code$code[f], seasons[s], sep = "_"))),
                         tos = eval(sym(paste("tos_historical", seasons[s], sep = "_"))),
                         o2os = eval(sym(paste("o2os_historical", seasons[s], sep = "_"))),
                         phos = eval(sym(paste("phos_historical", seasons[s], sep = "_"))),
                         chlos = eval(sym(paste("chlos_historical", seasons[s], sep = "_"))),
                         sos = eval(sym(paste("sos_historical", seasons[s], sep = "_"))),
                         mlotst = eval(sym(paste("mlotst_historical", seasons[s], sep = "_"))),
                         no3os = eval(sym(paste("no3os_historical", seasons[s], sep = "_"))),
                         po4os = eval(sym(paste("po4os_historical", seasons[s], sep = "_"))),
                         nh4os = eval(sym(paste("nh4os_historical", seasons[s], sep = "_"))),
                         tf = eval(sym(paste("thermal_front_historical", seasons[s], sep = "_"))),
                         sf = eval(sym(paste("salinity_front_historical", seasons[s], sep = "_"))),
                         mesoscale = eval(sym(paste("mesoscale_features_historical", seasons[s], sep = "_"))),
                         bathy = bathy,
                         dist2coast = dist2coast,
                         species = aqua)
    
    write_csv(df, file = paste0("Output/CSV/", species_code$code[f], "_historical_", seasons[s], ".csv"))
  }
}
