# Load preliminaries
#source("00_Utils.R")

# If any of the data layers need to be reprocessed, run 01b-01k first

####################
# Fish data #
####################
species_code <- tibble::tribble(~species, ~code,
                                # "yellowfin-tuna", "YFT",
                                "albacore", "ALB",
                                # "skipjack-tuna", "SKP",
                                "swordfish", "SWO",
                                "blue-marlin", "BLUM",
                                "shortbill-spearfish", "SHOS",
                                "frigate-tuna", "FRI",
                                "bigeye-tuna", "BET",
                                "striped-marlin", "STRM",
                                "sauries", "SAU",
                                "sailfish", "SAIL",
                                "longfin-escolar", "LESC",
                                "bluefin-tuna", "BFT",
                                "little-tuna", "LIT",
                                "southern-bluefin-tuna", "SBFT",
                                "slender-tuna", "SLT",
                                "bonitos", "BON",
                                "black-marlin", "BLAM"
                                )

# Grid per season
for(i in 1:nrow(species_code)) {
  sf <- combineFish(species = species_code$species[i]) %>% 
    sf::st_transform(crs = moll) %>% 
    sf::st_centroid() # transform into point data
  
  seasons <- c("jan-mar", "apr-jun", "jul-sept", "oct-dec")
  for(s in 1:length(seasons)) {
    gridded <- assembleGrid(grid, sf %>% dplyr::filter(season == seasons[s]))
    
    assign(paste("grid", species_code$code[i], seasons[s], sep = "_"), gridded)
  }
}

# Full grid
# for(i in 1:nrow(species_code)) {
#   sf <- combineFish(species = species_code$species[i]) %>% 
#     sf::st_transform(crs = moll) %>% 
#     sf::st_centroid() # transform into point data
#   
#   gridded <- assembleGrid(grid, sf) %>% 
#     dplyr::group_by(cellID) %>% 
#     dplyr::summarise(ocean = unique(ocean), species = unique(species), abundance = sum(abundance), longitude = unique(longitude),
#                      latitude = unique(latitude), geometry = unique(geometry))
#   
#   assign(paste("grid", species_code$code[i], sep = "_"), gridded)
# }

##########################
# Load climatology data #
##########################
predictors <- c("tos", "o2os", "phos", "chlos", "sos", "mlotst", "no3os", "po4os", "nh4os")

# Load full data
timestamps <- c("historical", "present", "midCentury", "endCentury")
# for(p in 1:length(predictors)) {
#   for(t in 1:length(timestamps)) {
#     df <- readRDS(paste0("Data/Climatology/sf/", predictors[p], "_", timestamps[t], "_interpolated.rds")) %>%
#       dplyr::select(-geometry)
# 
#     assign(paste(predictors[p], timestamps[t], sep = "_"), df)
#   }
# }

# Load historical data
seasons <- c("jan-mar", "apr-jun", "jul-sept", "oct-dec")

for(p in 1:length(predictors)) {
  for(s in 1:length(seasons)) {
    df <- readRDS(paste0("Data/Climatology/sf/", predictors[p], "_historical_", seasons[s], "_interpolated.rds")) %>% 
      dplyr::select(-geometry)
    
    assign(paste(predictors[p], "historical", seasons[s], sep = "_"), df)
  }
}

# Load present data
# for(p in 1:length(predictors)) {
#   for(s in 1:length(seasons)) {
#     df <- readRDS(paste0("Data/Climatology/sf/", predictors[p], "_present_", seasons[s], "_interpolated.rds")) %>% 
#       dplyr::select(-geometry)
#     
#     assign(paste(predictors[p], "present", seasons[s], sep = "_"), df)
#   }
# }

# Load mid-century data
# for(p in 1:length(predictors)) {
#   for(s in 1:length(seasons)) {
#     df <- readRDS(paste0("Data/Climatology/sf/", predictors[p], "_midCentury_", seasons[s], "_interpolated.rds")) %>% 
#       dplyr::select(-geometry)
#     
#     assign(paste(predictors[p], "midCentury", seasons[s], sep = "_"), df)
#   }
# }

# Load end of the century data
# for(p in 1:length(predictors)) {
#   for(s in 1:length(seasons)) {
#     df <- readRDS(paste0("Data/Climatology/sf/", predictors[p], "_endCentury_", seasons[s], "_interpolated.rds")) %>% 
#       dplyr::select(-geometry)
#     
#     assign(paste(predictors[p], "endCentury", seasons[s], sep = "_"), df)
#   }
# }

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

###############################################################
# Joining all predictor and response per fish species #
###############################################################

# Joining all the data with the species data (FULL)
# for(f in 1:nrow(species_code)) {
#   for(t in 1:length(timestamps)) {
#     df <- joinPredictors(grid = eval(sym(paste("grid", species_code$code[f], sep = "_"))),
#                          tos = eval(sym(paste("tos", timestamps[t], sep = "_"))),
#                          o2os = eval(sym(paste("o2os", timestamps[t], sep = "_"))),
#                          phos = eval(sym(paste("phos", timestamps[t], sep = "_"))),
#                          chlos = eval(sym(paste("chlos", timestamps[t], sep = "_"))),
#                          sos = eval(sym(paste("sos", timestamps[t], sep = "_"))),
#                          mlotst = eval(sym(paste("mlotst", timestamps[t], sep = "_"))),
#                          no3os = eval(sym(paste("no3os", timestamps[t], sep = "_"))),
#                          po4os = eval(sym(paste("po4os", timestamps[t], sep = "_"))),
#                          nh4os = eval(sym(paste("nh4os", timestamps[t], sep = "_"))),
#                          bathy = bathy,
#                          dist2coast = dist2coast,
#                          season = FALSE)
#     
#     write_csv(df, file = paste0("Output/CSV/", species_code$code[f], "_", timestamps[t], "_full.csv"))
#   }
# }

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
                         bathy = bathy,
                         dist2coast = dist2coast)
    
    write_csv(df, file = paste0("Output/CSV/", species_code$code[f], "_historical_", seasons[s], ".csv"))
  }
}

# Joining all the data with the species data for each season (PRESENT)
# for(f in 1:nrow(species_code)) {
#   for(s in 1:length(seasons)) {
#     df <- joinPredictors(grid = eval(sym(paste("grid", species_code$code[f], seasons[s], sep = "_"))),
#                          tos = eval(sym(paste("tos_present", seasons[s], sep = "_"))),
#                          o2os = eval(sym(paste("o2os_present", seasons[s], sep = "_"))),
#                          phos = eval(sym(paste("phos_present", seasons[s], sep = "_"))),
#                          chlos = eval(sym(paste("chlos_present", seasons[s], sep = "_"))),
#                          sos = eval(sym(paste("sos_present", seasons[s], sep = "_"))),
#                          mlotst = eval(sym(paste("mlotst_present", seasons[s], sep = "_"))),
#                          no3os = eval(sym(paste("no3os_present", seasons[s], sep = "_"))),
#                          po4os = eval(sym(paste("po4os_present", seasons[s], sep = "_"))),
#                          nh4os = eval(sym(paste("nh4os_present", seasons[s], sep = "_"))),
#                          bathy = bathy,
#                          dist2coast = dist2coast)
#     
#     write_csv(df, file = paste0("Output/CSV/", species_code$code[f], "_present_", seasons[s], ".csv"))
#   }
# }

# Joining all the data with the species data for each season (MID-CENTURY)
# for(f in 1:nrow(species_code)) {
#   for(s in 1:length(seasons)) {
#     df <- joinPredictors(grid = eval(sym(paste("grid", species_code$code[f], seasons[s], sep = "_"))),
#                          tos = eval(sym(paste("tos_midCentury", seasons[s], sep = "_"))),
#                          o2os = eval(sym(paste("o2os_midCentury", seasons[s], sep = "_"))),
#                          phos = eval(sym(paste("phos_midCentury", seasons[s], sep = "_"))),
#                          chlos = eval(sym(paste("chlos_midCentury", seasons[s], sep = "_"))),
#                          sos = eval(sym(paste("sos_midCentury", seasons[s], sep = "_"))),
#                          mlotst = eval(sym(paste("mlotst_midCentury", seasons[s], sep = "_"))),
#                          no3os = eval(sym(paste("no3os_midCentury", seasons[s], sep = "_"))),
#                          po4os = eval(sym(paste("po4os_midCentury", seasons[s], sep = "_"))),
#                          nh4os = eval(sym(paste("nh4os_midCentury", seasons[s], sep = "_"))),
#                          bathy = bathy,
#                          dist2coast = dist2coast)
#     
#     write_csv(df, file = paste0("Output/CSV/", species_code$code[f], "_midCentury_", seasons[s], ".csv"))
#   }
# }

# Joining all the data with the species data for each season (END OF THE CENTURY)
# for(f in 1:nrow(species_code)) {
#   for(s in 1:length(seasons)) {
#     df <- joinPredictors(grid = eval(sym(paste("grid", species_code$code[f], seasons[s], sep = "_"))),
#                          tos = eval(sym(paste("tos_endCentury", seasons[s], sep = "_"))),
#                          o2os = eval(sym(paste("o2os_endCentury", seasons[s], sep = "_"))),
#                          phos = eval(sym(paste("phos_endCentury", seasons[s], sep = "_"))),
#                          chlos = eval(sym(paste("chlos_endCentury", seasons[s], sep = "_"))),
#                          sos = eval(sym(paste("sos_endCentury", seasons[s], sep = "_"))),
#                          mlotst = eval(sym(paste("mlotst_endCentury", seasons[s], sep = "_"))),
#                          no3os = eval(sym(paste("no3os_endCentury", seasons[s], sep = "_"))),
#                          po4os = eval(sym(paste("po4os_endCentury", seasons[s], sep = "_"))),
#                          nh4os = eval(sym(paste("nh4os_endCentury", seasons[s], sep = "_"))),
#                          bathy = bathy,
#                          dist2coast = dist2coast)
#     
#     write_csv(df, file = paste0("Output/CSV/", species_code$code[f], "_endCentury_", seasons[s], ".csv"))
#   }
# }
