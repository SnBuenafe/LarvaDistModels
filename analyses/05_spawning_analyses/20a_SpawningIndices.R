# DESCRIPTION: Calculating the Spatial Aggregation Index and Seasonality Index

# Defining preliminaries --------------------------------------------------

source(file.path("analyses", "02_preliminaries", "00_Preliminaries.R"))
# source(file.path("Utils", "fxnshemisphere.R"))

pacman::p_load(purrr, magrittr, ggrepel, patchwork)

figure_dir <- file.path(figure_dir, "spawning_indices")

seasons <- c("jan-mar", "apr-jun", "jul-sep", "oct-dec")

# Add the groups
spec_dict
groups <- c("Tuna", "Tuna", "Tuna", "Billfish", "Billfish", "Tuna", "Tuna", "Tuna", "Other", "Billfish", "Tuna", "Tuna", "Billfish", "Billfish", "Other")
LifeHistory <- c("Fast", "Fast", "Slow", "Slow", "Slow", "Fast", "Fast", "Slow", "Unknown", "Slow", "Slow", "Unknown", "Slow", "Slow", "Unknown")

spec_dict %<>%
  dplyr::filter(!code %in% c("BON", "LIT")) %>% # Remove species with too few data
  dplyr::bind_cols(., groups = groups) %>% 
  dplyr::bind_cols(., lifehistory = LifeHistory)


# Spatial Aggregation Index -----------------------------------------------

# Define the Spatial Aggregation Index (SAI) based on coefficient of variation)
# Logic: for each season for each species, the larger the sd the more spatially aggregated they are
index <- function(x) {
  ind <- sd(x, na.rm = TRUE)/(mean(x, na.rm = TRUE))
}

# Prepare data
df_SAI <- list()
for(i in 1:length(seasons)) {
  # Load file
  tmp <- terra::rast(file.path(rast_dir, paste("ModelOutputs", paste0(seasons[i], ".tif"), sep = "_"))) %>% 
    as.data.frame(xy = TRUE) %>% # get longitude and latitudes
    dplyr::as_tibble() %>% 
    dplyr::mutate(hemisphere = ifelse(y >= 0, "North", "South")) # separate them per hemisphere
  
  # Repeat for all 4 seasons            
  df_SAI[[i]] <- tmp %>% 
    dplyr::group_by(hemisphere) %>% 
    dplyr::summarise(across(!c(x, y), index)) %>% # *** Calculate SAI
    tidyr::pivot_longer(cols = !c(hemisphere),
                        names_to = "species", 
                        values_to = "ind") %>% 
    dplyr::rename(!!sym(paste("ind", seasons[i], sep = "_")) := ind) # Rename columns
}

# Calculate across seasons
full_SAI <- purrr::reduce(df_SAI, dplyr::left_join, by = c("species", "hemisphere")) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(SAI = mean(c_across(starts_with("ind")), na.rm = TRUE)) %>% # SAI Annual = Mean of SAI across Seasons
  dplyr::mutate(SAI_SEM = sd(c_across(starts_with("ind")), na.rm = TRUE)/sqrt(length(c_across(starts_with("ind"))))) %>% # calculating standard error of mean
  dplyr::select(species, hemisphere, SAI, SAI_SEM) %>% 
  dplyr::mutate(code = toupper(species)) %>% 
  dplyr::select(-species) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(hemisphere) %>% 
  dplyr::left_join(., spec_dict, by = "code") # %>% 
  # dplyr::mutate(common = fct_reorder(common, desc(SAI))) %>% # We want to plot it with the common names
  # dplyr::bind_cols(., )


# Seasonality Index -------------------------------------------------------
# Seasonality Index = standard deviation (across seasons) of the mean of probabilities
df_SI <- list()
for(i in 1:length(seasons)) {
  # Load file
  tmp <- terra::rast(file.path(rast_dir, paste("ModelOutputs", paste0(seasons[i], ".tif"), sep = "_"))) %>% 
    as.data.frame(xy = TRUE) %>% # get longitude and latitudes
    dplyr::as_tibble() %>% 
    dplyr::mutate(hemisphere = ifelse(y >= 0, "North", "South")) # separate them per hemisphere
  
  # Repeat for all 4 seasons            
    df_SI[[i]] <- tmp %>% 
    dplyr::group_by(hemisphere) %>% 
    dplyr::summarise(across(!c(x, y), mean, na.rm = TRUE)) %>% # Calculate the index
    tidyr::pivot_longer(cols = !c(hemisphere),
                        names_to = "species", 
                        values_to = "ind") %>% 
    dplyr::rename(!!sym(paste("ind", seasons[i], sep = "_")) := ind) # Rename columns
}

# Calculate across seasons
full_SI <- purrr::reduce(df_SI, dplyr::left_join, by = c("species", "hemisphere")) %>% 
  dplyr::rowwise() %>% 
  # dplyr::mutate(SI = sd(c_across(starts_with("ind")), na.rm = TRUE)) %>% # Take the SD across seasons for mean per species (Seasonality Index)
  dplyr::mutate(SI = sd(c_across(starts_with("ind")), na.rm = TRUE) / mean(c_across(starts_with("ind")), na.rm = TRUE)) %>% # Take the SD across seasons for mean per species and standardise for the mean (Seasonality Index)
  dplyr::mutate(SI_SEM = sd(c_across(starts_with("ind")), na.rm = TRUE)/sqrt(length(c_across(starts_with("ind"))))) %>% # calculating standard error of mean
  dplyr::select(species, hemisphere, SI, SI_SEM) %>% 
  dplyr::mutate(code = toupper(species)) %>% 
  dplyr::select(-species) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(hemisphere) %>% 
  dplyr::left_join(., spec_dict, by = "code")

# Calculate weighted mean latitudes ---------------------------------------

# Calculate the weighted mean Lat for each species in each hemisphere
# Calculate MnLat for each Season (for each Species and each Hemisphere)
df_MnLat <- list()
for(i in 1:length(seasons)) {
  # Load file
  tmp <- terra::rast(file.path(rast_dir, paste("ModelOutputs", paste0(seasons[i], ".tif"), sep = "_"))) %>% 
    as.data.frame(xy = TRUE) %>% # get longitude and latitudes
    dplyr::as_tibble() %>% 
    dplyr::mutate(hemisphere = ifelse(y >= 0, "North", "South")) # separate them per hemisphere
  
  # Repeat for all 4 seasons            
  df_MnLat[[i]] <- tmp %>% 
    dplyr::group_by(hemisphere) %>% 
    dplyr::summarise(across(!c(x, y), ~ sum(.x * y, na.rm = TRUE)/sum(.x, na.rm = TRUE))) %>% # Calculate index2
    tidyr::pivot_longer(cols = !c(hemisphere),
                        names_to = "species", 
                        values_to = "ind") %>% 
    dplyr::rename(!!sym(paste("ind", seasons[i], sep = "_")) := ind) # Rename columns
}

# Calculate across seasons
full_MnLat <- purrr::reduce(df_MnLat, dplyr::left_join, by = c("species", "hemisphere")) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(MnLat = mean(c_across(starts_with("ind")), na.rm = TRUE)) %>%
  dplyr::select(species, hemisphere, MnLat) %>% 
  dplyr::mutate(code = toupper(species)) %>% 
  dplyr::select(-species) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(hemisphere) %>% 
  dplyr::left_join(., spec_dict, by = "code")


# Joining data frames -----------------------------------------------------

# Join SAI and SI in one df
full <- left_join(full_SAI, full_SI) #, by = c("common", "hemisphere"))
full <- left_join(full, full_MnLat) #, by = c("common", "hemisphere"))

# Remove single hemisphere species
# remove: SBFT in NH, BFT in SH, and SLT in NH
full <- full %>% mutate(SAI = replace(SAI, code == "SBFT" & hemisphere == "North", NA))
full <- full %>% mutate(SI = replace(SI, code == "SBFT" & hemisphere == "North", NA))
full <- full %>% mutate(SAI = replace(SAI, code == "BFT" & hemisphere == "South", NA))
full <- full %>% mutate(SI = replace(SI, code == "BFT" & hemisphere == "South", NA))
full <- full %>% mutate(SAI = replace(SAI, code == "SLT" & hemisphere == "North", NA))
full <- full %>% mutate(SI = replace(SI, code == "SLT" & hemisphere == "North", NA))

# Rename hemispheres
full <- full %>% 
  rename(Hemisphere = hemisphere)
