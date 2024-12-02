# DESCRIPTION: Calculating the index of dispersion (Coefficient of Variation - CV)
source(file.path("analyses", "02_preliminaries", "00_Preliminaries.R"))
# source(here::here("Utils", "fxnshemisphere.R"))

pacman::p_load(purrr, magrittr, ggrepel)

seasons <- c("jan-mar", "apr-jun", "jul-sep", "oct-dec")

# Add the groups
spec_dict
groups <- c(rep("Tunas", 3), rep("Billfish", 2), rep("Tunas", 3), "Others", "Billfish", rep("Tunas", 3), rep("Billfish", 2), "Others", "Tunas")
spec_dict %<>%
  dplyr::bind_cols(., groups = groups) %>% 
  dplyr::mutate(groups = factor(groups,  levels = c("Tunas", "Billfish", "Others"))) %>% 
  dplyr::filter(!code %in% c("BON", "LIT"))

# Define the Spatial Aggregation Index (SAI, based on CV)
index <- function(x) {
  ind <- sd(x)/(mean(x))
}

# Prepare data
df_SAI <- list()
for(i in 1:length(seasons)) {
  # Load file
  tmp <- terra::rast(here::here(rast_dir, paste("ModelOutputs", paste0(seasons[i], ".tif"), sep = "_"))) %>% 
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
  dplyr::mutate(SAI = mean(c_across(starts_with("ind")))) %>% # SAI Annual = Mean of SAI across Seasons
  dplyr::select(species, hemisphere, SAI) %>% 
  dplyr::mutate(code = toupper(species)) %>% 
  dplyr::select(-species) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(hemisphere) %>% 
  dplyr::left_join(., spec_dict, by = "code") %>% 
  dplyr::mutate(common = fct_reorder(common, desc(SAI))) %>% # We want to plot it with the common names
  dplyr::bind_cols(., )

# Calculate Seasonality Index (SI, aka Reviewer #2)
df_SI <- list()
for(i in 1:length(seasons)) {
  # Load file
  tmp <- terra::rast(here::here(rast_dir, paste("ModelOutputs", paste0(seasons[i], ".tif"), sep = "_"))) %>% 
    as.data.frame(xy = TRUE) %>% # get longitude and latitudes
    dplyr::as_tibble() %>% 
    dplyr::mutate(hemisphere = ifelse(y >= 0, "North", "South")) # separate them per hemisphere
  
  # Repeat for all 4 seasons            
    df_SI[[i]] <- tmp %>% 
    dplyr::group_by(hemisphere) %>% 
    dplyr::summarise(across(!c(x, y), mean)) %>% # Calculate the index
    tidyr::pivot_longer(cols = !c(hemisphere),
                        names_to = "species", 
                        values_to = "ind") %>% 
    dplyr::rename(!!sym(paste("ind", seasons[i], sep = "_")) := ind) # Rename columns
}

# Calculate across seasons
full_SI <- purrr::reduce(df_SI, dplyr::left_join, by = c("species", "hemisphere")) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(SI = sd(c_across(starts_with("ind")))) %>% # Take the SD across seasons for mean per species (Seasonality Index)
  dplyr::select(species, hemisphere, SI) %>% 
  dplyr::mutate(code = toupper(species)) %>% 
  dplyr::select(-species) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(hemisphere) %>% 
  dplyr::left_join(., spec_dict, by = "code") %>% 
  dplyr::mutate(common = fct_reorder(common, desc(SI))) %>% # We want to plot it with the common names
  dplyr::bind_cols(., )

# Join SAI and SI in one df
full <- left_join(full_SAI, full_SI) #, by = c("common", "hemisphere"))

# Remove single hemisphere species
# SBFT in NH
# BFT in SH
# SLT in NH
full <- full %>% mutate(SAI = replace(SAI, code == "SBFT" & hemisphere == "North", NA))
full <- full %>% mutate(SI = replace(SI, code == "SBFT" & hemisphere == "North", NA))
full <- full %>% mutate(SAI = replace(SAI, code == "BFT" & hemisphere == "South", NA))
full <- full %>% mutate(SI = replace(SI, code == "BFT" & hemisphere == "South", NA))
full <- full %>% mutate(SAI = replace(SAI, code == "SLT" & hemisphere == "North", NA))
full <- full %>% mutate(SI = replace(SI, code == "SLT" & hemisphere == "North", NA))


# Plot Spatial Aggregation Index (full_SAI) vs Seasonality Index (full_SI)
# Not sure how I change colours now
ggplot(data = full, aes(x = SAI, y = SI, shape = hemisphere, label = code)) +
  geom_point(size = 1.5) +
  xlab("Spatial Aggregation Index") +
  ylab("Seasonality Index") +
  geom_text_repel() +
  # scale_color_manual(values = c("North" = "red", "South" = "blue")) +
  scale_shape_manual(values = c(1, 16)) +
  labs(shape = "Hemisphere") + 
  theme_bw()

ggsave(filename = here::here(figure_dir, "Dispersion.png"), dpi = 600, width = 15, height = 8, units = "in")

# Printing for interpretation
full %>% 
  dplyr::filter(hemisphere == "North") %>% 
  dplyr::arrange(desc(temp_cv)) %>% 
  print(n = 5)
