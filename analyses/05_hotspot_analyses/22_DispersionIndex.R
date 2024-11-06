# DESCRIPTION: Calculating the index of dispersion (Coefficient of Variation - CV)
source("00_Preliminaries.R")
source(here::here("Utils", "fxnshemisphere.R"))
pacman::p_load(purrr, magrittr)
seasons <- c("jan-mar", "apr-jun", "jul-sept", "oct-dec")

# Add the groups
spec_dict
groups <- c(rep("Tunas", 3), rep("Billfish", 2), rep("Tunas", 3), "Others", "Billfish", rep("Tunas", 3), rep("Billfish", 2), "Others", "Tunas")
spec_dict %<>%
  dplyr::bind_cols(., groups = groups) %>% 
  dplyr::mutate(groups = factor(groups,  levels = c("Tunas", "Billfish", "Others"))) %>% 
  dplyr::filter(!code %in% c("BON", "LIT"))

# Define the index of dispersion as CV
index <- function(x) {
  ind <- sd(x)/(mean(x))
}

# Prepare data
df <- list()
for(i in 1:length(seasons)) {
  # Load file
  tmp <- terra::rast(here::here(rast_dir, paste("ModelOutputs", paste0(seasons[i], ".tif"), sep = "_"))) %>% 
    as.data.frame(xy = TRUE) %>% # get longitude and latitudes
    dplyr::as_tibble() %>% 
    dplyr::mutate(hemisphere = ifelse(y >= 0, "North", "South")) # separate them per hemisphere
              
  # Repeat for all 4 seasons            
  df[[i]] <- tmp %>% 
    dplyr::group_by(hemisphere) %>% 
    dplyr::summarise(across(!c(x, y), index)) %>% # Calculate the index
    tidyr::pivot_longer(cols = !c(hemisphere),
                        names_to = "species", 
                        values_to = "ind") %>% 
    #dplyr::arrange(desc(ind)) %>% 
    dplyr::rename(!!sym(paste("ind", seasons[i], sep = "_")) := ind) # Rename columns
}

# Calculate across seasons
full <- purrr::reduce(df, dplyr::left_join, by = c("species", "hemisphere")) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(ind = mean(c_across(starts_with("ind"))),
                ind_sd = sd(c_across(starts_with("ind"))),
                temp_cv = ind_sd) %>% # Take the SD across seasons per species (temporal dispersion)
  dplyr::select(species, hemisphere, ind, ind_sd, temp_cv) %>% 
  dplyr::mutate(code = toupper(species)) %>% 
  dplyr::select(-species) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(hemisphere) %>% 
  dplyr::left_join(., spec_dict, by = "code") %>% 
  dplyr::mutate(common = fct_reorder(common, desc(ind))) %>% # We want to plot it with the common names
  dplyr::bind_cols(., )

# Transform data for the secondary axis (temporal dispersion)
# transformer <- full %>% 
#   ungroup() %>% 
#   transformer_dual_Y_axis(ind, temp_cv, FALSE)

# Plot mean index across seasons per species (spatial dispersion) and the CV of the indices per season per species (temporal dispersion)
ggplot(data = full, aes(x = common, y = ind)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = ind-ind_sd, ymax = ind+ind_sd), linewidth = 0.5) +
  # geom_point(aes(y = transformer$scale_func(temp_cv)),
  #           colour = "#827CBD",
  #           size = 3,
  #           shape = 8) +
  coord_fixed(ratio = 1.3) +
  # scale_y_continuous(
  #   sec.axis = sec_axis(
  #     trans = ~ transformer$inv_func(.),
  #     name = expression("Temporal dispersion")
  #   )) +
  facet_grid(rows = vars(hemisphere)) +
  # scale_color_manual(name = "Taxa grouping",
  #                   aesthetics = c("color"),
  #                   values = c("#ABA3D6", "#615A89", "#26223D")
  # ) +
  ylab("Index of dispersion") +
  theme_bw() +
  theme(axis.text.x = element_text(color = "black", size = 12, angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_text(color = "black", size = 16, vjust = 2),
        #axis.title.y.right = element_text(color = "#827CBD", size = 19, vjust = 2),
        axis.text.y = element_text(color = "black", size = 12),
        #axis.text.y.right = element_text(colour = "#827CBD"),
        legend.title = element_text(color = "black", size = 18),
        legend.text = element_text(color = "black", size = 15),
        strip.background = element_blank(),
        strip.text = element_blank(),
        legend.position = "bottom")

ggsave(filename = here::here(figure_dir, "Dispersion.png"), dpi = 600, width = 15, height = 8, units = "in")

# Printing for interpretation
full %>% 
  dplyr::filter(hemisphere == "North") %>% 
  dplyr::arrange(desc(temp_cv)) %>% 
  print(n = 5)
