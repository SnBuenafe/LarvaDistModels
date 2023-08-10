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

#### SPATIAL DISPERSION ####

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
    dplyr::rename(!!sym(paste("ind", seasons[i], sep = "_")) := ind) # Rename
}

# Calculate across seasons
full <- purrr::reduce(df, dplyr::left_join, by = c("species", "hemisphere")) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(ind = mean(c_across(starts_with("ind"))),
                ind_sd = sd(c_across(starts_with("ind"))),
                temp_cv = ind_sd/ind) %>% # Take the mean and SD across species
  dplyr::select(species, hemisphere, ind, ind_sd, temp_cv) %>% 
  dplyr::mutate(code = toupper(species)) %>% 
  dplyr::select(-species) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(hemisphere) %>% 
  dplyr::left_join(., spec_dict, by = "code") %>% 
  dplyr::mutate(common = fct_reorder(common, desc(ind))) %>% 
  dplyr::bind_cols(., )

# Plot points with error bars
ggplot(data = full, aes(x = common, y = ind, color = groups)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = ind-ind_sd, ymax = ind+ind_sd), size = 1) +
  geom_point(aes(y = transformer$scale_func(temp_cv)),
            colour = "#ec7014",
            size = 5,
            shape = 8) +
  scale_y_continuous(
    sec.axis = sec_axis(
      trans = ~ transformer$inv_func(.),
      name = expression('Temporal dispersion')
    )) +
  facet_grid(rows = vars(hemisphere)) +
  scale_color_manual(name = "Taxa grouping",
                    aesthetics = c("color"),
                    values = c("#ABA3D6", "#615A89", "#26223D")
  ) +
  ylab("Spatial dispersion") +
  theme_bw() +
  theme(axis.text.x = element_text(color = "black", size = 17, angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_text(color = "black", size = 19, vjust = 2),
        axis.text.y = element_text(color = "black", size = 18),
        legend.title = element_text(color = "black", size = 18),
        legend.text = element_text(color = "black", size = 15),
        strip.background=element_rect(fill="white"),
        strip.text = element_text(color = "black", size = 20))

ggsave(filename = here::here(figure_dir, "Dispersion_Spatial.png"), dpi = 600, width = 20, height = 8, units = "in")












# Transform data for dual-y axes
# Written by Dave S (david.schoeman@gmail.com)
# Transformer function based on: https://www.r-bloggers.com/2022/12/how-to-make-a-plot-with-two-different-y-axis-in-r-with-ggplot2-a-secret-ggplot2-hack/
# There were many errors that had to be fixed!
transformer_dual_Y_axis <- function (data,
                                     primary_column, secondary_column,
                                     include_y_zero = FALSE) {
  # PARAMETER SETUP 
  params_tbl <- data %>%
    summarise(
      max_primary = max (!! enquo (primary_column)),
      min_primary = min (!! enquo (primary_column)),
      max_secondary = max(!! enquo (secondary_column)),
      min_secondary = min(!! enquo (secondary_column))
    )
  
  if (include_y_zero) {
    params_tbl$min_primary <- 0
    params_tbl$min_secondary <- 0
  }
  
  params_tbl <- params_tbl  %>% 
    mutate(
      scale = (max_primary - min_primary) / (max_secondary - min_secondary), #b
      shift = min_primary - scale * min_secondary #a
    )
  
  # MAKE SCALER FUNCTIONS
  scale_func <- function (x) {
    params_tbl$shift + (x * params_tbl$scale)
  }
  inv_func <- function(x) {
    (x - params_tbl$shift) / params_tbl$scale
  }
  
  # RETURN
  ret <- list(
    scale_func = scale_func,
    inv_func = inv_func,
    params_tbl = params_tbl
  )
  
  return (ret)
}  

# Transform data
transformer <- full %>% 
  ungroup() %>% 
  transformer_dual_Y_axis(ind, temp_cv, FALSE)
