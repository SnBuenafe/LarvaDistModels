# DESCRIPTION: Calculating the index of dispersion (Coefficient of Variation - CV)
source("00_Preliminaries.R")
library(purrr)
library(magrittr)
seasons <- c("jan-mar", "apr-jun", "jul-sept", "oct-dec")

# Add the groups
spec_dict
groups <- c(rep("Tunas", 3), rep("Billfish", 2), rep("Tunas", 3), "Others", "Billfish", rep("Tunas", 3), rep("Billfish", 2), "Others", "Tunas")
spec_dict %<>%
  dplyr::bind_cols(., groups = groups) %>% 
  dplyr::mutate(groups = factor(groups,  levels = c("Tunas", "Billfish", "Others")))

# Define the index of dispersion as CV
index <- function(x) {
  ind <- sd(x)/(mean(x))
}

df <- list()
for(i in 1:length(seasons)) {
  # Load file
  tmp <- rast(here::here("Output", "FinalRaster", 
                         paste0("ModelOutputs_", seasons[i], ".tif"))) %>% 
                dplyr::as_tibble()
              
  # Repeat for all 4 seasons            
  df[[i]] <- tmp %>% 
    dplyr::summarise_all(index) %>% # Calculate the index
    tidyr::pivot_longer(cols = everything(),
                        names_to = "species", 
                        values_to = "ind") %>% 
    #dplyr::arrange(desc(ind)) %>% 
    dplyr::rename(!!sym(paste("ind", seasons[i], sep = "_")) := ind) # Rename

}

# Calculate across seasons
full <- purrr::reduce(df, dplyr::left_join, by = c("species")) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(ind = mean(c_across(starts_with("ind"))),
                ind_sd = sd(c_across(starts_with("ind")))) %>% # Take the mean and SD across species
  dplyr::select(species, ind, ind_sd) %>% 
  dplyr::mutate(code = toupper(species)) %>% 
  dplyr::select(-species) %>% 
  dplyr::ungroup() %>% 
  dplyr::left_join(., spec_dict, by = "code") %>% 
  dplyr::mutate(common = fct_reorder(common, desc(ind))) %>% 
  dplyr::bind_cols(., )

# Plot points with error bars
ggplot(data = full, aes(x = common, y = ind, color = groups)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = ind-ind_sd, ymax = ind+ind_sd), size = 1) +
  scale_color_manual(name = "Taxa grouping",
                    aesthetics = c("color"),
                    values = c("#EB9C52", "#379FDB", "#6554B3")
  ) +
  ylab("Measure of dispersion") +
  theme_bw() +
  theme(axis.text.x = element_text(color = "black", size = 18, angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_text(color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 18),
        legend.title = element_text(color = "black", size = 18),
        legend.text = element_text(color = "black", size = 15))

ggsave(filename = here::here(figure_dir, "Dispersion.png"), dpi = 600, width = 12, height = 7)
