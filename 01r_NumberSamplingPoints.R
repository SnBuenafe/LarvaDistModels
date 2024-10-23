library(tidyverse)
library(sf)
library(purrr)

dir <- here::here("Data", "Fish")
fig_dir <- here::here("Figures")

# Get the frequency of CPUE categories per season per species
file_list <- list.files(dir)
idx <- str_detect(file_list, pattern = paste(c("little-tuna", "bonitos", "black-marlin"), collapse = "|"), negate = TRUE) # removing species that we're not interested in
file_list <- file_list[idx]

spp <- str_remove_all(file_list, "VectorFile_") %>% 
  str_extract("^[^_]+")

sum_freq_raw <- list()
for(i in 1:length(file_list)) {
  
  tmp <- readRDS(here::here(dir, file_list[i]))
  
  sum_freq_raw[[i]] <- tmp %>% 
    dplyr::as_tibble() %>% 
    dplyr::group_by(abundance) %>% 
    dplyr::summarise(freq = n(),
                     species = spp[i])
  
}

sum_freq <- bind_rows(sum_freq_raw) %>% 
  dplyr::summarise(freq = sum(freq), .by = "abundance")


sum(sum_freq$freq) # get total sampling points

ggplot() +
  geom_col(data = sum_freq, aes(x = abundance, y = freq/238950), fill = "lightsalmon2", color = NA) +
  xlab("CPUE category") +
  ylab("Relative frequency") +
  theme_bw() +
  theme(axis.ticks.y = element_blank(),
        axis.title = element_text(size = 15, color = "black"),
        axis.ticks.x = element_line(color = "black"),
        axis.text = element_text(size = 10, color = "black"),
        plot.margin = unit(c(1,0.5,1,0.5), "cm"))

ggsave(filename = here::here(fig_dir, "Supp_NumberSamplePoints.png"),
       dpi = 600,
       width = 7,
       height = 3)  
 


ggplot() +
  geom_col(data = sum_freq, aes(x = abundance, y = freq), fill = "lightsalmon2", color = NA) +
  xlab("CPUE category") +
  ylab("Frequency") +
  theme_bw() +
  scale_y_log10() +
  theme(axis.ticks.y = element_blank(),
        axis.title = element_text(size = 15, color = "black"),
        axis.ticks.x = element_line(color = "black"),
        axis.text = element_text(size = 10, color = "black"),
        plot.margin = unit(c(1,0.5,1,0.5), "cm"))

ggsave(filename = here::here(fig_dir, "Supp_NumberSamplePoints_Absolute.png"),
       dpi = 600,
       width = 7,
       height = 3)  


# Now do species frequency table

spp_freq <- bind_rows(sum_freq_raw) %>% 
  dplyr::summarise(freq = sum(freq), .by = c("abundance", "species")) %>% 
  tidyr::pivot_wider(names_from = "abundance", values_from = "freq")

write_csv(spp_freq, file = file.path("Output", "SpeciesFreqTable.csv"))

