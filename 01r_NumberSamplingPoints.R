library(tidyverse)
library(sf)
library(purrr)

dir <- here::here("Data", "Fish")
fig_dir <- here::here("Figures")

# Get the frequency of CPUE categories per season per species
file_list <- list.files(dir)
idx <- str_detect(file_list, pattern = paste(c("little-tuna", "bonitos", "black-marlin"), collapse = "|"), negate = TRUE) # removing species that we're not interested in
file_list <- file_list[idx]

sum_freq <- list()
for(i in 1:length(file_list)) {
  
  tmp <- readRDS(here::here(dir, file_list[i]))
  
  sum_freq[[i]] <- tmp %>% 
    dplyr::as_tibble() %>% 
    dplyr::group_by(abundance) %>% 
    dplyr::summarise(!!sym(paste0("freq", i)) := n())
  
}

sum_freq <- purrr::reduce(sum_freq, dplyr::left_join, by = "abundance") %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(freq = sum(c_across(starts_with("freq")), na.rm = TRUE)) %>% 
  dplyr::select(abundance, freq)

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
 