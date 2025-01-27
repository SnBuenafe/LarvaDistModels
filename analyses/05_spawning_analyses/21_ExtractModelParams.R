
library(tidyverse)

output_dir <- here::here("data_output", "CVGrid")

files <- list.files(output_dir, full.names = TRUE)

dat <- purrr::map(files, \(x) readRDS(x) %>% 
                    dplyr::mutate(species = basename(x) %>% str_remove("_CVGrid.rds")) %>% 
                    dplyr::arrange(desc(test_AUC)) %>%  # BEST TEST AUC
                    dplyr::slice_head(n = 1)) %>% 
  dplyr::bind_rows() %>% 
  dplyr::select(species, tidyselect::everything())

write.csv(dat, here::here("data_output", "FinalModelParameters.csv"))
