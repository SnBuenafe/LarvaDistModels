# DESCRIPTION: Identify core hotspots across species using the model outputs

source(file.path("analyses", "02_preliminaries", "00_SetupGrid.R"))
source(file.path("analyses", "02_preliminaries", "00_Preliminaries.R"))
seas_list <- c("jan-mar", "apr-jun", "jul-sept", "oct-dec")
new_names <- c("jm", "aj", "js", "od")
figure_dir <- here::here(figure_dir, "taxa_richness")

#### Associate grid (1x1) with coarser grid (10x10) ####
a_grid <- associateGrids(grid, grid_100) %>% 
  dplyr::as_tibble()

spp_list <- spec_dict %>% 
  dplyr::filter(!code %in% c("LIT", "BON")) %>% 
  dplyr::filter(!code %in% "SAU") %>%  # Temporarily removed while we sort out the BRT
  dplyr::pull(code) %>% 
  tolower()

#### Calculate hotspot metric ####
df <- list() # empty list
for(i in 1:length(seas_list)) {
  df[[i]] <- determineHotspots(seas_list[i], spp_list)
}

full <- purrr::reduce(df, dplyr::full_join) %>% # join all seasons together
  dplyr::left_join(a_grid, by = c("cellID", "geometry")) %>% 
  dplyr::mutate(count = rowSums(dplyr::across(tidyselect::starts_with("count_")))) # get the total number of NAs per season

filt <- full %>%
  dplyr::filter(count != 4) %>% # filter rows that do not have NAs for all 4 seasons
  dplyr::select(grid_100_category) %>%
  dplyr::distinct() %>% # get unique grid_100_category values
  dplyr::pull()

full %<>%
  dplyr::filter(grid_100_category %in% filt) %>% # take only grid cells that do not have NAs for all seasons
  dplyr::rename_with(~ new_names, all_of(seas_list)) %>% 
  dplyr::mutate(jm = ifelse(jm == 0, 1, jm),
                aj = ifelse(aj == 0, 1, aj),
                js = ifelse(js == 0, 1, js),
                od = ifelse(od == 0, 1, od)) %>%  # change cells without any species to a value of 1 because that's not going to affect the geometric mean anyway
  dplyr::mutate(gm = (jm * aj * js * od)^(1/(4-count))) %>%  # get the geometric mean only for cells where there are values
  dplyr::select(cellID, gm, all_of(new_names), geometry)
  
#### Assign categories ####
quant <- quantile(full$gm, c(0.5, 0.8, 0.95)) # get quantiles for categories

fin <- full %>% 
  dplyr::mutate(hotspot_cat = case_when(gm <= quant[[1]] ~ "4",
                                        (gm > quant[[1]] & gm <= quant[[2]]) ~ "3",
                                        (gm > quant[[2]] & gm <= quant[[3]]) ~ "2",
                                        (gm > quant[[3]] ~ "1"))) %>% # assign hotspot categories
  dplyr::mutate(hotspot_cat = fct_relevel(hotspot_cat, c("1", "2", "3", "4"))) %>%
  sf::st_as_sf(crs = cCRS)
  
#### Plotting maps ####
# Seasonal maps
for(i in 1:length(seas_list)) {
  obj <- plotHotspot(df[[i]], seas_list[i])
  
  ggsave(plot = obj, filename = here::here(figure_dir, paste("TaxaRichness", paste0(seas_list[i], ".png"), sep = "_")), width = 14, height = 5, dpi = 600)
}

# Summary map
sum_map <- plotHotspotSummary(fin)
ggsave(plot = sum_map, filename = here::here(figure_dir, paste("TaxaRichness", "SummaryHotspots.png", sep = "_")), width = 14, height = 5, dpi = 600)

