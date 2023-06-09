# TODO: FIX THIS

# DESCRIPTION: Identify core hotspots across species using the model outputs

source("00_SetupGrid.R")
source("00_Preliminaries.R")
spp <- c("skp", "yft", "alb", "bet", "fri", "sbft", "bft", "lit", "slt", "bon", "blum", "shos", "swo", "strm", "sail", "lesc", "sau")
seas_list <- c("jan-mar", "apr-jun", "jul-sept", "oct-dec")
new_names <- c("jm", "aj", "js", "od")

# Associate grid (1x1) with coarser gridf (10x10)
a_grid <- associateGrids(grid, grid_100) %>% 
  dplyr::as_tibble()

#### Seasonal hotspots ####
jm <- determineHotspots("jan-mar")
aj <- determineHotspots("apr-jun")
js <- determineHotspots("jul-sept")
od <- determineHotspots("oct-dec")

full <- purrr::reduce(list(jm, aj, js, od), dplyr::full_join) %>% 
  dplyr::left_join(a_grid, by = c("cellID", "geometry")) %>% 
  dplyr::mutate(count = rowSums(dplyr::across(tidyselect::starts_with("count_"))))

filt <- full %>%
  dplyr::filter(count != 4) %>%
  dplyr::select(grid_100_category) %>%
  dplyr::distinct() %>%
  dplyr::pull()

full %<>%
  dplyr::filter(grid_100_category %in% filt) %>% 
  dplyr::rename_with(~ new_names, all_of(seas_list)) %>% 
  dplyr::mutate(jm = ifelse(jm == 0, 1, jm),
                aj = ifelse(aj == 0, 1, aj),
                js = ifelse(js == 0, 1, js),
                od = ifelse(od == 0, 1, od)) %>%  # change all 0s to 1s
  dplyr::mutate(gm = (jm * aj * js * od)^(1/(4-count))) %>%  # get the geometric mean only for cells where there are values
  dplyr::select(cellID, gm, all_of(new_names), geometry)
  
quant <- quantile(full$gm, c(0.5, 0.8, 0.95)) # get quantiles for categories

full %<>%
  dplyr::mutate(hotspot_cat = case_when(gm <= quant[[1]] ~ "low",
                                        (gm > quant[[1]] & gm <= quant[[2]]) ~ "mid",
                                        (gm > quant[[2]] & gm <= quant[[3]]) ~ "high",
                                        (gm > quant[[3]] ~ "core"))) %>% # assign hotspot categories
  dplyr::mutate(hotspot_cat = fct_relevel(hotspot_cat, c("low", "mid", "high", "core"))) %>%
  sf::st_as_sf(crs = cCRS)
  
# Plot heatmaps for each season
hm_jm <- plotHotspot(jm, "jan-mar", "January-March")
hm_aj <- plotHotspot(aj, "apr-jun", "April-June")
hm_js <- plotHotspot(js, "jul-sept", "July-September")
hm_od <- plotHotspot(od, "oct-dec", "October-December")
  
hm_sum <- plotHotspotSummary(full)

all <- hm_jm + hm_aj + hm_js + hm_od + hm_sum + plot_spacer() +
  plot_layout(ncol = 2, nrow = 3) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = all, filename = here::here(figure_dir, "Hotspots.png"), width = 20, height = 15, dpi = 300)
