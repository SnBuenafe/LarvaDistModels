# DESCRIPTION: Identify core hotspots across species using the model outputs

source("00_SetupGrid.R")
source("00_Preliminaries.R")
spp <- c("skp", "yft", "alb", "bet", "fri", "sbft", "bft", "lit", "slt", "bon", "blum", "shos", "swo", "strm", "sail", "lesc", "sau")

# Take the union of grid_100 cells that were selected at least once
filt1 <- readRDS(here::here(preds_dir, paste("YFT", "jan-mar.rds", sep = "_"))) %>% 
  dplyr::as_tibble() %>% 
  dplyr::select(grid_100_category) %>% 
  unique()
filt2 <- readRDS(here::here(preds_dir, paste("YFT", "apr-jun.rds", sep = "_"))) %>% 
  dplyr::as_tibble() %>% 
  dplyr::select(grid_100_category) %>% 
  unique()
filt3 <- readRDS(here::here(preds_dir, paste("YFT", "jul-sept.rds", sep = "_"))) %>% 
  dplyr::as_tibble() %>% 
  dplyr::select(grid_100_category) %>% 
  unique()
filt4 <- readRDS(here::here(preds_dir, paste("YFT", "oct-dec.rds", sep = "_"))) %>% 
  dplyr::as_tibble() %>% 
  dplyr::select(grid_100_category) %>% 
  unique()
filt <- purrr::reduce(list(filt1, filt2, filt3, filt4), dplyr::bind_rows) %>% 
  unique() %>% 
  pull()


# Load dummy data to match coordinates and cellIDs
sf <- combineFish(species = "yellowfin-tuna") %>% 
  fSpatPlan_Convert2PacificCentered(., cCRS = cCRS) %>% 
  sf::st_centroid() # transform into point data
dummy <- assembleGrid(grid, sf %>% dplyr::filter(season == "jan-mar")) %>%  # season here doesn't really matter; it will produce the coordinates across the entire region
  associateGrids(., grid_100)

#### Generate heatmaps ####
jm <- determineHotspots("jan-mar")
aj <- determineHotspots("apr-jun")
js <- determineHotspots("jul-sept")
od <- determineHotspots("oct-dec")

seas_list <- c("jan-mar", "apr-jun", "jul-sept", "oct-dec")
new_names <- c("jm", "aj", "js", "od")

full <- purrr::reduce(list(jm, aj, js, od), dplyr::full_join) %>% 
  dplyr::rename_with(~ new_names, all_of(seas_list)) %>% 
  dplyr::mutate(count = rowSums(across(starts_with("count_")))) %>% 
  dplyr::mutate(jm = ifelse(jm == 0, 1, jm),
                aj = ifelse(aj == 0, 1, aj),
                js = ifelse(js == 0, 1, js),
                od = ifelse(od == 0, 1, od)) %>%  # change all 0s to 1s
  dplyr::rowwise() %>% 
  dplyr::mutate(gm = ifelse(count == 4, yes = NA,
                            no = (jm * aj * js * od)^(1/(4-count)))) %>%  # get the geometric mean only for cells where there are values
  dplyr::select(cellID, grid_100_category, gm, all_of(new_names), geometry) %>% 
  dplyr::filter(grid_100_category %in% filt) # make sure we only have cells that were filtered at least in one season
  
quant <- quantile(full$gm, c(0.5, 0.8, 0.95)) # get quantiles for categories

full %<>%
  dplyr::mutate(hotspot_cat = case_when(gm <= quant[[1]] ~ "low",
                                        (gm > quant[[1]] & gm <= quant[[2]]) ~ "mid",
                                        (gm > quant[[2]] & gm <= quant[[3]]) ~ "high",
                                        (gm > quant[[3]] ~ "core"))) %>% # assign hotspot categories
  dplyr::mutate(hotspot_cat = fct_relevel(hotspot_cat, c("low", "mid", "high", "core"))) %>%
  sf::st_as_sf(crs = cCRS)
  
# Plot heatmaps for each season
hm_jm <- plotHotspot(jm %>% 
                       dplyr::filter(grid_100_category %in% (filt1 %>% pull())), "jan-mar", "January-March")
hm_aj <- plotHotspot(aj %>% 
                       dplyr::filter(grid_100_category %in% (filt2 %>% pull())), "apr-jun", "April-June")
hm_js <- plotHotspot(js %>% 
                       dplyr::filter(grid_100_category %in% (filt3 %>% pull())), "jul-sept", "July-September")
hm_od <- plotHotspot(od %>% 
                       dplyr::filter(grid_100_category %in% (filt4 %>% pull())), "oct-dec", "October-December")
  
hm_sum <- plotHotspotSummary(full)

all <- hm_jm + hm_aj + hm_js + hm_od + hm_sum + plot_spacer() +
  plot_layout(ncol = 2, nrow = 3) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = all, filename = here::here(figure_dir, "Hotspots.png"), width = 20, height = 15, dpi = 300)
