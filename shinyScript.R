# Define directories
preds_dir <- here::here("Output", "Predictions")

# Define preliminaries
source("00_SetupGrid.R")
source(here::here("Utils", "plotPC.R"))
source(here::here("Utils", "plotHotspot.R"))
pacman::p_load(purrr, Hmisc, RColorBrewer, patchwork, corrplot, cmocean)

spec_dict <- tibble::tribble(
  ~code, ~common,
  "YFT", "Yellowfin tuna",
  "SKP", "Skipjack tuna",
  "ALB", "Albacore",
  "SWO", "Swordfish",
  "BLUM", "Blue marlin",
  "FRI", "Frigate tuna",
  "BET", "Bigeye tuna",
  "BFT", "P. bluefin tuna",
  "SAU", "Sauries",
  "SAIL", "Sailfish",
  "SBFT", "S. bluefin tuna",
  "SLT", "Slender tuna",
  "BON", "Bonitos",
  "SHOS", "Shortbill spearfish",
  "STRM", "Striped marlin",
  "LESC", "Longfin escolar",
  "LIT", "Little tuna"
)
seas_dict <- c("jan-mar", "apr-jun", "jul-sept", "oct-dec")

#### 1. ASSEMBLE CHOSEN SPECIES ####
fAssemblePredictions <- function(spp, # vector of species
                                 season # chosen season
) {
  
  full <- list()
  
  for(i in 1:length(spp)) {
    full[[i]] <- readRDS(here::here(preds_dir, base::paste0(spp[i], "_", season, ".rds"))) %>% 
      dplyr::as_tibble() %>% 
      dplyr::select(cellID, grid_100_category, model, geometry) %>% 
      dplyr::rename(!!sym(tolower(spp[i])) := model)
  }
  
  df <- purrr::reduce(full, 
                      dplyr::left_join, 
                      by = c('cellID', 'grid_100_category', 'geometry')) %>% # join all
    dplyr::relocate(cellID) %>%
    dplyr::relocate(grid_100_category, .after = cellID) %>% 
    dplyr::relocate(geometry, .after = last_col()) # arrange columns
  
  return(df)

}

spp <- c("YFT", "SKP") # species selected
season <- seas_dict[1]
spec_subs_jm <- fAssemblePredictions(spp, season) # subset of species
  
# We repeat this for all the other seasons as well
rep <- c("spec_subs_aj", "spec_subs_js", "spec_subs_od")
for(i in 1:length(rep)) {
  season <- seas_dict[i+1]
  x <- fAssemblePredictions(spp, season) # subset of species
  assign(rep[i], x)
  
  # lgrid <- spec_subs_jm
  # dplyr::select(grid_100_category) %>% 
  #   pull() %>% 
  #   unique() # get unique 10x10 IDs that were included in this season
}

#### 2. RESTRICT AREA ####

fRestrictArea <- function(df, # combined data frame with cellID and geometry
                          bounds # can only be from 40N-40S (-40 to 40) and from -140W-110E (-140 to 110)
) {
  
  sf <- df %>% 
    sf::st_as_sf(crs = cCRS) # convert into an sf object
  
  boundary <- SpatPlan_Get_Boundary(Limits = c(xmin = bounds[["xmin"]],
                                              xmax = bounds[["xmax"]],
                                              ymin = bounds[["ymin"]],
                                              ymax = bounds[["ymax"]]),
                                   cCRS = cCRS)
  
  # First get all the PUs partially/wholly within the planning region
  logi_Reg <- sf::st_centroid(sf) %>%
    sf::st_intersects(boundary) %>%
    lengths > 0 # Get logical vector instead of sparse geometry binary
  
  sf <- sf[logi_Reg, ] # Get TRUE
  
  return(sf)
}

restricted_jm <- fRestrictArea(spec_subs_jm, bounds = c(ymax = 10, ymin = -20, xmax = 10, xmin = -170))

# And repeat this for the rest of the seasons
restricted_aj <- fRestrictArea(spec_subs_aj, bounds = c(ymax = 10, ymin = -20, xmax = 10, xmin = -170))
restricted_js <- fRestrictArea(spec_subs_js, bounds = c(ymax = 10, ymin = -20, xmax = 10, xmin = -170))
restricted_od <- fRestrictArea(spec_subs_od, bounds = c(ymax = 10, ymin = -20, xmax = 10, xmin = -170))

#### 3. PERFORM PCA ####
fRunPCA <- function(df,
                    spp # list of species
                    ) {
  
  pca_df <- df %>%  # convert sf to a tibble and select model columns
    dplyr::as_tibble() %>% 
    dplyr::select(all_of(tolower(spp)))
  
  pca <- stats::princomp(pca_df, cor = FALSE)
  
  scores <- pca$scores[,1:2] %>% 
    tibble::as_tibble() %>% 
    dplyr::rename(PCA_1 = Comp.1,
                  PCA_2 = Comp.2) %>% 
    dplyr::bind_cols(., df) %>% 
    dplyr::relocate(cellID) # arrange columns
  
  return(scores)
}

pc_jm <- fRunPCA(restricted_jm, spp) # January-March
pc_aj <- fRunPCA(restricted_aj, spp) # April-June
pc_js <- fRunPCA(restricted_js, spp) # July-September
pc_od <- fRunPCA(restricted_od, spp) # October-December

#### 4. Correlation matrix with PC axis ####
# Pearson's correlation
fPerformPearson <- function(df,
                            axis,
                            spp) {
  mat <- df %>% 
    dplyr::select(axis, all_of(tolower(spp))) %>% 
    base::as.matrix()
  
  res <- (Hmisc::rcorr(mat))$r
  
  res <- res[1, order(res[1,], decreasing = TRUE)] %>%   # arrange columns according to their r values
    as.matrix() %>% 
    t()
  
  colnames(res) <- c("PCA 1", 
                     spec_dict[match(spp, spec_dict$code), "common"] %>% 
                       pull())
  
  rownames(res) <- ""
  
  corrplot::corrplot(res,
                     # type = "upper",
                     order = "original",
                     tl.col = "black",
                     # addCoef.col = "black",
                     tl.srt = 45,
                     insig = "blank", # make r values that have p values < 0.05 blank
                     col = corrplot::COL2('BrBG', 200),
                     diag = TRUE,
                     col.lim = c(-1, 1), # dictate limits of colors
                     # addgrid.col = NA # remove the grid
  )
  
}

fPerformPearson(pc_jm, "PCA_1", spp) # January-March
fPerformPearson(pc_aj, "PCA_1", spp) # April-June
fPerformPearson(pc_js, "PCA_1", spp) # July-September
fPerformPearson(pc_od, "PCA_1", spp) # October-December

#### 5. PLOT PCA ####
# January-March
ggpc <- pc_jm %>% 
  sf::st_as_sf(crs = cCRS)
pc1 <- plotPC(ggpc, "PCA_1", "PC1 score")
pc2 <- plotPC(ggpc, "PCA_2", "PC2 score")
pc1 + pc2 +
  plot_annotation(title = "January-March")

# April-June
ggpc <- pc_aj %>% 
  sf::st_as_sf(crs = cCRS)
pc1 <- plotPC(ggpc, "PCA_1", "PC1 score")
pc2 <- plotPC(ggpc, "PCA_2", "PC2 score")
pc1 + pc2 +
  plot_annotation(title = "April-June")

# July-September
ggpc <- pc_js %>% 
  sf::st_as_sf(crs = cCRS)
pc1 <- plotPC(ggpc, "PCA_1", "PC1 score")
pc2 <- plotPC(ggpc, "PCA_2", "PC2 score")
pc1 + pc2 +
  plot_annotation(title = "July-September")

# October-December
ggpc <- pc_od %>% 
  sf::st_as_sf(crs = cCRS)
pc1 <- plotPC(ggpc, "PCA_1", "PC1 score")
pc2 <- plotPC(ggpc, "PCA_2", "PC2 score")
pc1 + pc2 +
  plot_annotation(title = "October-December")

#### 6. HEATMAP PER SEASON ####
fDetermineHotspots <- function(df,
                               spp,
                               season
                               ) {
  
  spec <- base::tolower(spp)
  
  full <- df %>% 
    dplyr::as_tibble()
  
  dum_list <- list()
  
  for(i in 1:length(spec)) {
    dum_list[[i]] <- full %>% 
      dplyr::mutate(!!sym(spec[i]) := case_when(!!sym(spec[i]) >= median(full[[spec[i]]], na.rm = TRUE) ~ 1, 
                                               !!sym(spec[i]) < median(full[[spec[i]]], na.rm = TRUE) ~ 0,
                                               is.na(!!sym(spec[i])) ~ NA)) %>% 
      dplyr::select(cellID, grid_100_category, geometry, !!sym(spec[i]))
  }
  
  bind_df <- purrr::reduce(dum_list, dplyr::left_join) %>% 
    sf::st_as_sf(crs = cCRS) %>% 
    rowwise() %>% 
    dplyr::mutate(sum = sum(c_across(4:ncol(.)), na.rm = TRUE)) %>% 
    dplyr::mutate(count = sum(is.na(c_across(4:ncol(.))))) %>% # count the number of NAs (should be the same for all species)
    dplyr::mutate(count = ifelse(count > 0, yes = 1, no = 0)) %>% # count number of NAs
    dplyr::select(cellID, grid_100_category, sum, count, geometry) %>% 
    dplyr::rename(!!sym(season) := sum) %>% 
    dplyr::rename(!!sym(paste("count", season, sep = "_")) := count) %>% 
    ungroup() %>% 
    dplyr::as_tibble()
}

heatmap_jm <- fDetermineHotspots(restricted_jm, spp, "jan-mar")
gghm_jm <- plotHotspot(heatmap_jm, "jan-mar", "January-March")

# Repeat the rest for the other seasons
heatmap_aj <- fDetermineHotspots(restricted_aj, spp, "apr-jun")
gghm_aj <- plotHotspot(heatmap_aj, "apr-jun", "April-June")

heatmap_js <- fDetermineHotspots(restricted_js, spp, "jul-sept")
gghm_js <- plotHotspot(heatmap_js, "jul-sept", "July-September")

heatmap_od <- fDetermineHotspots(restricted_od, spp, "oct-dec")
gghm_od <- plotHotspot(heatmap_od, "oct-dec", "October-December")

#### 7. SUMMARY HOTSPOTS ####
fPrepareSumHotspots <- function(jm, # Jan-March heatmap
                                aj, # Apr-June heatmap
                                js, # July-September heatmap
                                od # October-December heatmap
                                ) {
  
  # Take the union of grid_100 cells that were selected at least once
  rep <- list(jm, aj, js, od)
  filt <- list() # empty list
  
  for(i in 1:length(rep)) {
    filt[[i]] <- rep[[i]] %>% 
      dplyr::select(grid_100_category) %>% 
      unique()
  }
  
  filt <- purrr::reduce(filt, dplyr::bind_rows) %>% 
    unique() %>% 
    pull()
  
  seas_list <- c("jan-mar", "apr-jun", "jul-sept", "oct-dec")
  new_names <- c("jm", "aj", "js", "od")
  
  df <- purrr::reduce(rep, dplyr::full_join) %>% 
    dplyr::relocate(geometry, .after = last_col()) %>% 
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
  
  quant <- quantile(df$gm, c(0.5, 0.8, 0.95), na.rm = TRUE) # get quantiles for categories
  
  df %<>%
    dplyr::mutate(hotspot_cat = case_when(gm <= quant[[1]] ~ "low",
                                          (gm > quant[[1]] & gm <= quant[[2]]) ~ "mid",
                                          (gm > quant[[2]] & gm <= quant[[3]]) ~ "high",
                                          (gm > quant[[3]] ~ "core"))) %>% # assign hotspot categories
    dplyr::mutate(hotspot_cat = fct_relevel(hotspot_cat, c("low", "mid", "high", "core"))) %>%
    sf::st_as_sf(crs = cCRS)
  
  return(df)
}

full <- fPrepareSumHotspots(heatmap_jm, heatmap_aj, heatmap_js, heatmap_od)
heatmap_sum <- plotHotspotSummary(full)
