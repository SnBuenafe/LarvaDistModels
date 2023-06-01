# Define directories
preds_dir <- here::here("Output", "Predictions")

# Define preliminaries
source("00_SetupGrid.R")
source(here::here("Utils", "plotPC.R"))
pacman::p_load(purrr, Hmisc, RColorBrewer, patchwork, ggcorrplot)

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
      dplyr::select(cellID, model, geometry) %>% 
      dplyr::rename(!!sym(tolower(spp[i])) := model)
  }
  
  df <- purrr::reduce(full, 
                      dplyr::left_join, 
                      by = c('cellID', 'geometry')) %>% # join all
    dplyr::relocate(cellID) %>% 
    dplyr::relocate(geometry, .after = last_col()) # arrange columns
  
  return(df)

}

spp <- c("YFT", "SKP")
season <- "jan-mar"
tmp <- fAssemblePredictions(spp, season)

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

res <- fRestrictArea(tmp, bounds = c(ymax = 10, ymin = -20, xmax = 10, xmin = -170))

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

pc <- fRunPCA(res, spp)

#### 5. Correlation matrix with PC axis ####
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
                     col = COL2('BrBG', 200),
                     diag = TRUE,
                     col.lim = c(-1, 1), # dictate limits of colors
                     # addgrid.col = NA # remove the grid
  )
  
}
fPerformPearson(pc, "PCA_1", spp)

#### 4. PLOT PCA ####
ggpc <- pc %>% 
  sf::st_as_sf(crs = cCRS)
pc1 <- plotPC(ggpc, "PCA_1", "PC1 score")
pc2 <- plotPC(ggpc, "PCA_2", "PC2 score")

#### 5. HOTSPOTS ####
fDetermineHotspots <- function(df,
                               spp
                               ) {
  
  
  
  dum_list <- list()
  
  for(i in 1:length(spp)) {
    dum_list[[i]] <- full %>% 
      dplyr::mutate(!!sym(spp[i]) := case_when(!!sym(spp[i]) >= median(full[[spp[i]]], na.rm = TRUE) ~ 1, 
                                               !!sym(spp[i]) < median(full[[spp[i]]], na.rm = TRUE) ~ 0,
                                               is.na(!!sym(spp[i])) ~ NA)) %>% 
      dplyr::select(cellID, grid_100_category, geometry, !!sym(spp[i]))
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
