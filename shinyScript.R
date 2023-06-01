# Define directories
preds_dir <- here::here("Output", "Predictions")

# Define preliminaries
source("00_SetupGrid.R")
source(here::here("Utils", "plotPC.R"))
pacman::p_load(purrr, Hmisc, RColorBrewer, patchwork)

spec_dict <- c("YFT", "SKP", "ALB", "SWO", "BLUM", "FRI", "BET", "BFT", "SAU", "SAIL", "SBFT",
               "SLT", "BON", "SHOS", "STRM", "LESC", "LIT")
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
  
  res <- res[1,2:ncol(res)] %>% 
    base::as.matrix() %>% 
    t()
  
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
