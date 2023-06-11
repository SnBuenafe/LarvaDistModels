# Define directories
preds_dir <- here::here("Output", "Predictions")

# Define preliminaries
source("00_SetupGrid.R")
source(here::here("Utils", "plotPC.R"))
source(here::here("Utils", "plotHotspot.R"))
utils <- list.files(path = here::here("ShinyUtils"), pattern = "*.R", full.names = TRUE)
sapply(X = utils, FUN = source) %>% invisible()

pacman::p_load(purrr, Hmisc, RColorBrewer, patchwork, corrplot, cmocean) # load additional packages

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
spp <- spec_dict$code # species selected
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
restricted_jm <- fRestrictArea(spec_subs_jm, bounds = c(ymax = 40, ymin = -40, xmax = 40, xmin = -80))

# And repeat this for the rest of the seasons
restricted_aj <- fRestrictArea(spec_subs_aj, bounds = c(ymax = 40, ymin = -40, xmax = 40, xmin = -80))
restricted_js <- fRestrictArea(spec_subs_js, bounds = c(ymax = 40, ymin = -40, xmax = 40, xmin = -80))
restricted_od <- fRestrictArea(spec_subs_od, bounds = c(ymax = 40, ymin = -40, xmax = 40, xmin = -80))

#### 3. PERFORM PCA ####
pc_jm <- fRunPCA(restricted_jm, spp) # January-March
pc_aj <- fRunPCA(restricted_aj, spp) # April-June
pc_js <- fRunPCA(restricted_js, spp) # July-September
pc_od <- fRunPCA(restricted_od, spp) # October-December

#### 4. Correlation matrix with PC axis ####
# Pearson's correlation
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
full <- fPrepareSumHotspots(heatmap_jm, heatmap_aj, heatmap_js, heatmap_od)
heatmap_sum <- plotHotspotSummary(full)
