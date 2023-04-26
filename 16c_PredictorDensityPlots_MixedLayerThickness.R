# DESCRIPTION: Density plots for mixed layer depth vs predictions

# Function for plotting mixed layer depth vs probability
plot_pred_spp <- function(predictor, spp, color, title) {
  gg <- ggplot(dataTmp) +
    geom_point(aes_string(x = predictor, y = spp), color = color, size = 0.5, shape = 16) +
    theme_bw() +
    ggtitle(title) +
    xlab(expression('Mixed layer thickness (m)')) +
    ylab("Probability") +
    theme(axis.text = element_text(size = 15, color = "black"),
          axis.ticks = element_line(linewidth = 2, color = "black"),
          axis.title = element_blank(),
          plot.title = element_text(size = 20, color = "black"))
}

### January-March ####

# Load full prediction data set
df <- read_csv("Output/CSV/FULL_predictions_jan-mar.csv") %>% 
  dplyr::select(-1)

# Load predictor data set
dataTmp <- readRDS("Data/Climatology/sf/mlotst_historical_jan-mar_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor() %>%  # load temperature
  tibble::as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  bind_cols(., df)

gg_yft <- plot_pred_spp("mlotst_transformed", "yft", "#708B75", "(a) Yellowfin tuna")
gg_skp <- plot_pred_spp("mlotst_transformed", "skp", "#88A88E", "(b) Skipjack tuna")
gg_alb <- plot_pred_spp("mlotst_transformed", "alb", "#9DC2A4", "(c) Albacore")
gg_fri <- plot_pred_spp("mlotst_transformed", "fri", "#B2DBB9", "(d) Frigate tuna")
gg_bet <- plot_pred_spp("mlotst_transformed", "bet", "#C2F0CA", "(e) Bigeye tuna")
gg_swo <- plot_pred_spp("mlotst_transformed", "swo", "#6A637A", "(f) Swordfish")
gg_blum <- plot_pred_spp("mlotst_transformed", "blum", "#9086A6", "(g) Blue marlin")
gg_shos <- plot_pred_spp("mlotst_transformed", "shos", "#A297BA", "(h) Shortbill spearfish")
gg_strm <- plot_pred_spp("mlotst_transformed", "strm", "#B8ABD4", "(i) Striped marlin")
gg_sail <- plot_pred_spp("mlotst_transformed", "sail", "#CABCE8", "(j) Sailfish")
gg_sau <- plot_pred_spp("mlotst_transformed", "sau", "#DB9F1D", "(k) Sauries")
gg_lesc <- plot_pred_spp("mlotst_transformed", "lesc", "#FFD67D", "(l) Longfin escolar")

`gg_tmp_jan-mar` <- gg_yft + gg_skp + gg_alb + gg_fri +
  gg_bet + gg_swo + gg_blum + gg_shos +
  gg_strm + gg_sail + gg_sau + gg_lesc +
  plot_layout(ncol = 4, nrow = 3)

ggsave(plot = `gg_tmp_jan-mar`, filename = "Figures/ImptPredictors_MixedLayerDepth_jan-mar.png", width = 27, height = 15, dpi = 300)

#### April-June ####

# Load full prediction data set
df <- read_csv("Output/CSV/FULL_predictions_apr-jun.csv") %>% 
  dplyr::select(-1)

# Load predictor data set
dataTmp <- readRDS("Data/Climatology/sf/mlotst_historical_apr-jun_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor() %>%  # load temperature
  tibble::as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  bind_cols(., df)

gg_yft <- plot_pred_spp("mlotst_transformed", "yft", "#708B75", "(a) Yellowfin tuna")
gg_skp <- plot_pred_spp("mlotst_transformed", "skp", "#88A88E", "(b) Skipjack tuna")
gg_alb <- plot_pred_spp("mlotst_transformed", "alb", "#9DC2A4", "(c) Albacore")
gg_fri <- plot_pred_spp("mlotst_transformed", "fri", "#B2DBB9", "(d) Frigate tuna")
gg_bet <- plot_pred_spp("mlotst_transformed", "bet", "#C2F0CA", "(e) Bigeye tuna")
gg_swo <- plot_pred_spp("mlotst_transformed", "swo", "#6A637A", "(f) Swordfish")
gg_blum <- plot_pred_spp("mlotst_transformed", "blum", "#9086A6", "(g) Blue marlin")
gg_shos <- plot_pred_spp("mlotst_transformed", "shos", "#A297BA", "(h) Shortbill spearfish")
gg_strm <- plot_pred_spp("mlotst_transformed", "strm", "#B8ABD4", "(i) Striped marlin")
gg_sail <- plot_pred_spp("mlotst_transformed", "sail", "#CABCE8", "(j) Sailfish")
gg_sau <- plot_pred_spp("mlotst_transformed", "sau", "#DB9F1D", "(k) Sauries")
gg_lesc <- plot_pred_spp("mlotst_transformed", "lesc", "#FFD67D", "(l) Longfin escolar")

`gg_tmp_apr-jun` <- gg_yft + gg_skp + gg_alb + gg_fri +
  gg_bet + gg_swo + gg_blum + gg_shos +
  gg_strm + gg_sail + gg_sau + gg_lesc +
  plot_layout(ncol = 4, nrow = 3)

ggsave(plot = `gg_tmp_apr-jun`, filename = "Figures/ImptPredictors_MixedLayerDepth_apr-jun.png", width = 27, height = 15, dpi = 300)

#### July-September ####

# Load full prediction data set
df <- read_csv("Output/CSV/FULL_predictions_jul-sept.csv") %>% 
  dplyr::select(-1)

# Load predictor data set
dataTmp <- readRDS("Data/Climatology/sf/mlotst_historical_jul-sept_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor() %>%  # load temperature
  tibble::as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  bind_cols(., df)

gg_yft <- plot_pred_spp("mlotst_transformed", "yft", "#708B75", "(a) Yellowfin tuna")
gg_skp <- plot_pred_spp("mlotst_transformed", "skp", "#88A88E", "(b) Skipjack tuna")
gg_alb <- plot_pred_spp("mlotst_transformed", "alb", "#9DC2A4", "(c) Albacore")
gg_fri <- plot_pred_spp("mlotst_transformed", "fri", "#B2DBB9", "(d) Frigate tuna")
gg_bet <- plot_pred_spp("mlotst_transformed", "bet", "#C2F0CA", "(e) Bigeye tuna")
gg_swo <- plot_pred_spp("mlotst_transformed", "swo", "#6A637A", "(f) Swordfish")
gg_blum <- plot_pred_spp("mlotst_transformed", "blum", "#9086A6", "(g) Blue marlin")
gg_shos <- plot_pred_spp("mlotst_transformed", "shos", "#A297BA", "(h) Shortbill spearfish")
gg_strm <- plot_pred_spp("mlotst_transformed", "strm", "#B8ABD4", "(i) Striped marlin")
gg_sail <- plot_pred_spp("mlotst_transformed", "sail", "#CABCE8", "(j) Sailfish")
gg_sau <- plot_pred_spp("mlotst_transformed", "sau", "#DB9F1D", "(k) Sauries")
gg_lesc <- plot_pred_spp("mlotst_transformed", "lesc", "#FFD67D", "(l) Longfin escolar")

`gg_tmp_jul-sept` <- gg_yft + gg_skp + gg_alb + gg_fri +
  gg_bet + gg_swo + gg_blum + gg_shos +
  gg_strm + gg_sail + gg_sau + gg_lesc +
  plot_layout(ncol = 4, nrow = 3)

ggsave(plot = `gg_tmp_jul-sept`, filename = "Figures/ImptPredictors_MixedLayerDepth_jul-sept.png", width = 27, height = 15, dpi = 300)

#### October-December ####

# Load full prediction data set
df <- read_csv("Output/CSV/FULL_predictions_oct-dec.csv") %>% 
  dplyr::select(-1)

df <- df[3:114786,] # TODO: BUG IN ASSIGNING LONGITUDE AND LATITUDE

# Load predictor data set
dataTmp <- readRDS("Data/Climatology/sf/mlotst_historical_oct-dec_interpolated.rds") %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  crop_predictor() %>%  # load temperature
  tibble::as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  bind_cols(., df)

gg_yft <- plot_pred_spp("mlotst_transformed", "yft", "#708B75", "(a) Yellowfin tuna")
gg_skp <- plot_pred_spp("mlotst_transformed", "skp", "#88A88E", "(b) Skipjack tuna")
gg_alb <- plot_pred_spp("mlotst_transformed", "alb", "#9DC2A4", "(c) Albacore")
gg_fri <- plot_pred_spp("mlotst_transformed", "fri", "#B2DBB9", "(d) Frigate tuna")
gg_bet <- plot_pred_spp("mlotst_transformed", "bet", "#C2F0CA", "(e) Bigeye tuna")
gg_swo <- plot_pred_spp("mlotst_transformed", "swo", "#6A637A", "(f) Swordfish")
gg_blum <- plot_pred_spp("mlotst_transformed", "blum", "#9086A6", "(g) Blue marlin")
gg_shos <- plot_pred_spp("mlotst_transformed", "shos", "#A297BA", "(h) Shortbill spearfish")
gg_strm <- plot_pred_spp("mlotst_transformed", "strm", "#B8ABD4", "(i) Striped marlin")
gg_sail <- plot_pred_spp("mlotst_transformed", "sail", "#CABCE8", "(j) Sailfish")
gg_sau <- plot_pred_spp("mlotst_transformed", "sau", "#DB9F1D", "(k) Sauries")
gg_lesc <- plot_pred_spp("mlotst_transformed", "lesc", "#FFD67D", "(l) Longfin escolar")

`gg_tmp_oct-dec` <- gg_yft + gg_skp + gg_alb + gg_fri +
  gg_bet + gg_swo + gg_blum + gg_shos +
  gg_strm + gg_sail + gg_sau + gg_lesc +
  plot_layout(ncol = 4, nrow = 3)

ggsave(plot = `gg_tmp_oct-dec`, filename = "Figures/ImptPredictors_MixedLayerDepth_oct-dec.png", width = 27, height = 15, dpi = 300)