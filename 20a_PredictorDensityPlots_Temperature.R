# DESCRIPTION: Density plots for temperature vs predictions

# Define preliminaries
source("00_Preliminaries.R")
pred_dir <- here::here("Output", "Predictions")
clim_dir <- here::here("Data", "Climatology", "sf")
fig_dir <- here::here("Figures")

# Greens: c("#708B75", "#88A88E", "#9DC2A4", "#B2DBB9", "#C2F0CA")
# Purples: c("#6A637A", "#9086A6", "#A297BA", "#B8ABD4", "#CABCE8")
# Yellows: c("#DB9F1D", "#FFD67D")

# Function for plotting temperature vs probability
plotPredictor <- function(predictor, spp, title) {
  gg <- ggplot(fin_tmp) +
    geom_point(aes(x = !!sym(predictor), y = !!sym(spp)), color = "#708B75", size = 0.5, shape = 16) +
    theme_bw() +
    ggtitle(title) +
    xlab(expression('Temperature ('^"o"*'C)')) +
    ylab("Probability") +
    theme(axis.text = element_text(size = 15, color = "black"),
          axis.ticks = element_line(linewidth = 2, color = "black"),
          axis.title = element_blank(),
          plot.title = element_text(size = 20, color = "black"))
}

# January-March
df <- read_csv(here::here(pred_dir, "FULL_predictions_jan-mar.csv")) %>% # load full prediction data set
  dplyr::select(-1)

tmp1 <- readRDS(here::here(clim_dir, "tos_historical_jan-mar_interpolated.rds")) %>% # load predictor data set
  dplyr::select(-geometry) %>% 
  dplyr::left_join(df, ., by = "cellID")

# April-June
df <- read_csv(here::here(pred_dir, "FULL_predictions_apr-jun.csv")) %>% # load full prediction data set
  dplyr::select(-1)

tmp2 <- readRDS(here::here(clim_dir, "tos_historical_apr-jun_interpolated.rds")) %>% # load predictor data set
  dplyr::select(-geometry) %>% 
  dplyr::left_join(df, ., by = "cellID")

# July-September
df <- read_csv(here::here(pred_dir, "FULL_predictions_jul-sept.csv")) %>% # load full prediction data set
  dplyr::select(-1)

tmp3 <- readRDS(here::here(clim_dir, "tos_historical_jul-sept_interpolated.rds")) %>% # load predictor data set
  dplyr::select(-geometry) %>% 
  dplyr::left_join(df, ., by = "cellID")

# October-December
df <- read_csv(here::here(pred_dir, "FULL_predictions_oct-dec.csv")) %>% # load full prediction data set
  dplyr::select(-1)

tmp4 <- readRDS(here::here(clim_dir, "tos_historical_oct-dec_interpolated.rds")) %>% # load predictor data set
  dplyr::select(-geometry) %>% 
  dplyr::left_join(df, ., by = "cellID")

fin_tmp <- purrr::reduce(list(tmp1, tmp2, tmp3, tmp4), dplyr::bind_rows) # join all seasons

# Plot all the species
gg_yft <- plotPredictor("tos_transformed", "yft", "(a) Yellowfin tuna")
gg_skp <- plotPredictor("tos_transformed", "skp", "(b) Skipjack tuna")
gg_alb <- plotPredictor("tos_transformed", "alb", "(c) Albacore")
gg_fri <- plotPredictor("tos_transformed", "fri", "(d) Frigate tuna")
gg_bet <- plotPredictor("tos_transformed", "bet", "(e) Bigeye tuna")
gg_bft <- plotPredictor("tos_transformed", "bft", "(f) Pacific bluefin tuna")
gg_sbft <- plotPredictor("tos_transformed", "sbft", "(g) Southern bluefin tuna")
gg_slt <- plotPredictor("tos_transformed", "slt", "(h) Slender tuna")
gg_bon <- plotPredictor("tos_transformed", "bon", "(i) Bonitos")
gg_swo <- plotPredictor("tos_transformed", "swo", "(j) Swordfish")
gg_blum <- plotPredictor("tos_transformed", "blum", "(k) Blue marlin")
gg_sail <- plotPredictor("tos_transformed", "sail", "(l) Sailfish")
gg_sau <- plotPredictor("tos_transformed", "sau", "(m) Sauries")


gg_tmp <- gg_yft + gg_skp + gg_alb + gg_fri +
  gg_bet + gg_bft + gg_sbft + gg_slt +
  gg_bon + gg_swo + gg_blum + gg_sail + gg_sau +
  plot_layout(nrow = 3)

ggsave(plot = gg_tmp, filename = here::here(fig_dir, "ImptPredictors_tos_jan-mar.png"), width = 27, height = 15, dpi = 300)
