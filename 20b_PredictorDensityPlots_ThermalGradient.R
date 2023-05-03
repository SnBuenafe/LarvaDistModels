# DESCRIPTION: Density plots for broad-scale thermal gradients

# Define preliminaries
#source("00_Preliminaries.R")
pred_dir <- here::here("Output", "Predictions")
clim_dir <- here::here("Data", "Climatology", "sf")
fig_dir <- here::here("Figures")

#### Assemble data ####
# January-March
df <- read_csv(here::here(pred_dir, "FULL_predictions_jan-mar.csv")) %>% # load full prediction data set
  dplyr::select(-1)

tmp1 <- readRDS(here::here(clim_dir, "thermal_front_historical_jan-mar_interpolated.rds")) %>% # load predictor data set
  dplyr::select(-geometry) %>% 
  dplyr::left_join(df, ., by = "cellID")

# April-June
df <- read_csv(here::here(pred_dir, "FULL_predictions_apr-jun.csv")) %>% # load full prediction data set
  dplyr::select(-1)

tmp2 <- readRDS(here::here(clim_dir, "thermal_front_historical_apr-jun_interpolated.rds")) %>% # load predictor data set
  dplyr::select(-geometry) %>% 
  dplyr::left_join(df, ., by = "cellID")

# July-September
df <- read_csv(here::here(pred_dir, "FULL_predictions_jul-sept.csv")) %>% # load full prediction data set
  dplyr::select(-1)

tmp3 <- readRDS(here::here(clim_dir, "thermal_front_historical_jul-sept_interpolated.rds")) %>% # load predictor data set
  dplyr::select(-geometry) %>% 
  dplyr::left_join(df, ., by = "cellID")

# October-December
df <- read_csv(here::here(pred_dir, "FULL_predictions_oct-dec.csv")) %>% # load full prediction data set
  dplyr::select(-1)

tmp4 <- readRDS(here::here(clim_dir, "thermal_front_historical_oct-dec_interpolated.rds")) %>% # load predictor data set
  dplyr::select(-geometry) %>% 
  dplyr::left_join(df, ., by = "cellID")

fin_tmp <- purrr::reduce(list(tmp1, tmp2, tmp3, tmp4), dplyr::bind_rows) # join all seasons

#### Plot the tunas ####
spp <- c("skp", "yft", "alb", "bet", "fri", "sbft", "bft", "lit", "slt", "bon")
tmp <- fin_tmp %>% 
  dplyr::mutate(skp = case_when(skp >= median(fin_tmp$skp) ~ thermal_front_transformed, TRUE ~ NA),
                yft = case_when(yft >= median(fin_tmp$yft) ~ thermal_front_transformed, TRUE ~ NA),
                alb = case_when(alb >= median(fin_tmp$alb) ~ thermal_front_transformed, TRUE ~ NA),
                bet = case_when(bet >= median(fin_tmp$bet) ~ thermal_front_transformed, TRUE ~ NA),
                fri = case_when(fri >= median(fin_tmp$fri) ~ thermal_front_transformed, TRUE ~ NA),
                sbft = case_when(sbft >= median(fin_tmp$sbft) ~ thermal_front_transformed, TRUE ~ NA),
                bft = case_when(bft >= median(fin_tmp$bft) ~ thermal_front_transformed, TRUE ~ NA),
                lit = case_when(lit >= median(fin_tmp$lit) ~ thermal_front_transformed, TRUE ~ NA),
                slt = case_when(slt >= median(fin_tmp$slt) ~ thermal_front_transformed, TRUE ~ NA),
                bon = case_when(bon >= median(fin_tmp$bon) ~ thermal_front_transformed, TRUE ~ NA)) %>% 
  dplyr::select(skp, yft, alb, bet, fri, sbft, bft, lit, slt, bon) %>% # select and arrange columns
  tidyr::pivot_longer(everything(), names_to = "species", values_to = "transformed") %>% 
  dplyr::mutate(species = fct_relevel(species, rev(spp)))
mean_val <- mean(tmp$transformed, na.rm = TRUE)

col_values <- c("#48594B", "#5F7563", "#708B75", "#88A88E", "#91B397", "#9DC2A4", "#A5CCAC", "#B2DBB9", "#BAE6C1", "#C2F0CA")
(tunas2 <- plot_thermgrad(tmp, col_values, mean_val) +
    theme(axis.title.x = element_blank()))

#### Plot the billfish ####
spp <- c("blum", "shos", "swo", "strm", "sail")
tmp <- fin_tmp %>% 
  dplyr::mutate(blum = case_when(blum >= median(fin_tmp$blum) ~ thermal_front_transformed, TRUE ~ NA),
                shos = case_when(shos >= median(fin_tmp$shos) ~ thermal_front_transformed, TRUE ~ NA),
                swo = case_when(swo >= median(fin_tmp$swo) ~ thermal_front_transformed, TRUE ~ NA),
                strm = case_when(strm >= median(fin_tmp$strm) ~ thermal_front_transformed, TRUE ~ NA),
                sail = case_when(sail >= median(fin_tmp$sail) ~ thermal_front_transformed, TRUE ~ NA)) %>% 
  dplyr::select(blum, shos, swo, strm, sail) %>% # select and arrange columns
  tidyr::pivot_longer(everything(), names_to = "species", values_to = "transformed") %>% 
  dplyr::mutate(species = fct_relevel(species, rev(spp)))
mean_val <- mean(tmp$transformed, na.rm = TRUE)

col_values <- c("#6A637A", "#9086A6", "#A297BA", "#B8ABD4", "#CABCE8")
(bill2 <- plot_thermgrad(tmp, col_values, mean_val) + 
  theme(axis.title.x = element_blank()))

#### Plot the rest ####
spp <- c("lesc", "sau")
tmp <- fin_tmp %>% 
  dplyr::mutate(lesc = case_when(lesc >= median(fin_tmp$lesc) ~ thermal_front_transformed, TRUE ~ NA),
                sau = case_when(sau >= median(fin_tmp$sau) ~ thermal_front_transformed, TRUE ~ NA)) %>% 
  dplyr::select(lesc, sau) %>% # select and arrange columns
  tidyr::pivot_longer(everything(), names_to = "species", values_to = "transformed") %>% 
  dplyr::mutate(species = fct_relevel(species, rev(spp)))
mean_val <- mean(tmp$transformed, na.rm = TRUE)

col_values <- c("#DB9F1D", "#FFD67D")
oth2 <- plot_thermgrad(tmp, col_values, mean_val)

all <- (tunas2 / bill2 / oth2) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = all, filename = here::here(fig_dir, "ImptPredictors_thermal_gradient.png"), width = 15, height = 27, dpi = 300)
