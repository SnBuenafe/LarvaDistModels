# DESCRIPTION: Summary plots for distance to coastline

# Define preliminaries
source("00_Preliminaries.R")
pacman::p_load(ggridges, patchwork, purrr)
figure_dir <- here::here(figure_dir, "predictors")
spp_list <- c("skp", "yft", "alb", "bet", "fri", "sbft", "bft", "lit", "slt", "bon", "blum", "shos", "swo", "strm", "sail", "lesc", "sau")

fin_tmp <- assembleOthers("coastDistance") %>% 
  dplyr::mutate(across(everything(), ~(.x/1000))) # convert to km instead of m
  
#### Boxplots ####
breaks = waiver()
limits = c(0, 2000)
expand = c(0.1,0.1)

(skp <- plotBP(fin_tmp,
               "skp", 
               breaks, limits, expand,
               "Skipjack tuna") +
    ylab(expression('distance to the\nnearest coastline (km)')) +
    theme(axis.title.y = element_text(color = "black", size = 20, angle = 90)))
(yft <- plotBP(fin_tmp, 
               "yft", 
               breaks, limits, expand,
               "Yellowfin tuna"))
(alb <- plotBP(fin_tmp, 
               "alb", 
               breaks, limits, expand,
               "Albacore"))
(bet <- plotBP(fin_tmp, 
               "bet", 
               breaks, limits, expand,
               "Bigeye tuna"))
(fri <- plotBP(fin_tmp, 
               "fri", 
               breaks, limits, expand,
               "Frigate tuna"))
(sbft <- plotBP(fin_tmp,
                "sbft", 
                breaks, limits, expand,
                "Southern bluefin tuna") +
    ylab(expression('distance to the\nnearest coastline (km)')) +
    theme(axis.title.y = element_text(color = "black", size = 20, angle = 90)))
(bft <- plotBP(fin_tmp, 
               "bft", 
               breaks, limits, expand,
               "Pacific bluefin tuna"))
(lit <- plotBP(fin_tmp, 
               "lit", 
               breaks, limits, expand,
               "Little tuna"))
(slt <- plotBP(fin_tmp, 
               "slt", 
               breaks, limits, expand,
               "Slender tuna"))
(bon <- plotBP(fin_tmp, 
               "bon", 
               breaks, limits, expand,
               "Bonitos"))
(blum <- plotBP(fin_tmp, 
                "blum", 
                breaks, limits, expand,
                "Blue marlin") +
    ylab(expression('distance to the\nnearest coastline (km)')) +
    theme(axis.title.y = element_text(color = "black", size = 20, angle = 90)))
(shos <- plotBP(fin_tmp, 
                "shos", 
                breaks, limits, expand,
                "Shortbill spearfish"))
(swo <- plotBP(fin_tmp, 
               "swo", 
               breaks, limits, expand,
               "Swordfish"))
(strm <- plotBP(fin_tmp,
                "strm", 
                breaks, limits, expand,
                "Striped marlin"))
(sail <- plotBP(fin_tmp, 
                "sail",
                breaks, limits, expand,
                "Sailfish"))
(lesc <- plotBP(fin_tmp, 
                "lesc", 
                breaks, limits, expand,
                "Longfin escolar") +
    ylab(expression('distance to the\nnearest coastline (km)')) +
    theme(axis.title.y = element_text(color = "black", size = 20, angle = 90)))
(sau <- plotBP(fin_tmp, 
               "sau", 
               breaks, limits, expand,
               "Sauries"))

all <- skp + yft + alb + bet + fri + sbft + bft + lit + slt + bon +
  blum + shos + swo + strm + sail +
  lesc + sau + plot_spacer() + plot_spacer() + plot_spacer() +
  plot_layout(ncol = 5, nrow = 4) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = all, filename = here::here(figure_dir, "Dist2Coast_Seasonality.png"), width = 60, height = 20, dpi = 300, limitsize = FALSE)

#### Kernel density plots ####
# Abundant tunas
spp <- c("skp", "yft", "alb", "bet", "fri")
col_values <- c("#48594B", "#708B75", "#91B397", "#A5CCAC", "#BAE6C1")

df <- prepareDF(fin_tmp, spp) # assemble data frame to plot the KD plot
printSummary(df)

df %<>%
  dplyr::mutate(transformed = log(transformed)) %>% 
  dplyr::mutate(transformed = case_when(transformed < 2 ~ 2,
                                        TRUE ~ transformed))

(ab_tunas <- plot_KD(df,
                     spp,
                     expression('distance to the nearest coastline (km)'),
                     limits = c(2, NA),
                     col_values
) +
    theme(axis.title.x = element_blank())) # KD plots

# Less abundant tunas
spp <- c("sbft", "bft", "lit", "slt", "bon")
col_values <- c("#48594B", "#708B75", "#91B397", "#A5CCAC", "#BAE6C1")

df <- prepareDF(fin_tmp, spp) # assemble data frame to plot the KD plot
printSummary(df)

df %<>%
  dplyr::mutate(transformed = log(transformed)) %>% 
  dplyr::mutate(transformed = case_when(transformed < 2 ~ 2,
                                        TRUE ~ transformed))

(lab_tunas <- plot_KD(df,
                     spp,
                     expression('distance to the nearest coastline (km)'),
                     limits = c(2, NA),
                     col_values
) +
    theme(axis.title.x = element_blank())) # KD plots

# Billfish
spp <- c("blum", "shos", "swo", "strm", "sail")
col_values <- c("#6A637A", "#9086A6", "#A297BA", "#B8ABD4", "#CABCE8")

df <- prepareDF(fin_tmp, spp) # assemble data frame to plot the KD plot
printSummary(df)

df %<>%
  dplyr::mutate(transformed = log(transformed)) %>% 
  dplyr::mutate(transformed = case_when(transformed < 2 ~ 2,
                                        TRUE ~ transformed))

(bill <- plot_KD(df,
                     spp,
                     expression('distance to the nearest coastline (km)'),
                     limits = c(2, NA),
                     col_values
) +
    theme(axis.title.x = element_blank())) # KD plots

# Other species
spp <- c("lesc", "sau")
col_values <- c("#DB9F1D", "#FFD67D")

df <- prepareDF(fin_tmp, spp) # assemble data frame to plot the KD plot
printSummary(df)

df %<>%
  dplyr::mutate(transformed = log(transformed)) %>% 
  dplyr::mutate(transformed = case_when(transformed < 2 ~ 2,
                                        TRUE ~ transformed))

(oth <- plot_KD(df,
                     spp,
                     expression('distance to the nearest coastline (km)'),
                     limits = c(2, NA),
                     col_values
)) # KD plots

all <- ((ab_tunas) / (lab_tunas) / (bill) / oth)

ggsave(plot = all, filename = here::here(figure_dir, "ImptPredictors_Dist2Coast.png"), width = 15, height = 27, dpi = 300)
