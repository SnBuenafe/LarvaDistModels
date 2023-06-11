# DESCRIPTION: Summary plots for surface chlorophyll

# Define preliminaries
source("00_Preliminaries.R")
pacman::p_load(ggridges, patchwork, purrr)
figure_dir <- here::here(figure_dir, "predictors")
spp_list <- c("skp", "yft", "alb", "bet", "fri", "sbft", "bft", "lit", "slt", "bon", "blum", "shos", "swo", "strm", "sail", "lesc", "sau")

fin_tmp <- assemblePreds("chlos") %>% 
  dplyr::mutate(across(everything(), ~(.x*1000000))) # convert to mg m-3 instead of kg m-3

#### Boxplots ####
breaks = seq(0, 0.3, 0.05)
limits = c(0, 0.3)
expand = c(0,0)

(skp <- plotBP(fin_tmp,
               "skp", 
               breaks, limits, expand,
               "Skipjack tuna") +
    ylab(expression('[Chl-a] (mg m'^'-3'*')')) +
    theme(axis.title.y = element_text(color = "black", size = 25, angle = 90, vjust = 1)))
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
               "Bigeye tuna") +
    ylab(expression('[Chl-a] (mg m'^'-3'*')')) +
    theme(axis.title.y = element_text(color = "black", size = 25, angle = 90, vjust = 1)))
(fri <- plotBP(fin_tmp, 
               "fri", 
               breaks, limits, expand,
               "Frigate tuna"))
(sbft <- plotBP(fin_tmp,
                "sbft", 
                breaks, limits, expand,
                "Southern bluefin tuna"))
(bft <- plotBP(fin_tmp, 
               "bft", 
               breaks, limits, expand,
               "Pacific bluefin tuna") +
    ylab(expression('[Chl-a] (mg m'^'-3'*')')) +
    theme(axis.title.y = element_text(color = "black", size = 25, angle = 90, vjust = 1)))
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
               "Bonitos") +
    ylab(expression('[Chl-a] (mg m'^'-3'*')')) +
    theme(axis.title.y = element_text(color = "black", size = 25, angle = 90, vjust = 1)))
(blum <- plotBP(fin_tmp, 
                "blum", 
                breaks, limits, expand,
                "Blue marlin"))
(shos <- plotBP(fin_tmp, 
                "shos", 
                breaks, limits, expand,
                "Shortbill spearfish"))
(swo <- plotBP(fin_tmp, 
               "swo", 
               breaks, limits, expand,
               "Swordfish") +
    ylab(expression('[Chl-a] (mg m'^'-3'*')')) +
    theme(axis.title.y = element_text(color = "black", size = 25, angle = 90, vjust = 1)))
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
    ylab(expression('[Chl-a] (mg m'^'-3'*')')) +
    theme(axis.title.y = element_text(color = "black", size = 25, angle = 90, vjust = 1)))
(sau <- plotBP(fin_tmp, 
               "sau", 
               breaks, limits, expand,
               "Sauries"))

all <- skp + yft + alb + bet + fri + sbft + bft + lit + slt + bon +
  blum + shos + swo + strm + sail +
  lesc + sau + plot_spacer() +
  plot_layout(ncol = 3, nrow = 6)

ggsave(plot = all, filename = here::here(figure_dir, "Seasonality_chlos.png"), width = 30, height = 25, dpi = 300, limitsize = FALSE)

#### Kernel density plots ####
# Abundant tunas
spp <- c("skp", "yft", "alb", "bet", "fri")
col_values <- c("#48594B", "#708B75", "#91B397", "#A5CCAC", "#BAE6C1")

df <- prepareDF(fin_tmp, spp) # assemble data frame to plot the KD plot
printSummary(df)

df %<>%
  dplyr::mutate(transformed = log(transformed))

(ab_tunas <- plot_KD(df,
                     spp,
                     expression('log (chlorophyll)'),
                     limits = c(NA, NA),
                     col_values
) +
    theme(axis.title.x = element_blank())) # KD plots

# Less abundant tunas
spp <- c("sbft", "bft", "lit", "slt", "bon")
col_values <- c("#48594B", "#708B75", "#91B397", "#A5CCAC", "#BAE6C1")

df <- prepareDF(fin_tmp, spp) # assemble data frame to plot the KD plot
printSummary(df)

df %<>%
  dplyr::mutate(transformed = log(transformed))

(lab_tunas <- plot_KD(df,
                     spp,
                     expression('log (chlorophyll)'),
                     limits = c(NA, NA),
                     col_values
) +
    theme(axis.title.x = element_blank())) # KD plots

# Billfish
spp <- c("blum", "shos", "swo", "strm", "sail")
col_values <- c("#6A637A", "#9086A6", "#A297BA", "#B8ABD4", "#CABCE8")

df <- prepareDF(fin_tmp, spp) # assemble data frame to plot the KD plot
printSummary(df)

df %<>%
  dplyr::mutate(transformed = log(transformed))

(bill <- plot_KD(df,
                     spp,
                     expression('log (chlorophyll)'),
                     limits = c(NA, NA),
                     col_values
) +
    theme(axis.title.x = element_blank())) # KD plots

# Other species
spp <- c("lesc", "sau")
col_values <- c("#DB9F1D", "#FFD67D")

df <- prepareDF(fin_tmp, spp) # assemble data frame to plot the KD plot
printSummary(df)

df %<>%
  dplyr::mutate(transformed = log(transformed))

(oth <- plot_KD(df,
                     spp,
                     expression('log (chlorophyll)'),
                     limits = c(NA, NA),
                     col_values
)) # KD plots

all <- ((ab_tunas) / (lab_tunas) / (bill) / oth)

ggsave(plot = all, filename = here::here(figure_dir, "ImptPredictors_Chlorophyll.png"), width = 15, height = 27, dpi = 300)
