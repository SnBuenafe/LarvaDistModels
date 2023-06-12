# DESCRIPTION: Barplots showing seasonality between N. and S. Hemispheres

# Load preliminaries
source("00_SetupGrid.R")
source("00_Preliminaries.R")
pacman::p_load(patchwork, purrr)
seas_list <- c("Jan-Mar", "Apr-Jun", "Jul-Sept", "Oct-Dec")

load_data <- function(seas) {
  full <- read_csv(here::here(preds_dir, paste0("FULL_predictions_", seas, ".csv"))) %>%  # Jan-Mar
    dplyr::left_join(., dummy) %>% 
    dplyr::select(-c(1:2, 20:24, 26)) %>% 
    dplyr::mutate(hemisphere = ifelse(latitude >= 0, "North", "South")) 
  
  standardize <- function(x) {
    y <- sum(x) / n()
  }
  
  df <- full %>% 
    dplyr::group_by(hemisphere) %>% 
    dplyr::summarise(across(c(everything(), -latitude), standardize)) %>% 
    dplyr::rename_with(~paste(., seas, sep = "_"), !hemisphere)
  
}

plot_spp <- function(spp) {
  df <- full %>% 
    dplyr::select(hemisphere, starts_with(spp)) %>% 
    dplyr::mutate(across(starts_with(paste0(spp, "_")),
                         ~ ifelse(hemisphere == "South", -(.x), .x),
                         .names = "{col}"))
  
  fin <- df %>% 
    tidyr::pivot_longer(!hemisphere, names_to = "season", values_to = "sum") %>% 
    dplyr::mutate(season = case_when(str_detect(season, "jan-mar") ~ "Jan-Mar",
                                     str_detect(season, "apr-jun") ~ "Apr-Jun",
                                     str_detect(season, "jul-sept") ~ "Jul-Sept",
                                     str_detect(season, "oct-dec") ~ "Oct-Dec")) %>% 
    dplyr::mutate(season = fct_relevel(season, seas_list))
  
  fin2 <- df %>% 
    dplyr::summarise(across(!hemisphere, mean)) %>% 
    tidyr::pivot_longer(cols = everything(),
                        names_to = "season",
                        values_to = "mean") %>% 
    dplyr::mutate(season = case_when(str_detect(season, "jan-mar") ~ "Jan-Mar",
                                     str_detect(season, "apr-jun") ~ "Apr-Jun",
                                     str_detect(season, "jul-sept") ~ "Jul-Sept",
                                     str_detect(season, "oct-dec") ~ "Oct-Dec"))
    
  ggplot() + 
    geom_bar(data = fin, aes(x = season, y = sum, fill = hemisphere),
             width = 0.9,
             stat = "identity", position = "identity", show.legend = FALSE) +
    scale_fill_manual(aesthetics = "fill",
      values = c("North" = "#0084C2", "South" = "#EAB47F")) +
    geom_line(data = fin2, aes(x = season, y = mean, group = 1), color = "black", linewidth = 3) +
    geom_point(data = fin2, aes(x = season, y = mean), size = 5) +
    scale_x_discrete(expand = expansion(add = c(0.5, 0.5))) +
    scale_y_continuous(limits = c(-max(abs(fin$sum)), max(abs(fin$sum)))) +
    xlab("Seasons") +
    ylab("Sum of probabilities") +
    theme_bw() +
    theme(axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_line(color = "black"),
          axis.text = element_text(size = 25, color = "black"),
          plot.margin = unit(c(1,0.5,1,0.5), "cm"),
          axis.title.x = element_blank()
          #plot.title = element_text(color = "black", size = 30))
          #axis.title.x = element_text(color = "black", size = 25))
    )
}

# Load dummy data to match coordinates and cellIDs
sf <- combineFish(species = "yellowfin-tuna") %>% 
  fSpatPlan_Convert2PacificCentered(., cCRS = cCRS) %>% 
  sf::st_centroid() # transform into point data
dummy <- assembleGrid(grid, sf %>% dplyr::filter(season == "jan-mar")) # season here doesn't really matter; it will produce the coordinates across the entire region

# Load data
full_jm <- load_data("jan-mar")

full_aj <- load_data("apr-jun")

full_js <- load_data("jul-sept")

full_od <- load_data("oct-dec")

full <- purrr::reduce(list(full_jm, full_aj, full_js, full_od), dplyr::left_join)

# Seasonal bar plots per taxon
(skp <- plot_spp("skp"))
ggsave(filename = here::here(figure_dir, "SKP", paste0("HemisSeas_bp.png")), plot = skp, dpi = 600, width = 7.5, height = 10)
(yft <- plot_spp("yft"))
ggsave(filename = here::here(figure_dir, "YFT", paste0("HemisSeas_bp.png")), plot = yft, dpi = 600, width = 7.5, height = 10)
(alb <- plot_spp("alb"))
ggsave(filename = here::here(figure_dir, "ALB", paste0("HemisSeas_bp.png")), plot = alb, dpi = 600, width = 7.5, height = 10)
(bet <- plot_spp("bet"))
ggsave(filename = here::here(figure_dir, "BET", paste0("HemisSeas_bp.png")), plot = bet, dpi = 600, width = 7.5, height = 10)
(fri <- plot_spp("fri"))
ggsave(filename = here::here(figure_dir, "FRI", paste0("HemisSeas_bp.png")), plot = fri, dpi = 600, width = 7.5, height = 10)
(sbft <- plot_spp("sbft"))
ggsave(filename = here::here(figure_dir, "SBFT", paste0("HemisSeas_bp.png")), plot = sbft, dpi = 600, width = 7.5, height = 10)
(bft <- plot_spp("bft"))
ggsave(filename = here::here(figure_dir, "BFT", paste0("HemisSeas_bp.png")), plot = bft, dpi = 600, width = 7.5, height = 10)
(lit <- plot_spp("lit"))
ggsave(filename = here::here(figure_dir, "LIT", paste0("HemisSeas_bp.png")), plot = lit, dpi = 600, width = 7.5, height = 10)
(slt <- plot_spp("slt"))
ggsave(filename = here::here(figure_dir, "SLT", paste0("HemisSeas_bp.png")), plot = slt, dpi = 600, width = 7.5, height = 10)
(bon <- plot_spp("bon"))
ggsave(filename = here::here(figure_dir, "BON", paste0("HemisSeas_bp.png")), plot = bon, dpi = 600, width = 7.5, height = 10)
(blum <- plot_spp("blum"))
ggsave(filename = here::here(figure_dir, "BLUM", paste0("HemisSeas_bp.png")), plot = blum, dpi = 600, width = 7.5, height = 10)
(shos <- plot_spp("shos"))
ggsave(filename = here::here(figure_dir, "SHOS", paste0("HemisSeas_bp.png")), plot = shos, dpi = 600, width = 7.5, height = 10)
(swo <- plot_spp("swo"))
ggsave(filename = here::here(figure_dir, "SWO", paste0("HemisSeas_bp.png")), plot = swo, dpi = 600, width = 7.5, height = 10)
(strm <- plot_spp("strm"))
ggsave(filename = here::here(figure_dir, "STRM", paste0("HemisSeas_bp.png")), plot = strm, dpi = 600, width = 7.5, height = 10)
(sail <- plot_spp("sail"))
ggsave(filename = here::here(figure_dir, "SAIL", paste0("HemisSeas_bp.png")), plot = sail, dpi = 600, width = 7.5, height = 10)
(lesc <- plot_spp("lesc"))
ggsave(filename = here::here(figure_dir, "LESC", paste0("HemisSeas_bp.png")), plot = lesc, dpi = 600, width = 7.5, height = 10)
(sau <- plot_spp("sau"))
ggsave(filename = here::here(figure_dir, "SAU", paste0("HemisSeas_bp.png")), plot = sau, dpi = 600, width = 7.5, height = 10)

# all <- skp + yft + alb + bet + fri + sbft + bft + lit + slt + bon +
#   blum + shos + swo + strm + sail +
#   lesc + sau + plot_spacer() + plot_spacer() + plot_spacer() +
#   plot_layout(ncol = 5, nrow = 4) +
#   plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
#   theme(plot.tag = element_text(size = 25))
# 
# ggsave(plot = all, filename = here::here(figure_dir, "Hemisphere_Seasonality.png"), width = 60, height = 20, dpi = 300, limitsize = FALSE)
