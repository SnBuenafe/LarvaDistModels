# DESCRIPTION: Barplots showing seasonality between N. and S. Hemispheres

# Load preliminaries
source("00_Preliminaries.R")
fig_dir <- here::here("Figures")
seas_list <- c("January-March", "April-June", "July-September", "October-December")

load_data <- function(seas) {
  full_jm <- read_csv(here::here(pred_dir, paste0("FULL_predictions_", seas, ".csv"))) %>%  # Jan-Mar
    dplyr::left_join(., dummy) %>% 
    dplyr::select(-c(1:2, 20:24, 26)) %>% 
    dplyr::mutate(hemisphere = ifelse(latitude >= 0, "North", "South")) %>% 
    dplyr::group_by(hemisphere) %>% 
    dplyr::summarise(across(c(everything(), -latitude), sum)) %>% 
    dplyr::rename_with(~paste(., seas, sep = "_"), !hemisphere)
}

plot_spp <- function(spp) {
  df <- full %>% 
    dplyr::select(hemisphere, starts_with(spp)) %>% 
    dplyr::mutate(across(starts_with(paste0(spp, "_")),
                         ~ ifelse(hemisphere == "South", -(.x), .x),
                         .names = "{col}")) %>% 
    tidyr::pivot_longer(!hemisphere, names_to = "season", values_to = "sum") %>% 
    dplyr::mutate(season = case_when(str_detect(season, "jan-mar") ~ "January-March",
                                     str_detect(season, "apr-jun") ~ "April-June",
                                     str_detect(season, "jul-sept") ~ "July-September",
                                     str_detect(season, "oct-dec") ~ "October-December")) %>% 
    dplyr::mutate(season = fct_relevel(season, seas_list))
  
  ggplot(df, aes(x = season, y = sum, fill = hemisphere)) + 
    geom_bar(stat = "identity", position = "identity", show.legend = FALSE) +
    scale_fill_manual(aesthetics = "fill",
      values = c("North" = "#0084C2", "South" = "#EAB47F")) +
    scale_x_discrete(expand = expansion(add = c(0.5, 0.5))) +
    scale_y_continuous(limits = c(-max(abs(df$sum)), max(abs(df$sum)))) +
    xlab("Seasons") +
    ylab("Sum of probabilities") +
    theme_bw() +
    theme(axis.text.y = element_text(color = "black", size = 12),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_line(color = "black"),
          axis.text.x = element_text(color = "black", size = 18),
          axis.title.x = element_blank())
          #axis.title.x = element_text(color = "black", size = 25))
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
(yft <- plot_spp("yft"))
(alb <- plot_spp("alb"))
(bet <- plot_spp("bet"))
(fri <- plot_spp("fri"))
(sbft <- plot_spp("sbft"))
(bft <- plot_spp("bft"))
(lit <- plot_spp("lit"))
(slt <- plot_spp("slt"))
(bon <- plot_spp("bon"))
(blum <- plot_spp("blum"))
(shos <- plot_spp("shos"))
(swo <- plot_spp("swo"))
(strm <- plot_spp("strm"))
(sail <- plot_spp("sail"))
(lesc <- plot_spp("lesc"))
(sau <- plot_spp("sau"))

all <- skp + yft + alb + bet + fri + sbft + bft + lit + slt + bon +
  blum + shos + swo + strm + sail +
  lesc + sau + plot_spacer() + plot_spacer() + plot_spacer() +
  plot_layout(ncol = 5, nrow = 4) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = all, filename = here::here(fig_dir, "Hemisphere_Seasonality.png"), width = 60, height = 20, dpi = 300, limitsize = FALSE)
