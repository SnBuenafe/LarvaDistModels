# DESCRIPTION: Barplots showing seasonality between N. and S. Hemispheres

# Load preliminaries
source("00_SetupGrid.R")
source("00_Preliminaries.R")
pacman::p_load(patchwork, purrr)
seas_list <- c("January-March", "April-June", "July-September", "October-December")

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

plot_spp <- function(spp, label) {
  df <- full %>% 
    dplyr::select(hemisphere, starts_with(spp)) %>% 
    dplyr::mutate(across(starts_with(paste0(spp, "_")),
                         ~ ifelse(hemisphere == "South", -(.x), .x),
                         .names = "{col}"))
  
  fin <- df %>% 
    tidyr::pivot_longer(!hemisphere, names_to = "season", values_to = "sum") %>% 
    dplyr::mutate(season = case_when(str_detect(season, "jan-mar") ~ "January-March",
                                     str_detect(season, "apr-jun") ~ "April-June",
                                     str_detect(season, "jul-sept") ~ "July-September",
                                     str_detect(season, "oct-dec") ~ "October-December")) %>% 
    dplyr::mutate(season = fct_relevel(season, seas_list))
  
  fin2 <- df %>% 
    dplyr::summarise(across(!hemisphere, mean)) %>% 
    tidyr::pivot_longer(cols = everything(),
                        names_to = "season",
                        values_to = "mean") %>% 
    dplyr::mutate(season = case_when(str_detect(season, "jan-mar") ~ "January-March",
                                     str_detect(season, "apr-jun") ~ "April-June",
                                     str_detect(season, "jul-sept") ~ "July-September",
                                     str_detect(season, "oct-dec") ~ "October-December"))
    
  ggplot() + 
    geom_bar(data = fin, aes(x = season, y = sum, fill = hemisphere),
             stat = "identity", position = "identity", show.legend = FALSE) +
    scale_fill_manual(aesthetics = "fill",
      values = c("North" = "#0084C2", "South" = "#EAB47F")) +
    geom_line(data = fin2, aes(x = season, y = mean, group = 1), color = "black", linewidth = 3) +
    geom_point(data = fin2, aes(x = season, y = mean), size = 5) +
    scale_x_discrete(expand = expansion(add = c(0.5, 0.5))) +
    scale_y_continuous(limits = c(-max(abs(fin$sum)), max(abs(fin$sum)))) +
    ggtitle(label) +
    xlab("Seasons") +
    ylab("Sum of probabilities") +
    theme_bw() +
    theme(axis.text.y = element_text(color = "black", size = 12),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_line(color = "black"),
          axis.text.x = element_text(color = "black", size = 18),
          axis.title.x = element_blank(),
          plot.title = element_text(color = "black", size = 22))
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
(skp <- plot_spp("skp", "Skipjack tuna") +
    ylab("Mean probability") +
    theme(axis.title.y = element_text(color = "black", size = 20, angle = 90, vjust = 2)))
(yft <- plot_spp("yft", "Yellowfin tuna"))
(alb <- plot_spp("alb", "Albacore"))
(bet <- plot_spp("bet", "Bigeye tuna"))
(fri <- plot_spp("fri", "Frigate tuna"))
(sbft <- plot_spp("sbft", "Southern bluefin tuna") +
    ylab("Mean probability") +
    theme(axis.title.y = element_text(color = "black", size = 20, angle = 90, vjust = 2)))
(bft <- plot_spp("bft", "Pacific bluefin tuna"))
(lit <- plot_spp("lit", "Little tuna"))
(slt <- plot_spp("slt", "Slender tuna"))
(bon <- plot_spp("bon", "Bonitos"))
(blum <- plot_spp("blum", "Blue marlin") +
    ylab("Mean probability") +
    theme(axis.title.y = element_text(color = "black", size = 20, angle = 90, vjust = 2)))
(shos <- plot_spp("shos", "Shortbill spearfish"))
(swo <- plot_spp("swo", "Swordfish"))
(strm <- plot_spp("strm", "Striped marlin"))
(sail <- plot_spp("sail", "Sailfish"))
(lesc <- plot_spp("lesc", "Longfin escolar") +
    ylab("Mean probability") +
    theme(axis.title.y = element_text(color = "black", size = 20, angle = 90, vjust = 2)))
(sau <- plot_spp("sau", "Sauries"))

all <- skp + yft + alb + bet + fri + sbft + bft + lit + slt + bon +
  blum + shos + swo + strm + sail +
  lesc + sau + plot_spacer() + plot_spacer() + plot_spacer() +
  plot_layout(ncol = 5, nrow = 4) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = all, filename = here::here(figure_dir, "Hemisphere_Seasonality.png"), width = 60, height = 20, dpi = 300, limitsize = FALSE)
