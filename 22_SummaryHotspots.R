# DESCRIPTION: Identify hot

pc_dir <- here::here("Output", "PCA")
fig_dir <- here::here("Figures")
seas_list <- c("jan-mar", "apr-jun", "jul-sept", "oct-dec")
dum_list <- list()

for(i in 1:length(seas_list)) {
  PC_scores <- read_csv(here::here(pc_dir, paste0("hotspots_", seas_list[i], "_scores.csv"))) %>%  # Load PC scores
    dplyr::select(Comp.1) %>% 
    dplyr::rename(!!sym(seas_list[i]) := Comp.1)
  
  dum_list[[i]] <- readRDS(here::here(pred_dir, paste0("YFT_", seas_list[i], ".rds"))) %>% 
    dplyr::select(-ocean, -model, -grid_100_category) %>% 
    cbind(., PC_scores) %>% 
    dplyr::as_tibble()
}

dum <- purrr::reduce(dum_list, dplyr::full_join) %>% 
  dplyr::select(cellID, `jan.mar`, `apr.jun`, `jul.sept`, `oct.dec`, geometry) 

# January-March hotspots
fin <- dum %>%  # arrange columns
  dplyr::mutate(`jan.mar` = case_when((`jan.mar` >= (quantile(dum$jan.mar, c(0.95), na.rm = TRUE) %>% unname())) ~ "High",
                                      (`jan.mar` < (quantile(dum$jan.mar, c(0.95), na.rm = TRUE) %>% unname())) & (`jan.mar` >= (quantile(dum$jan.mar, c(0.85), na.rm = TRUE) %>% unname())) ~ "Mid",
                                      TRUE ~ NA)) %>% 
  sf::st_as_sf()

sum1 <- ggplot() + geom_sf(data = fin, aes(fill = as.factor(jan.mar)), color = NA, size = 0.1) +
  scale_fill_manual(values = c("High" = "#EB4949", "Mid" = "#66A7C4"),
                    na.value = NA) +
  geom_sf(data = landmass, fill = "black", color = NA, size = 0.01) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_blank(),
        legend.text = element_text(size = 20),
        legend.title = element_blank(),
        panel.border = element_blank()) +
  coord_sf(xlim = st_bbox(grid)$xlim, ylim = st_bbox(grid)$ylim) 

# April-June hotspots
fin <- dum %>%  # arrange columns
  dplyr::mutate(`apr.jun` = case_when((`apr.jun` >= (quantile(dum$apr.jun, c(0.95), na.rm = TRUE) %>% unname())) ~ "High",
                                      (`apr.jun` < (quantile(dum$apr.jun, c(0.95), na.rm = TRUE) %>% unname())) & (`apr.jun` >= (quantile(dum$apr.jun, c(0.85), na.rm = TRUE) %>% unname())) ~ "Mid",
                                      TRUE ~ NA)) %>% 
  sf::st_as_sf()

sum2 <- ggplot() + geom_sf(data = fin, aes(fill = as.factor(apr.jun)), color = NA, size = 0.1) +
  scale_fill_manual(values = c("High" = "#EB4949", "Mid" = "#66A7C4"),
                    na.value = NA) +
  geom_sf(data = landmass, fill = "black", color = NA, size = 0.01) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_blank(),
        legend.text = element_text(size = 20),
        legend.title = element_blank(),
        panel.border = element_blank()) +
  coord_sf(xlim = st_bbox(grid)$xlim, ylim = st_bbox(grid)$ylim) 

# July-September hotspots
fin <- dum %>%  # arrange columns
  dplyr::mutate(`jul.sept` = case_when((`jul.sept` >= (quantile(dum$jul.sept, c(0.95), na.rm = TRUE) %>% unname())) ~ "High",
                                      (`jul.sept` < (quantile(dum$jul.sept, c(0.95), na.rm = TRUE) %>% unname())) & (`jul.sept` >= (quantile(dum$jul.sept, c(0.85), na.rm = TRUE) %>% unname())) ~ "Mid",
                                      TRUE ~ NA)) %>% 
  sf::st_as_sf()

sum3 <- ggplot() + geom_sf(data = fin, aes(fill = as.factor(jul.sept)), color = NA, size = 0.1) +
  scale_fill_manual(values = c("High" = "#EB4949", "Mid" = "#66A7C4"),
                    na.value = NA) +
  geom_sf(data = landmass, fill = "black", color = NA, size = 0.01) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_blank(),
        legend.text = element_text(size = 20),
        legend.title = element_blank(),
        panel.border = element_blank()) +
  coord_sf(xlim = st_bbox(grid)$xlim, ylim = st_bbox(grid)$ylim) 

# October-December hotspots
fin <- dum %>%  # arrange columns
  dplyr::mutate(`oct.dec` = case_when((`oct.dec` >= (quantile(dum$oct.dec, c(0.95), na.rm = TRUE) %>% unname())) ~ "High",
                                       (`oct.dec` < (quantile(dum$oct.dec, c(0.95), na.rm = TRUE) %>% unname())) & (`oct.dec` >= (quantile(dum$oct.dec, c(0.85), na.rm = TRUE) %>% unname())) ~ "Mid",
                                       TRUE ~ NA)) %>% 
  sf::st_as_sf()

sum4 <- ggplot() + geom_sf(data = fin, aes(fill = as.factor(oct.dec)), color = NA, size = 0.1) +
  scale_fill_manual(values = c("High" = "#EB4949", "Mid" = "#66A7C4"),
                    na.value = NA) +
  geom_sf(data = landmass, fill = "black", color = NA, size = 0.01) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_blank(),
        legend.text = element_text(size = 20),
        legend.title = element_blank(),
        panel.border = element_blank()) +
  coord_sf(xlim = st_bbox(grid)$xlim, ylim = st_bbox(grid)$ylim) 

# Summary across the different seasons

fin <- dum %>%  # arrange columns
  dplyr::mutate(`jan.mar` = case_when((`jan.mar` >= (quantile(dum$jan.mar, c(0.95), na.rm = TRUE) %>% unname())) ~ 1,
                                      TRUE ~ NA),
                `apr.jun` = case_when((`apr.jun` >= (quantile(dum$apr.jun, c(0.95), na.rm = TRUE) %>% unname())) ~ 1,
                                      TRUE ~ NA),
                `jul.sept` = case_when((`jul.sept` >= (quantile(dum$jul.sept, c(0.95), na.rm = TRUE) %>% unname())) ~ 1,
                                       TRUE ~ NA),
                `oct.dec` = case_when((`oct.dec` >= (quantile(dum$oct.dec, c(0.95), na.rm = TRUE) %>% unname())) ~ 1,
                                      TRUE ~ NA)) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(category = sum(c(jan.mar, apr.jun, jul.sept, oct.dec), na.rm = TRUE)) %>% 
  dplyr::mutate(category = case_when(category == 0 ~ NA,
                                     category == 1 ~ "Unique",
                                     TRUE ~ "Shared")) %>% 
  dplyr::ungroup() %>% 
  sf::st_as_sf(sf_column_name = "geometry")

sum5 <- ggplot() + geom_sf(data = fin, aes(fill = as.factor(category)), color = NA, size = 0.1) +
  scale_fill_manual(values = c("Unique" = "#8A74A6", "Shared" = "#DEA22A"),
    na.value = NA) +
  geom_sf(data = landmass, fill = "black", color = NA, size = 0.01) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_blank(),
        legend.text = element_text(size = 20),
        legend.title = element_blank(),
        panel.border = element_blank()) +
  coord_sf(xlim = st_bbox(grid)$xlim, ylim = st_bbox(grid)$ylim) 


all <- sum1 + sum2 + sum3 + sum4 + sum5 + plot_spacer() +
  plot_layout(ncol = 3, nrow = 2) +
  plot_annotation(tag_level = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = all, filename = here::here(fig_dir, "SummaryHotspots.png"), width = 30, height = 10, dpi = 300)
