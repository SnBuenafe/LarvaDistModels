# DESCRIPTION: Correlation matrices using the PCA loadings

# Load preliminaries
source(file.path("analyses", "02_preliminaries", "00_Preliminaries.R"))
seasons <- c("jan-mar", "apr-jun", "jul-sep", "oct-dec")
PC <- c("PC1", "PC2")
figure_dir <- here::here(figure_dir, "PCA")

#### January-March ####
# PC1
res <- prepare_corrmat_obj("jan-mar", "Comp.1")
res %>% 
  as_tibble(rownames = "species") %>% 
  mutate(case_when(Comp.1 >= 0.6 ~ "High",
                   Comp.1 <= -0.6 ~ "High",
                   Comp.1 < 0.6 & Comp.1 > 0.3 ~ "Medium",
                   Comp.1 > -0.6 & Comp.1 < -0.3 ~ "Medium",
                   TRUE ~ "Low"))
gg <- plotCorrMat(res)
# ggsave(plot = gg, here::here(figure_dir, paste("CorrMat_PC1_jan-mar.png")), height = 4, width = 9, dpi = 600)

# PC2
res <- prepare_corrmat_obj("jan-mar", "Comp.2")
res %>% 
  as_tibble(rownames = "species") %>% 
  mutate(case_when(Comp.2 >= 0.6 ~ "High",
                   Comp.2 <= -0.6 ~ "High",
                   Comp.2 < 0.6 & Comp.2 > 0.3 ~ "Medium",
                   Comp.2 > -0.6 & Comp.2 < -0.3 ~ "Medium",
                   TRUE ~ "Low"))
gg <- plotCorrMat(res)
ggsave(plot = gg, here::here(figure_dir, paste("CorrMat_PC2_jan-mar.png")), height = 4, width = 9, dpi = 600)

#### April-June ####
# PC1
res <- prepare_corrmat_obj("apr-jun", "Comp.1")
res %>% 
  as_tibble(rownames = "species") %>% 
  mutate(case_when(Comp.1 >= 0.6 ~ "High",
                   Comp.1 <= -0.6 ~ "High",
                   Comp.1 < 0.6 & Comp.1 > 0.3 ~ "Medium",
                   Comp.1 > -0.6 & Comp.1 < -0.3 ~ "Medium",
                   TRUE ~ "Low"))
gg <- plotCorrMat(res)
ggsave(plot = gg, here::here(figure_dir, paste("CorrMat_PC1_apr-jun.png")), height = 4, width = 9, dpi = 600)

# PC2
res <- prepare_corrmat_obj("apr-jun", "Comp.2")
res %>% 
  as_tibble(rownames = "species") %>% 
  mutate(case_when(Comp.2 >= 0.6 ~ "High",
                   Comp.2 <= -0.6 ~ "High",
                   Comp.2 < 0.6 & Comp.2 > 0.3 ~ "Medium",
                   Comp.2 > -0.6 & Comp.2 < -0.3 ~ "Medium",
                   TRUE ~ "Low"))
gg <- plotCorrMat(res)
ggsave(plot = gg, here::here(figure_dir, paste("CorrMat_PC2_apr-jun.png")), height = 4, width = 9, dpi = 600)

#### July-September ####
# PC1
res <- prepare_corrmat_obj("jul-sep", "Comp.1")
res %>% 
  as_tibble(rownames = "species") %>% 
  mutate(case_when(Comp.1 >= 0.6 ~ "High",
                   Comp.1 <= -0.6 ~ "High",
                   Comp.1 < 0.6 & Comp.1 > 0.3 ~ "Medium",
                   Comp.1 > -0.6 & Comp.1 < -0.3 ~ "Medium",
                   TRUE ~ "Low"))
gg <- plotCorrMat(res)
ggsave(plot = gg, here::here(figure_dir, paste("CorrMat_PC1_jul-sep.png")), height = 4, width = 9, dpi = 600)

# PC2
res <- prepare_corrmat_obj("jul-sep", "Comp.2")
res %>% 
  as_tibble(rownames = "species") %>% 
  mutate(case_when(Comp.2 >= 0.6 ~ "High",
                   Comp.2 <= -0.6 ~ "High",
                   Comp.2 < 0.6 & Comp.2 > 0.3 ~ "Medium",
                   Comp.2 > -0.6 & Comp.2 < -0.3 ~ "Medium",
                   TRUE ~ "Low"))
gg <- plotCorrMat(res)
ggsave(plot = gg, here::here(figure_dir, paste("CorrMat_PC2_jul-sep.png")), height = 4, width = 9, dpi = 600)

#### October-December ####
# PC1
res <- prepare_corrmat_obj("oct-dec", "Comp.1")
res %>% 
  as_tibble(rownames = "species") %>% 
  mutate(case_when(Comp.1 >= 0.6 ~ "High",
                   Comp.1 <= -0.6 ~ "High",
                   Comp.1 < 0.6 & Comp.1 > 0.3 ~ "Medium",
                   Comp.1 > -0.6 & Comp.1 < -0.3 ~ "Medium",
                   TRUE ~ "Low"))
gg <- plotCorrMat(res)
gg <- plotCorrMat(res)
ggsave(plot = gg, here::here(figure_dir, paste("CorrMat_PC1_oct-dec.png")), height = 4, width = 9, dpi = 600)

# PC2
res <- prepare_corrmat_obj("oct-dec", "Comp.2")
res %>% 
  as_tibble(rownames = "species") %>% 
  mutate(case_when(Comp.2 >= 0.6 ~ "High",
                   Comp.2 <= -0.6 ~ "High",
                   Comp.2 < 0.6 & Comp.2 > 0.3 ~ "Medium",
                   Comp.2 > -0.6 & Comp.2 < -0.3 ~ "Medium",
                   TRUE ~ "Low"))
gg <- plotCorrMat(res)
ggsave(plot = gg, here::here(figure_dir, paste("CorrMat_PC2_oct-dec.png")), height = 4, width = 9, dpi = 600)
